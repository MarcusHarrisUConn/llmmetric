test_that("backend_mock is deterministic and does not mutate global RNG state", {
  set.seed(777)
  seed_before <- .Random.seed
  schema <- coding_schema(
    score = schema_field("numeric", min = -5, max = 10),
    cat = schema_field("factor", levels = c("low", "mid", "high")),
    flag = "logical",
    rationale = "character"
  )
  des <- coding_design(
    prompts = c(p1 = "Rate: {text}", p2 = "Judge: {text}"),
    models = c("m1", "m2"),
    temperatures = c(0, 0.7),
    schema = schema
  )
  df <- tibble::tibble(
    id = paste0("u", 1:6),
    text = paste("Diary entry", 1:6),
    true_score = seq(-1, 1, length.out = 6),
    group = rep(c("a", "b"), 3)
  )
  be <- backend_mock(seed = 42, dpf_uniform = c(b = 0.8), temp_noise_sd = 0.4)

  c1 <- code_texts(df, "text", id_col = "id", design = des, backend = be, cache = coding_cache("none"))
  seed_after <- .Random.seed
  expect_identical(seed_before, seed_after)

  c2 <- code_texts(df, "text", id_col = "id", design = des, backend = be, cache = coding_cache("none"))
  expect_equal(c1$scores$score, c2$scores$score)
  expect_equal(c1$scores$cat, c2$scores$cat)
  expect_equal(nrow(c1$scores), 6 * 2 * 2 * 2)
  expect_true(all(c1$scores$valid))
})

test_that("code_texts caches responses, retries malformed outputs, and supports per-model backends", {
  df <- tibble::tibble(
    id = paste0("u", 1:5),
    text = paste("Text", 1:5),
    true_score = rnorm(5),
    cov_x = 1:5
  )
  des <- coding_design(
    prompts = c(p1 = "P1: {text}", p2 = "P2: {text}"),
    models = c("fast", "strong"),
    schema = coding_schema(score = schema_field("numeric"))
  )

  # First run populates cache; malformed_rate = 1 forces retry on attempt 1 -> succeeds on attempt 2
  tmp_dir <- withr::local_tempdir()
  disk_cache <- coding_cache("disk", dir = tmp_dir)
  backends <- list(
    fast = backend_mock(seed = 10, intercept = 0.5, malformed_rate = 1),
    strong = backend_mock(seed = 10, intercept = 0.0, malformed_rate = 0)
  )

  res1 <- code_texts(df, "text", id_col = "id", design = des, backend = backends,
                     cache = disk_cache, max_validation_retries = 2L)
  expect_equal(nrow(res1$scores), 20L)
  expect_true(all(res1$scores$valid))
  expect_equal(nrow(res1$failures), 0L)
  expect_true(all(res1$scores$attempts[res1$scores$model == "fast"] == 2L))
  expect_true(all(res1$scores$attempts[res1$scores$model == "strong"] == 1L))
  expect_true(all(!res1$scores$cache_hit))

  # Second run hits cache 100%
  res2 <- code_texts(df, "text", id_col = "id", design = des, backend = backends,
                     cache = disk_cache, max_validation_retries = 2L)
  expect_true(all(res2$scores$cache_hit))
  expect_equal(res1$scores$score, res2$scores$score)

  # With 0 retries and malformed_rate = 1, failures are cleanly recorded with a warning
  expect_warning(
    res_fail <- code_texts(
      df[1:2, ], "text", id_col = "id",
      design = coding_design(prompts = c(p1 = "{text}"), models = "fast"),
      backend = backend_mock(seed = 1, malformed_rate = 1),
      cache = coding_cache("none"),
      max_validation_retries = 0L
    ),
    "coding cell"
  )
  expect_equal(nrow(res_fail$failures), 2L)
  expect_equal(res_fail$failed_indices, c(1L, 2L))
  expect_true(all(!res_fail$scores$valid))

  # aggregate_scores collapses to unit level and retains unit-constant cov_x
  agg <- aggregate_scores(res1)
  expect_equal(nrow(agg), 5L)
  expect_equal(agg$n_ratings, rep(4L, 5L))
  expect_true("cov_x" %in% names(agg))
  expect_equal(agg$cov_x, 1:5)
})

test_that("backend_openai and backend_ollama build valid HTTP specs and parse responses offline", {
  captured_openai <- NULL
  be_oa <- backend_openai(
    model = "gpt-4o-mini",
    api_key = "test-secret",
    base_url = "https://example.test/v1/",
    response_format = "json_schema",
    transport = function(spec) {
      captured_openai <<- spec
      list(
        model = spec$body$model,
        choices = list(list(message = list(content = '{"score": 5.25}')))
      )
    }
  )

  captured_ol <- NULL
  be_ol <- backend_ollama(
    model = "llama3.2",
    base_url = "http://localhost:11434/",
    format = "schema",
    transport = function(spec) {
      captured_ol <<- spec
      list(
        model = spec$body$model,
        message = list(content = '{"score": 4.75}')
      )
    }
  )

  df <- tibble::tibble(id = "u1", text = "I feel stressed today.")
  des <- coding_design(
    prompts = c(p1 = "Rate stress: {text}"),
    models = c("gpt-4o-mini", "llama3.2"),
    temperatures = 0.2,
    schema = coding_schema(score = schema_field("numeric", min = 1, max = 7))
  )

  coded <- code_texts(
    df, "text", id_col = "id", design = des,
    backend = list(`gpt-4o-mini` = be_oa, llama3.2 = be_ol),
    cache = coding_cache("none")
  )

  expect_true(all(coded$scores$valid))
  expect_equal(coded$scores$score[coded$scores$model == "gpt-4o-mini"], 5.25)
  expect_equal(coded$scores$score[coded$scores$model == "llama3.2"], 4.75)

  expect_equal(captured_openai$url, "https://example.test/v1/chat/completions")
  expect_equal(captured_openai$headers$Authorization, "Bearer test-secret")
  expect_equal(captured_openai$body$response_format$type, "json_schema")
  expect_equal(captured_openai$body$temperature, 0.2)

  expect_equal(captured_ol$url, "http://localhost:11434/api/chat")
  expect_false(captured_ol$body$stream)
  expect_equal(captured_ol$body$options$temperature, 0.2)
  expect_equal(captured_ol$body$format$type, "object")

  # Missing API key fails fast when no transport mock is supplied
  be_nokey <- backend_openai(api_key = "")
  expect_error(
    backend_score(be_nokey, list(prompt_text = "hi", model = "gpt-4o-mini", temperature = 0)),
    "API key is empty"
  )
})
