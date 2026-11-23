test_that("coding_schema and schema_field validate definitions and export JSON Schema", {
  s <- coding_schema(
    score = schema_field("numeric", min = 1, max = 7, description = "Distress score"),
    rank = schema_field("integer", min = 1, max = 5),
    sentiment = schema_field("enum", levels = c("neg", "neu", "pos")),
    flagged = "logical",
    notes = schema_field("character", required = FALSE, nullable = TRUE)
  )

  expect_s3_class(s, "llm_coding_schema")
  expect_equal(s$fields$sentiment$type, "factor")
  expect_true(nzchar(s$hash))

  js <- as_json_schema(s)
  expect_equal(js$type, "object")
  expect_equal(js$properties$score$minimum, 1)
  expect_equal(js$properties$score$maximum, 7)
  expect_equal(js$properties$rank$type, "integer")
  expect_equal(js$properties$notes$type, c("string", "null"))
  expect_false("notes" %in% js$required)
  expect_false(js$additionalProperties)

  expect_error(schema_field("numeric", min = 5, max = 2))
  expect_error(schema_field("factor", levels = "only_one"))
  expect_error(coding_schema())
})

test_that("validate_coded_output enforces types, bounds, enums, fences, strictness, and coercion", {
  s <- coding_schema(
    score = schema_field("numeric", min = 1, max = 7),
    count = schema_field("integer", min = 0, max = 10),
    label = schema_field("factor", levels = c("low", "high")),
    ok = schema_field("logical"),
    note = schema_field("character", required = FALSE, nullable = TRUE)
  )

  # Valid JSON wrapped in markdown code fence
  raw_ok <- "```json\n{\"score\": 4.5, \"count\": 3, \"label\": \"high\", \"ok\": true}\n```"
  v1 <- validate_coded_output(raw_ok, s)
  expect_true(v1$valid)
  expect_equal(v1$data$score, 4.5)
  expect_identical(v1$data$count, 3L)
  expect_equal(as.character(v1$data$label), "high")
  expect_true(v1$data$ok)
  expect_true(is.na(v1$data$note))

  # Out-of-bounds score
  v_oob <- validate_coded_output('{"score": 9.1, "count": 2, "label": "low", "ok": false}', s)
  expect_false(v_oob$valid)
  expect_match(paste(v_oob$errors, collapse = " "), "exceeds max")

  # Non-integer in integer field
  v_nonint <- validate_coded_output('{"score": 3, "count": 2.4, "label": "low", "ok": false}', s)
  expect_false(v_nonint$valid)
  expect_match(paste(v_nonint$errors, collapse = " "), "not a valid finite integer")

  # Invalid factor level
  v_lvl <- validate_coded_output('{"score": 3, "count": 2, "label": "medium", "ok": true}', s)
  expect_false(v_lvl$valid)
  expect_match(paste(v_lvl$errors, collapse = " "), "allowed levels")

  # Unexpected extra key under strict = TRUE
  v_extra <- validate_coded_output('{"score": 3, "count": 2, "label": "low", "ok": true, "extra": 1}', s)
  expect_false(v_extra$valid)
  expect_match(paste(v_extra$errors, collapse = " "), "Unexpected")

  # Coercion mode
  v_strict_str <- validate_coded_output('{"score": "4.2", "count": "2", "label": "low", "ok": "true"}', s, coerce = FALSE)
  expect_false(v_strict_str$valid)
  v_coerced <- validate_coded_output('{"score": "4.2", "count": "2", "label": "low", "ok": "true"}', s, coerce = TRUE)
  expect_true(v_coerced$valid)
  expect_equal(v_coerced$data$score, 4.2)
  expect_identical(v_coerced$data$count, 2L)
  expect_true(v_coerced$data$ok)

  # Malformed JSON
  v_bad <- validate_coded_output("{not json", s)
  expect_false(v_bad$valid)
  expect_match(v_bad$errors, "JSON parse error")
})

test_that("coding_cache works in memory, disk, and none modes", {
  # Memory cache
  mc <- coding_cache("memory")
  k1 <- cache_key("u1", "p1", "m1", 0)
  expect_null(cache_get(mc, k1))
  expect_equal(cache_stats(mc)$misses, 1L)

  cache_set(mc, k1, list(score = 5.5))
  expect_equal(cache_get(mc, k1)$score, 5.5)
  expect_equal(cache_stats(mc)$hits, 1L)
  expect_equal(cache_stats(mc)$entries, 1L)

  cache_clear(mc)
  expect_equal(cache_stats(mc)$entries, 0L)
  expect_null(cache_get(mc, k1))

  # Disk cache inside temporary directory
  tmp_dir <- withr::local_tempdir()
  dc1 <- coding_cache("disk", dir = tmp_dir)
  cache_set(dc1, k1, list(score = 6.25))
  expect_equal(cache_stats(dc1)$entries, 1L)

  # Separate cache instance reading same disk dir gets immediate hit
  dc2 <- coding_cache("disk", dir = tmp_dir)
  got <- cache_get(dc2, k1)
  expect_equal(got$score, 6.25)
  expect_equal(cache_stats(dc2)$hits, 1L)
  cache_clear(dc2)
  expect_equal(cache_stats(dc2)$entries, 0L)

  # None mode
  nc <- coding_cache("none")
  expect_false(cache_set(nc, k1, list(score = 1)))
  expect_null(cache_get(nc, k1))
})
