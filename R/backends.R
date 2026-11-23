#' Score a Single Coding Request via an LLM Backend
#'
#' S3 generic invoked by [code_texts()] to obtain a raw JSON string or
#' structured response from an `llm_backend` object.
#'
#' @param backend An object inheriting from `"llm_backend"`.
#' @param request A named list containing `unit_id`, `text`, `prompt_id`,
#'   `prompt_text`, `model`, `temperature`, `replicate`, `schema`, `attempt`,
#'   `previous_errors`, and `meta`.
#' @param ... Additional backend-specific arguments.
#'
#' @return A list with at least `content` (JSON character string or named list)
#'   and optional metadata (`model`, `latency_ms`, `raw_response`).
#' @export
backend_score <- function(backend, request, ...) {
  UseMethod("backend_score")
}

#' Deterministic Synthetic LLM Rater Backend
#'
#' Creates an offline, deterministic mock LLM backend parameterized by an
#' explicit psychometric measurement model. Supports configurable true-score
#' slope/calibration, additive and interaction variance components
#' (prompt, model, unit-by-prompt, unit-by-model, prompt-by-model), temperature
#' noise, uniform and non-uniform Differential Prompt Functioning (DPF),
#' prompt-by-group DPF interactions, temporal/version drift, and optional
#' malformed-JSON injection for testing retry pipelines.
#'
#' Calls are hashed deterministically from `(seed, unit_id, prompt_id, model,
#' temperature, replicate)` without mutating R's global `.Random.seed`.
#'
#' @param seed Integer random seed for deterministic synthetic generation.
#' @param intercept Overall additive bias (calibration intercept shift).
#' @param true_score_slope Calibration slope multiplying the unit true score.
#' @param true_score_col Name of the numeric true-score column in the input
#'   data frame (if present in `request$meta`). If absent, a deterministic
#'   latent true score \eqn{T_i \sim \mathcal{N}(0, 1)} is generated per `unit_id`.
#' @param error_sd Standard deviation of independent measurement error.
#' @param temp_noise_sd Additional error standard deviation multiplied by
#'   `temperature`.
#' @param prompt_sd SD of random prompt main effects (when `prompt_bias` does
#'   not specify a prompt).
#' @param model_sd SD of random model main effects (when `model_bias` does not
#'   specify a model).
#' @param unit_prompt_sd SD of `unit x prompt` interaction variance component.
#' @param unit_model_sd SD of `unit x model` interaction variance component.
#' @param prompt_model_sd SD of `prompt x model` interaction variance component.
#' @param prompt_bias Optional named numeric vector of fixed prompt main effects.
#' @param model_bias Optional named numeric vector of fixed model main effects.
#' @param dpf_uniform Optional named numeric vector of uniform subgroup shifts
#'   keyed by subgroup level (e.g. `c(group_b = 0.75)`).
#' @param dpf_nonuniform Optional named numeric vector of subgroup slope shifts
#'   multiplying the true score (e.g. `c(group_b = -0.35)`).
#' @param dpf_prompt_group Optional numeric matrix (rows = prompt IDs,
#'   cols = subgroup levels) specifying prompt-by-group DPF interactions.
#' @param group_col Column name in input data identifying subgroup membership.
#' @param drift_per_wave Numeric mean drift per unit increase in `wave_col`.
#' @param version_shift Optional named numeric vector of fixed shifts keyed by
#'   `wave_col` (or version label).
#' @param wave_col Column name in input data identifying temporal wave or model
#'   version.
#' @param malformed_rate Probability in `[0, 1]` of returning malformed JSON on
#'   attempt 1 (useful for testing schema validation and retry behavior).
#' @param score_field Name of the primary numeric/factor field in the schema.
#'   Defaults to the first field in the schema.
#'
#' @return An object of class `c("llm_backend_mock", "llm_backend")`.
#' @export
#'
#' @examples
#' b <- backend_mock(seed = 42, error_sd = 0.4, prompt_sd = 0.3, model_sd = 0.2)
#' b
backend_mock <- function(seed = 1234L,
                         intercept = 0,
                         true_score_slope = 1,
                         true_score_col = "true_score",
                         error_sd = 0.5,
                         temp_noise_sd = 0.5,
                         prompt_sd = 0.25,
                         model_sd = 0.20,
                         unit_prompt_sd = 0.25,
                         unit_model_sd = 0.20,
                         prompt_model_sd = 0.15,
                         prompt_bias = NULL,
                         model_bias = NULL,
                         dpf_uniform = NULL,
                         dpf_nonuniform = NULL,
                         dpf_prompt_group = NULL,
                         group_col = "group",
                         drift_per_wave = 0,
                         version_shift = NULL,
                         wave_col = "wave",
                         malformed_rate = 0,
                         score_field = NULL) {
  assert_scalar_numeric(seed, "seed")
  assert_scalar_numeric(intercept, "intercept")
  assert_scalar_numeric(true_score_slope, "true_score_slope")
  assert_scalar_numeric(error_sd, "error_sd", min = 0)
  assert_scalar_numeric(temp_noise_sd, "temp_noise_sd", min = 0)
  assert_scalar_numeric(prompt_sd, "prompt_sd", min = 0)
  assert_scalar_numeric(model_sd, "model_sd", min = 0)
  assert_scalar_numeric(unit_prompt_sd, "unit_prompt_sd", min = 0)
  assert_scalar_numeric(unit_model_sd, "unit_model_sd", min = 0)
  assert_scalar_numeric(prompt_model_sd, "prompt_model_sd", min = 0)
  assert_scalar_numeric(drift_per_wave, "drift_per_wave")
  assert_scalar_numeric(malformed_rate, "malformed_rate", min = 0, max = 1)
  assert_scalar_character(true_score_col, "true_score_col", allow_null = TRUE)
  assert_scalar_character(group_col, "group_col", allow_null = TRUE)
  assert_scalar_character(wave_col, "wave_col", allow_null = TRUE)
  assert_scalar_character(score_field, "score_field", allow_null = TRUE)

  params <- list(
    seed = as.integer(seed),
    intercept = intercept,
    true_score_slope = true_score_slope,
    true_score_col = true_score_col,
    error_sd = error_sd,
    temp_noise_sd = temp_noise_sd,
    prompt_sd = prompt_sd,
    model_sd = model_sd,
    unit_prompt_sd = unit_prompt_sd,
    unit_model_sd = unit_model_sd,
    prompt_model_sd = prompt_model_sd,
    prompt_bias = prompt_bias,
    model_bias = model_bias,
    dpf_uniform = dpf_uniform,
    dpf_nonuniform = dpf_nonuniform,
    dpf_prompt_group = dpf_prompt_group,
    group_col = group_col,
    drift_per_wave = drift_per_wave,
    version_shift = version_shift,
    wave_col = wave_col,
    malformed_rate = malformed_rate,
    score_field = score_field
  )

  structure(
    c(
      params,
      list(
        backend_type = "mock",
        id = paste0("mock_", substr(digest::digest(params, algo = "sha256"), 1L, 12L))
      )
    ),
    class = c("llm_backend_mock", "llm_backend")
  )
}

#' Helper: deterministic normal draw for a named effect tuple
#' @noRd
mock_effect_draw <- function(seed, tag, ..., sd = 1) {
  if (sd <= 0) {
    return(0)
  }
  s <- hash_to_seed(seed, tag, ...)
  with_local_seed(s, stats::rnorm(1L, mean = 0, sd = sd))
}

#' @export
backend_score.llm_backend_mock <- function(backend, request, ...) {
  t0 <- proc.time()[["elapsed"]]
  seed <- backend$seed
  uid <- as.character(request$unit_id)
  pid <- as.character(request$prompt_id)
  mid <- as.character(request$model)
  temp <- as.numeric(request$temperature %||% 0)
  rep_id <- as.integer(request$replicate %||% 1L)
  attempt <- as.integer(request$attempt %||% 1L)
  meta <- request$meta %||% list()
  schema <- request$schema

  # Optional malformed output injection on attempt 1 to test retries
  if (backend$malformed_rate > 0 && attempt == 1L) {
    u_mal <- with_local_seed(
      hash_to_seed(seed, "malformed", uid, pid, mid, temp, rep_id),
      stats::runif(1L)
    )
    if (u_mal < backend$malformed_rate) {
      return(list(
        content = "{malformed json output",
        model = mid,
        latency_ms = (proc.time()[["elapsed"]] - t0) * 1000,
        raw_response = NULL
      ))
    }
  }

  # Unit true score
  true_score <- NULL
  if (!is.null(backend$true_score_col) &&
      backend$true_score_col %in% names(meta) &&
      !is.na(meta[[backend$true_score_col]])) {
    true_score <- as.numeric(meta[[backend$true_score_col]])
  } else {
    true_score <- mock_effect_draw(seed, "unit_true", uid, sd = 1)
  }

  # Facet main effects
  b_prompt <- if (!is.null(backend$prompt_bias) && pid %in% names(backend$prompt_bias)) {
    as.numeric(backend$prompt_bias[[pid]])
  } else {
    mock_effect_draw(seed, "prompt", pid, sd = backend$prompt_sd)
  }

  b_model <- if (!is.null(backend$model_bias) && mid %in% names(backend$model_bias)) {
    as.numeric(backend$model_bias[[mid]])
  } else {
    mock_effect_draw(seed, "model", mid, sd = backend$model_sd)
  }

  # Facet interaction effects
  u_up <- mock_effect_draw(seed, "unit_prompt", uid, pid, sd = backend$unit_prompt_sd)
  u_um <- mock_effect_draw(seed, "unit_model", uid, mid, sd = backend$unit_model_sd)
  b_pm <- mock_effect_draw(seed, "prompt_model", pid, mid, sd = backend$prompt_model_sd)

  # Subgroup DPF effects
  dpf_shift <- 0
  if (!is.null(backend$group_col) && backend$group_col %in% names(meta) && !is.na(meta[[backend$group_col]])) {
    grp <- as.character(meta[[backend$group_col]])
    if (!is.null(backend$dpf_uniform) && grp %in% names(backend$dpf_uniform)) {
      dpf_shift <- dpf_shift + as.numeric(backend$dpf_uniform[[grp]])
    }
    if (!is.null(backend$dpf_nonuniform) && grp %in% names(backend$dpf_nonuniform)) {
      dpf_shift <- dpf_shift + as.numeric(backend$dpf_nonuniform[[grp]]) * true_score
    }
    if (!is.null(backend$dpf_prompt_group) &&
        is.matrix(backend$dpf_prompt_group) &&
        pid %in% rownames(backend$dpf_prompt_group) &&
        grp %in% colnames(backend$dpf_prompt_group)) {
      dpf_shift <- dpf_shift + as.numeric(backend$dpf_prompt_group[pid, grp])
    }
  }

  # Temporal / version drift
  drift_shift <- 0
  if (!is.null(backend$wave_col) && backend$wave_col %in% names(meta) && !is.na(meta[[backend$wave_col]])) {
    wv <- meta[[backend$wave_col]]
    wv_chr <- as.character(wv)
    if (!is.null(backend$version_shift) && wv_chr %in% names(backend$version_shift)) {
      drift_shift <- drift_shift + as.numeric(backend$version_shift[[wv_chr]])
    }
    if (backend$drift_per_wave != 0 && is.numeric(wv)) {
      drift_shift <- drift_shift + backend$drift_per_wave * as.numeric(wv)
    }
  }

  # Residual + temperature noise
  eps_base <- mock_effect_draw(seed, "resid", uid, pid, mid, rep_id, sd = backend$error_sd)
  eps_temp <- if (temp > 0 && backend$temp_noise_sd > 0) {
    mock_effect_draw(seed, "temp_noise", uid, pid, mid, temp, rep_id, sd = backend$temp_noise_sd * temp)
  } else {
    0
  }

  latent <- backend$intercept +
    backend$true_score_slope * true_score +
    b_prompt + b_model + u_up + u_um + b_pm +
    dpf_shift + drift_shift + eps_base + eps_temp

  # Populate all fields defined in schema
  payload <- list()
  primary <- backend$score_field %||% names(schema$fields)[1L]

  for (nm in names(schema$fields)) {
    f <- schema$fields[[nm]]
    val_latent <- if (identical(nm, primary)) {
      latent
    } else {
      latent + mock_effect_draw(seed, paste0("aux_", nm), uid, pid, mid, rep_id, sd = 0.25)
    }

    if (f$type == "numeric") {
      v <- round(val_latent, 4L)
      if (!is.null(f$min)) v <- max(v, f$min)
      if (!is.null(f$max)) v <- min(v, f$max)
      payload[[nm]] <- v
    } else if (f$type == "integer") {
      v <- as.integer(round(val_latent))
      if (!is.null(f$min)) v <- max(v, as.integer(f$min))
      if (!is.null(f$max)) v <- min(v, as.integer(f$max))
      payload[[nm]] <- v
    } else if (f$type == "factor") {
      k <- length(f$levels)
      probs <- stats::pnorm(val_latent)
      idx <- min(k, max(1L, as.integer(floor(probs * k)) + 1L))
      payload[[nm]] <- f$levels[idx]
    } else if (f$type == "logical") {
      payload[[nm]] <- isTRUE(val_latent > 0)
    } else if (f$type == "character") {
      payload[[nm]] <- paste0("Synthetic rationale for ", uid, " (", pid, "/", mid, ")")
    }
  }

  json_str <- jsonlite::toJSON(payload, auto_unbox = TRUE)

  list(
    content = as.character(json_str),
    model = mid,
    latency_ms = (proc.time()[["elapsed"]] - t0) * 1000,
    raw_response = payload
  )
}

#' OpenAI-Compatible HTTP Coding Backend
#'
#' Creates a hardened backend targeting any OpenAI-compatible
#' `/chat/completions` API (OpenAI, Azure OpenAI proxies, vLLM, OpenRouter,
#' Together, LiteLLM, or local servers) using `httr2` with strict URL/header
#' validation, sealed API-key redaction, bounded response sizes, and jittered
#' exponential backoff retries.
#'
#' @param model Default model identifier (overridden by design model column
#'   when `code_texts()` runs multi-model designs).
#' @param api_key API key string (wrapped in a non-printable redacted container;
#'   never echoed in `print()`, `summary()`, cache keys, or errors). Defaults to
#'   `Sys.getenv("OPENAI_API_KEY")`.
#' @param base_url Base HTTP/HTTPS API URL without trailing slash.
#' @param system_prompt Optional system instruction prepended to every request.
#' @param response_format Structured output mode: `"json_schema"` (default),
#'   `"json_object"`, or `"none"`.
#' @param timeout Request timeout in seconds. Default `60`.
#' @param max_retries Maximum HTTP retry attempts on transient errors.
#' @param backoff_base Base backoff seconds for jittered exponential retry.
#' @param max_response_bytes Maximum allowed response payload size in bytes.
#'   Default `1048576L` (1 MiB).
#' @param extra_headers Optional named list of additional HTTP headers (checked
#'   against CRLF/control-character injection).
#' @param extra_body Optional named list merged into the JSON request body.
#' @param transport Optional custom transport function `function(req_spec)`
#'   returning a parsed list response (intended for offline testing/mocking).
#'
#' @return An object of class `c("llm_backend_openai", "llm_backend")`.
#' @export
#'
#' @examples
#' b <- backend_openai(model = "gpt-4o-mini", api_key = "demo-key")
#' b
backend_openai <- function(model = "gpt-4o-mini",
                           api_key = Sys.getenv("OPENAI_API_KEY"),
                           base_url = "https://api.openai.com/v1",
                           system_prompt = "You are a precise research coder. Return only valid JSON matching the requested schema.",
                           response_format = c("json_schema", "json_object", "none"),
                           timeout = 60,
                           max_retries = 3L,
                           backoff_base = 1,
                           max_response_bytes = 1048576L,
                           extra_headers = list(),
                           extra_body = list(),
                           transport = NULL) {
  assert_scalar_character(model, "model")
  base_url <- validate_endpoint_url(base_url, arg = "base_url")
  assert_scalar_character(system_prompt, "system_prompt", allow_null = TRUE)
  response_format <- match.arg(response_format)
  assert_scalar_numeric(timeout, "timeout", min = 1, max = 3600)
  assert_scalar_numeric(max_retries, "max_retries", min = 0, max = 20)
  assert_scalar_numeric(backoff_base, "backoff_base", min = 0, max = 60)
  assert_scalar_numeric(max_response_bytes, "max_response_bytes", min = 1024)
  extra_headers <- validate_http_headers(extra_headers, arg = "extra_headers")
  if (!is.null(transport) && !is.function(transport)) {
    cli::cli_abort("{.arg transport} must be a function or NULL.")
  }

  secret_box <- new_redacted_secret(api_key)
  id_hash <- digest::digest(
    list("openai", base_url, system_prompt, response_format, extra_body),
    algo = "sha256"
  )

  structure(
    list(
      backend_type = "openai",
      id = paste0("openai_", substr(id_hash, 1L, 12L)),
      model = model,
      api_key = secret_box,
      base_url = base_url,
      system_prompt = system_prompt,
      response_format = response_format,
      timeout = timeout,
      max_retries = as.integer(max_retries),
      backoff_base = backoff_base,
      max_response_bytes = as.integer(round(max_response_bytes)),
      extra_headers = extra_headers,
      extra_body = extra_body,
      transport = transport
    ),
    class = c("llm_backend_openai", "llm_backend")
  )
}

#' Build an OpenAI-compatible request specification
#' @noRd
build_openai_request_spec <- function(backend, request) {
  model_name <- request$model %||% backend$model
  messages <- list()
  if (!is.null(backend$system_prompt)) {
    messages[[length(messages) + 1L]] <- list(role = "system", content = backend$system_prompt)
  }

  user_content <- request$prompt_text
  if (!is.null(request$previous_errors) && length(request$previous_errors) > 0L) {
    user_content <- paste0(
      user_content,
      "\n\nYour previous response failed JSON schema validation with error(s): ",
      paste(request$previous_errors, collapse = "; "),
      ". Return ONLY valid JSON matching the schema."
    )
  }
  messages[[length(messages) + 1L]] <- list(role = "user", content = user_content)

  body <- list(
    model = model_name,
    messages = messages,
    temperature = as.numeric(request$temperature %||% 0)
  )

  if (backend$response_format == "json_schema" && !is.null(request$schema)) {
    body$response_format <- list(
      type = "json_schema",
      json_schema = list(
        name = request$schema$name,
        strict = isTRUE(request$schema$strict),
        schema = as_json_schema(request$schema)
      )
    )
  } else if (backend$response_format == "json_object") {
    body$response_format <- list(type = "json_object")
  }

  if (length(backend$extra_body) > 0L) {
    body <- utils::modifyList(body, backend$extra_body)
  }

  secret_val <- reveal_secret(backend$api_key)
  headers <- c(
    list(
      Authorization = paste("Bearer", secret_val),
      `Content-Type` = "application/json"
    ),
    backend$extra_headers
  )

  list(
    url = paste0(backend$base_url, "/chat/completions"),
    headers = headers,
    body = body,
    timeout = backend$timeout,
    max_retries = backend$max_retries,
    backoff_base = backend$backoff_base,
    max_response_bytes = backend$max_response_bytes %||% 1048576L
  )
}

#' @export
backend_score.llm_backend_openai <- function(backend, request, ...) {
  t0 <- proc.time()[["elapsed"]]
  spec <- build_openai_request_spec(backend, request)
  secret_val <- reveal_secret(backend$api_key)

  if (!is.null(backend$transport)) {
    parsed <- tryCatch(
      backend$transport(spec),
      error = function(e) {
        cli::cli_abort(
          paste0("OpenAI transport error: ", redact_secrets(conditionMessage(e), secrets = secret_val))
        )
      }
    )
  } else {
    if (!nzchar(secret_val)) {
      cli::cli_abort(
        "OpenAI API key is empty. Supply {.arg api_key} or set the {.envvar OPENAI_API_KEY} environment variable."
      )
    }
    req <- httr2::request(spec$url)
    req <- do.call(httr2::req_headers, c(list(.req = req), spec$headers))
    req <- httr2::req_body_json(req, spec$body, auto_unbox = TRUE)
    req <- httr2::req_timeout(req, spec$timeout)
    if (spec$max_retries > 0L) {
      req <- httr2::req_retry(
        req,
        max_tries = spec$max_retries + 1L,
        backoff = function(attempt) jittered_backoff(attempt, base = spec$backoff_base)
      )
    }
    resp <- tryCatch(
      httr2::req_perform(req),
      error = function(e) {
        cli::cli_abort(
          paste0("OpenAI request failed: ", redact_secrets(conditionMessage(e), secrets = secret_val))
        )
      }
    )
    raw_body <- httr2::resp_body_raw(resp)
    check_response_size(raw_body, max_bytes = spec$max_response_bytes)
    parsed <- jsonlite::fromJSON(rawToChar(raw_body), simplifyVector = FALSE)
  }

  content <- NULL
  if (is.list(parsed) && !is.null(parsed$choices) && length(parsed$choices) >= 1L) {
    content <- parsed$choices[[1L]]$message$content
  } else if (is.list(parsed) && !is.null(parsed$content)) {
    content <- parsed$content
  }
  if (is.null(content)) {
    cli::cli_abort("OpenAI backend response did not contain {.field choices[[1]]$message$content}.")
  }
  if (is.character(content)) {
    check_response_size(content, max_bytes = spec$max_response_bytes)
  }

  list(
    content = content,
    model = parsed$model %||% spec$body$model,
    latency_ms = (proc.time()[["elapsed"]] - t0) * 1000,
    raw_response = parsed
  )
}

#' Local Ollama HTTP Coding Backend
#'
#' Creates a hardened backend targeting a local or remote Ollama server
#' (`/api/chat`) via `httr2` with strict URL validation, bounded response
#' sizes, structured JSON/schema constraints, and jittered backoff retries.
#'
#' @param model Default Ollama model tag (e.g., `"llama3.2"`, `"qwen2.5:7b"`).
#' @param base_url Ollama HTTP/HTTPS base URL without trailing slash.
#' @param system_prompt Optional system instruction prepended to messages.
#' @param format Structured output constraint: `"schema"` (sends full JSON
#'   Schema), `"json"` (sends `"json"`), or `"none"`.
#' @param timeout Request timeout in seconds. Default `120`.
#' @param max_retries Maximum HTTP retry attempts on transient errors.
#' @param backoff_base Base backoff seconds for jittered exponential retry.
#' @param max_response_bytes Maximum allowed response payload size in bytes.
#'   Default `1048576L` (1 MiB).
#' @param options Named list of additional Ollama model options (e.g., `seed`,
#'   `num_ctx`).
#' @param transport Optional custom transport function `function(req_spec)`
#'   for offline testing.
#'
#' @return An object of class `c("llm_backend_ollama", "llm_backend")`.
#' @export
#'
#' @examples
#' b <- backend_ollama(model = "llama3.2")
#' b
backend_ollama <- function(model = "llama3.2",
                           base_url = "http://localhost:11434",
                           system_prompt = "You are a precise research coder. Return only valid JSON matching the requested schema.",
                           format = c("schema", "json", "none"),
                           timeout = 120,
                           max_retries = 2L,
                           backoff_base = 1,
                           max_response_bytes = 1048576L,
                           options = list(),
                           transport = NULL) {
  assert_scalar_character(model, "model")
  base_url <- validate_endpoint_url(base_url, arg = "base_url")
  assert_scalar_character(system_prompt, "system_prompt", allow_null = TRUE)
  format <- match.arg(format)
  assert_scalar_numeric(timeout, "timeout", min = 1, max = 3600)
  assert_scalar_numeric(max_retries, "max_retries", min = 0, max = 20)
  assert_scalar_numeric(backoff_base, "backoff_base", min = 0, max = 60)
  assert_scalar_numeric(max_response_bytes, "max_response_bytes", min = 1024)
  if (!is.null(transport) && !is.function(transport)) {
    cli::cli_abort("{.arg transport} must be a function or NULL.")
  }

  id_hash <- digest::digest(
    list("ollama", base_url, system_prompt, format, options),
    algo = "sha256"
  )

  structure(
    list(
      backend_type = "ollama",
      id = paste0("ollama_", substr(id_hash, 1L, 12L)),
      model = model,
      base_url = base_url,
      system_prompt = system_prompt,
      format = format,
      timeout = timeout,
      max_retries = as.integer(max_retries),
      backoff_base = backoff_base,
      max_response_bytes = as.integer(round(max_response_bytes)),
      options = options,
      transport = transport
    ),
    class = c("llm_backend_ollama", "llm_backend")
  )
}

#' Build an Ollama `/api/chat` request specification
#' @noRd
build_ollama_request_spec <- function(backend, request) {
  model_name <- request$model %||% backend$model
  messages <- list()
  if (!is.null(backend$system_prompt)) {
    messages[[length(messages) + 1L]] <- list(role = "system", content = backend$system_prompt)
  }

  user_content <- request$prompt_text
  if (!is.null(request$previous_errors) && length(request$previous_errors) > 0L) {
    user_content <- paste0(
      user_content,
      "\n\nYour previous response failed JSON schema validation: ",
      paste(request$previous_errors, collapse = "; "),
      ". Return ONLY valid JSON matching the schema."
    )
  }
  messages[[length(messages) + 1L]] <- list(role = "user", content = user_content)

  opts <- utils::modifyList(
    backend$options,
    list(temperature = as.numeric(request$temperature %||% 0))
  )

  body <- list(
    model = model_name,
    messages = messages,
    stream = FALSE,
    options = opts
  )

  if (backend$format == "schema" && !is.null(request$schema)) {
    body$format <- as_json_schema(request$schema)
  } else if (backend$format == "json") {
    body$format <- "json"
  }

  list(
    url = paste0(backend$base_url, "/api/chat"),
    body = body,
    timeout = backend$timeout,
    max_retries = backend$max_retries,
    backoff_base = backend$backoff_base %||% 1,
    max_response_bytes = backend$max_response_bytes %||% 1048576L
  )
}

#' @export
backend_score.llm_backend_ollama <- function(backend, request, ...) {
  t0 <- proc.time()[["elapsed"]]
  spec <- build_ollama_request_spec(backend, request)

  if (!is.null(backend$transport)) {
    parsed <- backend$transport(spec)
  } else {
    req <- httr2::request(spec$url)
    req <- httr2::req_body_json(req, spec$body, auto_unbox = TRUE)
    req <- httr2::req_timeout(req, spec$timeout)
    if (spec$max_retries > 0L) {
      req <- httr2::req_retry(
        req,
        max_tries = spec$max_retries + 1L,
        backoff = function(attempt) jittered_backoff(attempt, base = spec$backoff_base)
      )
    }
    resp <- httr2::req_perform(req)
    raw_body <- httr2::resp_body_raw(resp)
    check_response_size(raw_body, max_bytes = spec$max_response_bytes)
    parsed <- jsonlite::fromJSON(rawToChar(raw_body), simplifyVector = FALSE)
  }

  content <- NULL
  if (is.list(parsed) && !is.null(parsed$message$content)) {
    content <- parsed$message$content
  } else if (is.list(parsed) && !is.null(parsed$response)) {
    content <- parsed$response
  }
  if (is.null(content)) {
    cli::cli_abort("Ollama backend response did not contain {.field message$content}.")
  }
  if (is.character(content)) {
    check_response_size(content, max_bytes = spec$max_response_bytes)
  }

  list(
    content = content,
    model = parsed$model %||% spec$body$model,
    latency_ms = (proc.time()[["elapsed"]] - t0) * 1000,
    raw_response = parsed
  )
}

#' @export
print.llm_backend <- function(x, ...) {
  cli::cli_h3("LLM Backend ({x$backend_type})")
  cli::cli_text("ID: {x$id}")
  if (inherits(x, "llm_backend_mock")) {
    cli::cli_text(
      "Seed: {x$seed} | Slope: {x$true_score_slope} | Intercept: {x$intercept} | Error SD: {x$error_sd}"
    )
    cli::cli_text(
      "Facet SDs: prompt={x$prompt_sd}, model={x$model_sd}, u:p={x$unit_prompt_sd}, u:m={x$unit_model_sd}, p:m={x$prompt_model_sd}"
    )
  } else if (inherits(x, "llm_backend_openai")) {
    cli::cli_text(
      "Model: {x$model} | Endpoint: {x$base_url} | API key: {format(x$api_key)} | Timeout: {x$timeout}s"
    )
  } else {
    cli::cli_text("Model: {x$model} | Endpoint: {x$base_url} | Timeout: {x$timeout}s")
  }
  invisible(x)
}

#' @export
summary.llm_backend <- function(object, ...) {
  print(object, ...)
  invisible(object)
}
