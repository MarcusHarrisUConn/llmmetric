#' Specify a Multi-Facet LLM Coding Design
#'
#' Defines a crossed experimental coding design (`prompts x models x
#' temperatures x replicates`) paired with a strict [coding_schema()] for
#' reproducible LLM measurement studies.
#'
#' @param prompts Named (or unnamed) character vector of prompt templates.
#'   Templates may reference `{text}` and any additional column names in the
#'   input data frame passed to [code_texts()].
#' @param models Character vector of model identifiers to cross with prompts.
#' @param temperatures Numeric vector of sampling temperatures (`>= 0`).
#'   Default `0`.
#' @param replicates Positive integer number of stochastic replicates per
#'   `(unit, prompt, model, temperature)` cell. Default `1L`.
#' @param schema An [coding_schema()] object defining expected JSON fields.
#'   Defaults to a single numeric `score` field.
#' @param name Optional descriptive name for the coding design.
#'
#' @return An object of class `"llm_coding_design"`.
#' @export
#'
#' @examples
#' des <- coding_design(
#'   prompts = c(
#'     direct = "Rate emotional distress (1-7): {text}",
#'     rubric = "Using the clinical rubric, rate distress (1-7): {text}"
#'   ),
#'   models = c("model_a", "model_b"),
#'   temperatures = c(0, 0.5),
#'   schema = coding_schema(score = schema_field("numeric", min = 1, max = 7))
#' )
#' des
coding_design <- function(prompts,
                          models,
                          temperatures = 0,
                          replicates = 1L,
                          schema = NULL,
                          name = "default_design") {
  if (!is.character(prompts) || length(prompts) < 1L || anyNA(prompts) || any(!nzchar(prompts))) {
    cli::cli_abort("{.arg prompts} must be a non-empty character vector of prompt templates.")
  }
  if (is.null(names(prompts)) || any(!nzchar(names(prompts)))) {
    names(prompts) <- paste0("prompt_", seq_along(prompts))
  }
  if (any(duplicated(names(prompts)))) {
    cli::cli_abort("Prompt names in {.arg prompts} must be unique.")
  }

  if (!is.character(models) || length(models) < 1L || anyNA(models) || any(!nzchar(models))) {
    cli::cli_abort("{.arg models} must be a non-empty character vector.")
  }
  models <- unique(models)

  if (!is.numeric(temperatures) || length(temperatures) < 1L || anyNA(temperatures) || any(temperatures < 0)) {
    cli::cli_abort("{.arg temperatures} must be a non-empty numeric vector with values >= 0.")
  }
  temperatures <- unique(as.numeric(temperatures))

  assert_scalar_numeric(replicates, "replicates", min = 1)
  replicates <- as.integer(round(replicates))

  if (is.null(schema)) {
    schema <- coding_schema(score = schema_field("numeric"))
  }
  if (!inherits(schema, "llm_coding_schema")) {
    cli::cli_abort("{.arg schema} must be an {.cls llm_coding_schema} object.")
  }
  assert_scalar_character(name, "name")

  structure(
    list(
      name = name,
      prompts = prompts,
      models = models,
      temperatures = temperatures,
      replicates = replicates,
      schema = schema,
      cells_per_unit = length(prompts) * length(models) * length(temperatures) * replicates
    ),
    class = "llm_coding_design"
  )
}

#' Execute a Multi-Facet Structured LLM Coding Design
#'
#' Orchestrates crossed LLM scoring across `units x prompts x models x
#' temperatures x replicates` with content-addressed caching, strict JSON
#' schema validation, retry handling on malformed/invalid outputs, resumable
#' atomic checkpointing (`checkpoint_file`), partial-failure recovery
#' (`on_error`), and pluggable backends.
#'
#' @param data A `data.frame` containing the texts to score and any optional
#'   unit-level metadata (e.g., `human_score`, `group`, `wave`, `cluster`, or
#'   downstream covariates).
#' @param text_col Name of the character column containing input texts.
#' @param id_col Optional name of the unique unit identifier column. If `NULL`,
#'   sequential IDs `"unit_1"`, `"unit_2"`, ... are created.
#' @param design An [coding_design()] object.
#' @param backend Either a single `llm_backend` object (e.g., [backend_mock()],
#'   [backend_openai()], [backend_ollama()]) or a named list of `llm_backend`
#'   objects keyed by model name (with optional `".default"` fallback).
#' @param cache An [coding_cache()] object. Defaults to an in-memory cache.
#' @param max_validation_retries Maximum retries when backend output fails JSON
#'   parsing or schema validation. Default `2L`.
#' @param coerce Logical passed to [validate_coded_output()]. Default `FALSE`.
#' @param keep_cols Logical or character vector specifying which additional
#'   columns from `data` to attach to each coded row. `TRUE` (default) preserves
#'   all non-text columns that do not collide with facet or schema names;
#'   `FALSE` keeps none; a character vector preserves only those columns.
#' @param keep_raw Logical; if `TRUE`, retains raw JSON text in a `raw_output`
#'   column. Default `FALSE`.
#' @param checkpoint_file Optional path to an `.rds` checkpoint file for
#'   resumable batch coding. Completed valid cells are persisted atomically
#'   every `checkpoint_every` new cells and immediately upon failure or
#'   completion; re-running with the same `checkpoint_file` automatically
#'   resumes without re-scoring completed cells.
#' @param checkpoint_every Positive integer specifying how often (in newly
#'   completed cells) to flush `checkpoint_file`. Default `25L`.
#' @param on_error Failure handling policy after all retries for a cell are
#'   exhausted: `"warn_na"` (default: records `NA` for failed cells, populates
#'   `$failures` and `$failed_indices`, and emits a concise summary warning) or
#'   `"stop"` (flushes `checkpoint_file` first so completed cells are never
#'   lost, then aborts with an actionable error).
#'
#' @return An S3 object of class `"llm_coded_data"` with components:
#'   - `scores`: a [tibble::tibble()] with one row per `(unit, prompt, model,
#'     temperature, replicate)` cell, validated schema columns, `cache_hit`,
#'     `attempts`, `valid`, and preserved unit metadata.
#'   - `design`: the input [coding_design()].
#'   - `schema`: the active [coding_schema()].
#'   - `cache_stats`: snapshot of [cache_stats()] after execution.
#'   - `failures`: tibble of cells that failed validation after all retries.
#'   - `failed_indices`: integer vector of input row indices with at least one
#'     failed cell.
#'   - `failed_units`: character vector of unit IDs with at least one failed
#'     cell.
#'   - `resumed_count`: integer count of cells restored from `checkpoint_file`.
#'   - `checkpoint_file`: checkpoint path (or `NULL`).
#' @export
#'
#' @examples
#' texts <- tibble::tibble(
#'   id = paste0("u", 1:8),
#'   text = paste("Sample diary entry", 1:8),
#'   true_score = seq(-1.5, 1.5, length.out = 8)
#' )
#' des <- coding_design(
#'   prompts = c(p1 = "Rate: {text}", p2 = "Score: {text}"),
#'   models = c("m1", "m2"),
#'   schema = coding_schema(score = schema_field("numeric"))
#' )
#' coded <- code_texts(texts, text_col = "text", id_col = "id", design = des, backend = backend_mock())
#' coded
code_texts <- function(data,
                       text_col,
                       id_col = NULL,
                       design,
                       backend,
                       cache = coding_cache("memory"),
                       max_validation_retries = 2L,
                       coerce = FALSE,
                       keep_cols = TRUE,
                       keep_raw = FALSE,
                       checkpoint_file = NULL,
                       checkpoint_every = 25L,
                       on_error = c("warn_na", "stop")) {
  df <- extract_data_frame(data, arg = "data")
  assert_scalar_character(text_col, "text_col")
  assert_columns(df, text_col, arg = "data")
  assert_scalar_character(id_col, "id_col", allow_null = TRUE)
  if (!inherits(design, "llm_coding_design")) {
    cli::cli_abort("{.arg design} must be an {.cls llm_coding_design} object.")
  }
  if (!inherits(cache, "llm_coding_cache")) {
    cli::cli_abort("{.arg cache} must be an {.cls llm_coding_cache} object.")
  }
  assert_scalar_numeric(max_validation_retries, "max_validation_retries", min = 0)
  max_validation_retries <- as.integer(round(max_validation_retries))
  assert_flag(coerce, "coerce")
  assert_flag(keep_raw, "keep_raw")
  assert_scalar_character(checkpoint_file, "checkpoint_file", allow_null = TRUE)
  assert_scalar_numeric(checkpoint_every, "checkpoint_every", min = 1)
  checkpoint_every <- as.integer(round(checkpoint_every))
  on_error <- match.arg(on_error)

  if (!is.null(checkpoint_file)) {
    if (grepl("[[:cntrl:]]", checkpoint_file)) {
      cli::cli_abort("{.arg checkpoint_file} must not contain control characters.")
    }
    if (dir.exists(checkpoint_file)) {
      cli::cli_abort("{.arg checkpoint_file} ({.path {checkpoint_file}}) must be a file path, not an existing directory.")
    }
    if (is_symlink_path(checkpoint_file)) {
      cli::cli_abort("Symbolic link {.arg checkpoint_file} is rejected for security.")
    }
    ckpt_dir <- dirname(checkpoint_file)
    if (!dir.exists(ckpt_dir)) {
      dir.create(ckpt_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # Atomic checkpoint persistence helper with 0600 permissions & strict write verification
  save_checkpoint_atomic <- function(ckpt_map, path) {
    if (is.null(path)) return(invisible(FALSE))
    if (dir.exists(path)) {
      cli::cli_abort("{.arg checkpoint_file} ({.path {path}}) is a directory; cannot write checkpoint.")
    }
    if (is_symlink_path(path)) {
      cli::cli_abort("Refusing to overwrite symbolic link checkpoint file {.path {path}}.")
    }
    env_payload <- list(
      llmmetric_checkpoint_v = 1L,
      schema_hash = design$schema$hash,
      checksum = digest::digest(ckpt_map, algo = "sha256"),
      entries = ckpt_map
    )
    tmp_suffix <- substr(
      digest::digest(list(Sys.getpid(), length(ckpt_map), proc.time()[["elapsed"]]), algo = "sha256"),
      1L, 10L
    )
    tmp_path <- file.path(dirname(path), paste0(".", basename(path), "_", tmp_suffix, ".tmp"))
    on.exit(if (file.exists(tmp_path)) unlink(tmp_path), add = TRUE)
    saveRDS(env_payload, file = tmp_path)
    restrict_file_permissions(tmp_path)
    ok <- suppressWarnings(file.rename(tmp_path, path))
    if (!isTRUE(ok)) {
      ok <- suppressWarnings(file.copy(tmp_path, path, overwrite = TRUE))
      unlink(tmp_path)
    }
    if (!isTRUE(ok)) {
      cli::cli_abort("Failed to persist checkpoint file {.path {path}}.")
    }
    restrict_file_permissions(path)
    invisible(TRUE)
  }

  load_checkpoint_safe <- function(path, expected_schema_hash) {
    if (is.null(path) || !file.exists(path)) {
      return(list())
    }
    obj <- tryCatch(readRDS(path), error = function(e) NULL)
    valid_ck <- is.list(obj) &&
      identical(obj$llmmetric_checkpoint_v, 1L) &&
      identical(obj$schema_hash, expected_schema_hash) &&
      is.list(obj$entries) &&
      identical(digest::digest(obj$entries, algo = "sha256"), obj$checksum)
    if (!valid_ck) {
      cli::cli_warn(
        "Ignoring incompatible or corrupted checkpoint file {.path {path}} and starting fresh."
      )
      return(list())
    }
    obj$entries
  }

  ckpt_entries <- load_checkpoint_safe(checkpoint_file, design$schema$hash)
  ckpt_dirty_count <- 0L
  resumed_count <- 0L

  # Resolve unit IDs
  if (is.null(id_col)) {
    unit_ids <- paste0("unit_", seq_len(nrow(df)))
  } else {
    assert_columns(df, id_col, arg = "data")
    unit_ids <- as.character(df[[id_col]])
    if (anyNA(unit_ids) || any(duplicated(unit_ids))) {
      cli::cli_abort("{.arg id_col} ({.val {id_col}}) must contain unique non-missing identifiers.")
    }
  }

  # Validate backend or named list of backends
  resolve_backend <- function(model_name) {
    if (inherits(backend, "llm_backend")) {
      return(backend)
    }
    if (is.list(backend)) {
      if (model_name %in% names(backend) && inherits(backend[[model_name]], "llm_backend")) {
        return(backend[[model_name]])
      }
      if (".default" %in% names(backend) && inherits(backend[[".default"]], "llm_backend")) {
        return(backend[[".default"]])
      }
    }
    cli::cli_abort(
      "No valid {.cls llm_backend} provided for model {.val {model_name}}."
    )
  }

  # Determine metadata columns to preserve
  reserved <- c(
    "unit_id", "prompt", "model", "temperature", "replicate",
    names(design$schema$fields), "cache_hit", "attempts", "valid", "raw_output"
  )
  meta_cols <- character()
  if (isTRUE(keep_cols)) {
    meta_cols <- setdiff(names(df), c(text_col, id_col, reserved))
  } else if (is.character(keep_cols)) {
    assert_columns(df, keep_cols, arg = "data")
    meta_cols <- setdiff(keep_cols, reserved)
  } else if (!isFALSE(keep_cols)) {
    cli::cli_abort("{.arg keep_cols} must be TRUE, FALSE, or a character vector of column names.")
  }

  # Build crossed grid
  grid <- expand.grid(
    unit_idx = seq_len(nrow(df)),
    prompt = names(design$prompts),
    model = design$models,
    temperature = design$temperatures,
    replicate = seq_len(design$replicates),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  n_total <- nrow(grid)

  schema <- design$schema
  field_names <- names(schema$fields)

  # Preallocate result columns
  field_cols <- lapply(schema$fields, function(f) {
    switch(
      f$type,
      numeric = rep(NA_real_, n_total),
      integer = rep(NA_integer_, n_total),
      factor = factor(rep(NA_character_, n_total), levels = f$levels),
      logical = rep(NA, n_total),
      character = rep(NA_character_, n_total)
    )
  })
  names(field_cols) <- field_names

  cache_hit_vec <- logical(n_total)
  attempts_vec <- integer(n_total)
  valid_vec <- logical(n_total)
  raw_vec <- if (isTRUE(keep_raw)) rep(NA_character_, n_total) else NULL
  failure_rows <- list()

  for (row_i in seq_len(n_total)) {
    u_idx <- grid$unit_idx[row_i]
    uid <- unit_ids[u_idx]
    pid <- grid$prompt[row_i]
    mid <- grid$model[row_i]
    temp <- grid$temperature[row_i]
    rep_i <- grid$replicate[row_i]
    txt <- as.character(df[[text_col]][u_idx])

    row_meta <- as.list(df[u_idx, , drop = FALSE])
    row_meta$text <- txt
    row_meta$unit_id <- uid

    prompt_rendered <- render_prompt_template(design$prompts[[pid]], row_meta)
    b <- resolve_backend(mid)

    ckey <- cache_key(
      schema_hash = schema$hash,
      unit_id = uid,
      text = txt,
      prompt_id = pid,
      prompt_text = prompt_rendered,
      model = mid,
      temperature = temp,
      replicate = rep_i,
      backend_id = b$id
    )

    # 1. Resume from checkpoint_file if present
    if (!is.null(ckpt_entries[[ckey]])) {
      ck_val <- ckpt_entries[[ckey]]
      resumed_count <- resumed_count + 1L
      cache_hit_vec[row_i] <- TRUE
      attempts_vec[row_i] <- 0L
      valid_vec[row_i] <- TRUE
      for (fnm in field_names) {
        field_cols[[fnm]][row_i] <- ck_val$data[[fnm]]
      }
      if (isTRUE(keep_raw)) {
        raw_vec[row_i] <- ck_val$raw %||% NA_character_
      }
      next
    }

    # 2. Check coding cache
    cached <- cache_get(cache, ckey)
    if (!is.null(cached)) {
      cache_hit_vec[row_i] <- TRUE
      attempts_vec[row_i] <- 0L
      valid_vec[row_i] <- TRUE
      for (fnm in field_names) {
        field_cols[[fnm]][row_i] <- cached$data[[fnm]]
      }
      if (isTRUE(keep_raw)) {
        raw_vec[row_i] <- cached$raw %||% NA_character_
      }
      if (!is.null(checkpoint_file)) {
        ckpt_entries[[ckey]] <- list(
          data = cached$data,
          raw = if (isTRUE(keep_raw)) cached$raw else NULL
        )
      }
      next
    }

    # 3. Cache miss: invoke backend with validation retries
    prev_errors <- character()
    succeeded <- FALSE
    last_raw <- NA_character_
    used_attempts <- 0L

    for (attempt in seq_len(max_validation_retries + 1L)) {
      used_attempts <- attempt
      req <- list(
        unit_id = uid,
        text = txt,
        prompt_id = pid,
        prompt_text = prompt_rendered,
        model = mid,
        temperature = temp,
        replicate = rep_i,
        schema = schema,
        attempt = attempt,
        previous_errors = prev_errors,
        meta = row_meta
      )

      resp <- tryCatch(
        backend_score(b, req),
        error = function(e) {
          structure(list(error = conditionMessage(e)), class = "backend_call_error")
        }
      )

      if (inherits(resp, "backend_call_error")) {
        prev_errors <- paste0("Backend error: ", resp$error)
        next
      }

      last_raw <- if (is.character(resp$content)) {
        resp$content
      } else {
        as.character(jsonlite::toJSON(resp$content, auto_unbox = TRUE))
      }

      val <- validate_coded_output(resp$content, schema = schema, coerce = coerce)
      if (isTRUE(val$valid)) {
        succeeded <- TRUE
        for (fnm in field_names) {
          field_cols[[fnm]][row_i] <- val$data[[fnm]]
        }
        entry_val <- list(data = val$data, raw = if (isTRUE(keep_raw)) last_raw else NULL)
        cache_set(cache, ckey, entry_val)
        if (!is.null(checkpoint_file)) {
          ckpt_entries[[ckey]] <- entry_val
          ckpt_dirty_count <- ckpt_dirty_count + 1L
          if (ckpt_dirty_count >= checkpoint_every) {
            save_checkpoint_atomic(ckpt_entries, checkpoint_file)
            ckpt_dirty_count <- 0L
          }
        }
        break
      } else {
        prev_errors <- val$errors
      }
    }

    cache_hit_vec[row_i] <- FALSE
    attempts_vec[row_i] <- used_attempts
    valid_vec[row_i] <- succeeded
    if (isTRUE(keep_raw)) {
      raw_vec[row_i] <- last_raw
    }

    if (!succeeded) {
      err_msg <- paste(prev_errors, collapse = "; ")
      if (identical(on_error, "stop")) {
        if (!is.null(checkpoint_file) && length(ckpt_entries) > 0L) {
          save_checkpoint_atomic(ckpt_entries, checkpoint_file)
        }
        ckpt_hint <- if (!is.null(checkpoint_file)) {
          paste0(" Saved ", length(ckpt_entries), " completed cell(s) to checkpoint '", checkpoint_file, "' for resumable retry.")
        } else {
          ""
        }
        cli::cli_abort(
          "Coding failed at unit index {u_idx} (unit_id = {.val {uid}}, prompt = {.val {pid}}, model = {.val {mid}}) after {used_attempts} attempt(s): {err_msg}.{ckpt_hint}"
        )
      }
      failure_rows[[length(failure_rows) + 1L]] <- tibble::tibble(
        unit_idx = u_idx,
        unit_id = uid,
        prompt = pid,
        model = mid,
        temperature = temp,
        replicate = rep_i,
        attempts = used_attempts,
        error = err_msg
      )
    }
  }

  # Final flush of checkpoint file if dirty
  if (!is.null(checkpoint_file) && (ckpt_dirty_count > 0L || !file.exists(checkpoint_file))) {
    save_checkpoint_atomic(ckpt_entries, checkpoint_file)
  }

  scores_list <- list(
    unit_id = unit_ids[grid$unit_idx],
    prompt = grid$prompt,
    model = grid$model,
    temperature = grid$temperature,
    replicate = grid$replicate
  )
  scores_list <- c(scores_list, field_cols)
  scores_list$cache_hit <- cache_hit_vec
  scores_list$attempts <- attempts_vec
  scores_list$valid <- valid_vec
  if (isTRUE(keep_raw)) {
    scores_list$raw_output <- raw_vec
  }

  scores_tbl <- tibble::as_tibble(scores_list)
  if (length(meta_cols) > 0L) {
    meta_tbl <- df[grid$unit_idx, meta_cols, drop = FALSE]
    scores_tbl <- tibble::as_tibble(cbind(scores_tbl, meta_tbl))
  }

  failures_tbl <- if (length(failure_rows) > 0L) {
    do.call(rbind, failure_rows)
  } else {
    tibble::tibble(
      unit_idx = integer(),
      unit_id = character(),
      prompt = character(),
      model = character(),
      temperature = numeric(),
      replicate = integer(),
      attempts = integer(),
      error = character()
    )
  }

  failed_indices <- sort(unique(failures_tbl$unit_idx))
  failed_units <- unique(failures_tbl$unit_id)

  if (nrow(failures_tbl) > 0L && identical(on_error, "warn_na")) {
    cli::cli_warn(
      "{nrow(failures_tbl)} coding cell{?s} failed across {length(failed_units)} unit{?s} (indices: {.val {utils::head(failed_indices, 10L)}}); inspect {.field $failures} and {.field $failed_indices}."
    )
  }

  structure(
    list(
      scores = scores_tbl,
      design = design,
      schema = schema,
      cache_stats = cache_stats(cache),
      failures = failures_tbl,
      failed_indices = failed_indices,
      failed_units = failed_units,
      resumed_count = resumed_count,
      checkpoint_file = checkpoint_file,
      n_units = nrow(df),
      id_col = "unit_id",
      primary_score = field_names[1L]
    ),
    class = "llm_coded_data"
  )
}

#' Aggregate Multi-Facet LLM Scores to Unit Level
#'
#' Collapses multi-facet scores (e.g., across prompts, models, temperatures, or
#' replicates) into unit-level composite scores suitable for calibration, DPF,
#' or downstream Prediction-Powered Inference. Unit-level covariates that are
#' constant within each aggregation group are preserved automatically.
#'
#' @param x An `llm_coded_data` object or a `data.frame`.
#' @param score_col Name of the numeric score column to aggregate.
#' @param id_col Name of the unit identifier column. Default `"unit_id"`.
#' @param by Optional character vector of additional grouping columns (e.g.,
#'   `"model"` or `"prompt"`).
#' @param fn Summary function applied to `score_col` (default `mean`).
#' @param na.rm Logical; remove missing values before aggregating. Default `TRUE`.
#'
#' @return A [tibble::tibble()] containing `id_col`, `by` columns, aggregated
#'   `score_col`, `n_ratings`, `rating_sd`, and preserved group-constant
#'   metadata columns.
#' @export
#'
#' @examples
#' data(ema_coded)
#' agg <- aggregate_scores(ema_coded, score_col = "llm_score", id_col = "unit_id")
#' head(agg)
aggregate_scores <- function(x,
                             score_col = "score",
                             id_col = "unit_id",
                             by = NULL,
                             fn = mean,
                             na.rm = TRUE) {
  if (inherits(x, "llm_coded_data") && missing(score_col)) {
    score_col <- x$primary_score
  }
  df <- extract_data_frame(x, arg = "x")
  assert_scalar_character(score_col, "score_col")
  assert_scalar_character(id_col, "id_col")
  if (!is.function(fn)) {
    cli::cli_abort("{.arg fn} must be a summary function.")
  }
  assert_flag(na.rm, "na.rm")
  group_cols <- unique(c(id_col, by %||% character()))
  assert_columns(df, c(group_cols, score_col), arg = "x")
  if (!is.numeric(df[[score_col]])) {
    cli::cli_abort("Column {.val {score_col}} must be numeric to aggregate.")
  }

  # Create stable grouping key preserving order of first appearance
  key_parts <- lapply(group_cols, function(col) as.character(df[[col]]))
  key <- do.call(paste, c(key_parts, list(sep = "\r")))
  u_keys <- unique(key)
  idx_list <- split(seq_len(nrow(df)), factor(key, levels = u_keys))

  first_idx <- vapply(idx_list, `[`, integer(1L), 1L)
  out_list <- lapply(group_cols, function(col) df[[col]][first_idx])
  names(out_list) <- group_cols

  scores_vec <- df[[score_col]]
  agg_vals <- vapply(idx_list, function(ii) {
    vals <- scores_vec[ii]
    if (isTRUE(na.rm)) vals <- vals[!is.na(vals)]
    if (length(vals) == 0L) return(NA_real_)
    as.numeric(fn(vals))[1L]
  }, numeric(1L))

  n_ratings <- vapply(idx_list, function(ii) {
    vals <- scores_vec[ii]
    if (isTRUE(na.rm)) sum(!is.na(vals)) else length(vals)
  }, integer(1L))

  sd_vals <- vapply(idx_list, function(ii) {
    vals <- scores_vec[ii]
    if (isTRUE(na.rm)) vals <- vals[!is.na(vals)]
    if (length(vals) <= 1L) NA_real_ else stats::sd(vals)
  }, numeric(1L))

  out_list[[score_col]] <- unname(agg_vals)
  out_list$n_ratings <- unname(n_ratings)
  out_list$rating_sd <- unname(sd_vals)

  # Preserve any additional columns that are invariant within every group
  skip_cols <- c(
    group_cols, score_col, "prompt", "model", "temperature", "replicate",
    "cache_hit", "attempts", "valid", "raw_output", "n_ratings", "rating_sd"
  )
  cand_cols <- setdiff(names(df), skip_cols)
  for (col in cand_cols) {
    col_vec <- df[[col]]
    is_const <- all(vapply(idx_list, function(ii) {
      vals <- col_vec[ii]
      if (all(is.na(vals))) return(TRUE)
      if (anyNA(vals)) return(FALSE)
      length(unique(vals)) == 1L
    }, logical(1L)))
    if (is_const) {
      out_list[[col]] <- col_vec[first_idx]
    }
  }

  tibble::as_tibble(out_list)
}

#' @export
as_tibble.llm_coded_data <- function(x, ...) {
  tibble::as_tibble(x$scores)
}

#' @export
print.llm_coding_design <- function(x, ...) {
  cli::cli_h3("LLM Coding Design: {x$name}")
  cli::cli_text(
    "Prompts: {length(x$prompts)} | Models: {length(x$models)} | Temperatures: {length(x$temperatures)} | Replicates: {x$replicates}"
  )
  cli::cli_text("Cells per unit: {x$cells_per_unit}")
  cli::cli_text("Models: {.val {x$models}}")
  cli::cli_text("Prompt IDs: {.val {names(x$prompts)}}")
  invisible(x)
}

#' @export
print.llm_coded_data <- function(x, ...) {
  n_valid <- sum(x$scores$valid, na.rm = TRUE)
  n_tot <- nrow(x$scores)
  cli::cli_h3("LLM Coded Data ({x$design$name})")
  cli::cli_text(
    "Units: {x$n_units} | Rows: {n_tot} | Valid: {n_valid}/{n_tot} | Failures: {nrow(x$failures)}"
  )
  cli::cli_text(
    "Cache ({x$cache_stats$type}): {x$cache_stats$hits} hits, {x$cache_stats$misses} misses"
  )
  print(x$scores, n = 6L)
  invisible(x)
}

#' @export
summary.llm_coded_data <- function(object, ...) {
  sc <- object$scores
  primary <- object$primary_score
  summary_vals <- if (is.numeric(sc[[primary]])) {
    list(
      mean = mean(sc[[primary]], na.rm = TRUE),
      sd = stats::sd(sc[[primary]], na.rm = TRUE),
      min = min(sc[[primary]], na.rm = TRUE),
      max = max(sc[[primary]], na.rm = TRUE)
    )
  } else {
    list(counts = table(sc[[primary]], useNA = "ifany"))
  }
  out <- list(
    design_name = object$design$name,
    n_units = object$n_units,
    n_rows = nrow(sc),
    n_valid = sum(sc$valid, na.rm = TRUE),
    n_failures = nrow(object$failures),
    cache_stats = object$cache_stats,
    primary_score = primary,
    score_summary = summary_vals
  )
  class(out) <- "summary.llm_coded_data"
  out
}

#' @export
print.summary.llm_coded_data <- function(x, ...) {
  cli::cli_h3("Summary of LLM Coded Data ({x$design_name})")
  cli::cli_text("Units: {x$n_units} | Total codings: {x$n_rows} | Valid: {x$n_valid} | Failures: {x$n_failures}")
  if (!is.null(x$score_summary$mean)) {
    cli::cli_text(
      "Primary score ({x$primary_score}): mean = {fmt_num(x$score_summary$mean)}, sd = {fmt_num(x$score_summary$sd)}, range = [{fmt_num(x$score_summary$min)}, {fmt_num(x$score_summary$max)}]"
    )
  }
  invisible(x)
}
