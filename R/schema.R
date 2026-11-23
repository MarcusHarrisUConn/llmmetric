#' Define a Field for an LLM Coding Schema
#'
#' Specifies the expected type, bounds, allowed levels, nullability, and
#' requirement status for a single field in structured JSON LLM output.
#'
#' @param type Field type: one of `"numeric"`, `"integer"`, `"factor"`,
#'   `"enum"`, `"logical"`, or `"character"`. (`"enum"` is an alias for
#'   `"factor"`.)
#' @param min Optional numeric lower bound (for `"numeric"` or `"integer"`).
#' @param max Optional numeric upper bound (for `"numeric"` or `"integer"`).
#' @param levels Character vector of allowed category labels (required when
#'   `type` is `"factor"` or `"enum"`).
#' @param required Logical; whether the field must be present in every parsed
#'   JSON payload. Default `TRUE`.
#' @param nullable Logical; whether JSON `null` / `NA` is allowed. Default `FALSE`.
#' @param description Optional character description embedded in JSON Schema
#'   prompts and structured-output specifications.
#'
#' @return An object of class `"llm_schema_field"`.
#' @export
#'
#' @examples
#' schema_field("numeric", min = 1, max = 7, description = "Distress rating")
#' schema_field("factor", levels = c("low", "medium", "high"))
schema_field <- function(type = c("numeric", "integer", "factor", "enum", "logical", "character"),
                         min = NULL,
                         max = NULL,
                         levels = NULL,
                         required = TRUE,
                         nullable = FALSE,
                         description = NULL) {
  type <- match.arg(type)
  if (identical(type, "enum")) {
    type <- "factor"
  }
  assert_flag(required, "required")
  assert_flag(nullable, "nullable")
  assert_scalar_numeric(min, "min", allow_null = TRUE)
  assert_scalar_numeric(max, "max", allow_null = TRUE)
  assert_scalar_character(description, "description", allow_null = TRUE)

  if (!is.null(min) && !is.null(max) && min > max) {
    cli::cli_abort("{.arg min} ({min}) cannot be greater than {.arg max} ({max}).")
  }

  if (type == "factor") {
    if (is.null(levels) || !is.character(levels) || length(levels) < 2L || anyNA(levels)) {
      cli::cli_abort(
        "Fields of type {.val factor}/{.val enum} require a character vector {.arg levels} with at least 2 values."
      )
    }
    levels <- unique(levels)
  } else if (!is.null(levels)) {
    cli::cli_abort("{.arg levels} is only valid for {.val factor} or {.val enum} fields.")
  }

  if (!(type %in% c("numeric", "integer")) && (!is.null(min) || !is.null(max))) {
    cli::cli_abort("{.arg min} and {.arg max} are only valid for {.val numeric} or {.val integer} fields.")
  }

  structure(
    list(
      type = type,
      min = min,
      max = max,
      levels = levels,
      required = required,
      nullable = nullable,
      description = description
    ),
    class = "llm_schema_field"
  )
}

#' Define a Structured Output Schema for LLM Coding
#'
#' Creates a strict schema describing the JSON object returned by an LLM rater.
#' Schemas drive JSON Schema generation (for OpenAI structured outputs or
#' Ollama JSON schema constraints) as well as strict response validation and
#' retry logic in [code_texts()].
#'
#' @param ... Named field specifications created with [schema_field()] or
#'   shorthand type strings (`"numeric"`, `"integer"`, `"logical"`, `"character"`).
#' @param .fields Optional named list of field specifications (merged with `...`).
#' @param name Character name for the schema (used by JSON Schema APIs).
#' @param strict Logical; if `TRUE` (default), unexpected extra keys in the
#'   parsed JSON payload trigger validation failure.
#'
#' @return An object of class `"llm_coding_schema"`.
#' @export
#'
#' @examples
#' s <- coding_schema(
#'   score = schema_field("numeric", min = 1, max = 7),
#'   label = schema_field("factor", levels = c("negative", "neutral", "positive")),
#'   rationale = schema_field("character", required = FALSE)
#' )
#' s
coding_schema <- function(...,
                          .fields = NULL,
                          name = "llm_coding_output",
                          strict = TRUE) {
  assert_scalar_character(name, "name")
  assert_flag(strict, "strict")

  dots <- list(...)
  fields <- c(dots, .fields %||% list())

  if (length(fields) == 0L) {
    cli::cli_abort("A {.fn coding_schema} must define at least one field.")
  }
  nms <- names(fields)
  if (is.null(nms) || any(!nzchar(nms)) || any(duplicated(nms))) {
    cli::cli_abort("All fields in {.fn coding_schema} must have unique, non-empty names.")
  }

  normalized <- lapply(fields, function(f) {
    if (inherits(f, "llm_schema_field")) {
      return(f)
    }
    if (is.character(f) && length(f) == 1L) {
      return(schema_field(type = f))
    }
    cli::cli_abort(
      "Each field must be a {.cls llm_schema_field} or a single type string."
    )
  })
  names(normalized) <- nms

  schema_hash <- digest::digest(list(name = name, strict = strict, fields = normalized), algo = "sha256")

  structure(
    list(
      name = name,
      strict = strict,
      fields = normalized,
      hash = schema_hash
    ),
    class = "llm_coding_schema"
  )
}

#' Convert a Coding Schema to a JSON Schema List
#'
#' Generates a draft-07 JSON Schema representation suitable for prompt
#' instructions, OpenAI `response_format = list(type = "json_schema", ...)`,
#' or Ollama `format` parameters.
#'
#' @param schema An object of class `"llm_coding_schema"`.
#'
#' @return A named list representing a JSON Schema object.
#' @export
#'
#' @examples
#' s <- coding_schema(score = schema_field("numeric", min = 0, max = 10))
#' as_json_schema(s)
as_json_schema <- function(schema) {
  if (!inherits(schema, "llm_coding_schema")) {
    cli::cli_abort("{.arg schema} must be an {.cls llm_coding_schema} object.")
  }

  properties <- list()
  required_fields <- character()

  for (nm in names(schema$fields)) {
    f <- schema$fields[[nm]]
    base_type <- switch(
      f$type,
      numeric = "number",
      integer = "integer",
      factor = "string",
      logical = "boolean",
      character = "string"
    )
    prop <- list(
      type = if (isTRUE(f$nullable)) c(base_type, "null") else base_type
    )
    if (!is.null(f$description)) {
      prop$description <- f$description
    }
    if (!is.null(f$min)) {
      prop$minimum <- f$min
    }
    if (!is.null(f$max)) {
      prop$maximum <- f$max
    }
    if (!is.null(f$levels)) {
      prop$enum <- as.list(f$levels)
    }
    properties[[nm]] <- prop
    if (isTRUE(f$required)) {
      required_fields <- c(required_fields, nm)
    }
  }

  list(
    type = "object",
    properties = properties,
    required = required_fields,
    additionalProperties = !isTRUE(schema$strict)
  )
}

#' Validate and Parse Structured LLM Output Against a Coding Schema
#'
#' Parses a JSON string (hardened against prompt-injected multi-block payloads,
#' duplicate keys, oversized responses, control characters, and non-finite
#' values) or validates an already-parsed R named list against an
#' [coding_schema()].
#'
#' @param x A JSON character string or a named R list.
#' @param schema An object of class `"llm_coding_schema"`.
#' @param coerce Logical; if `TRUE`, allows safe coercion of finite numeric
#'   strings (e.g. `"4.5"` to `4.5`) and boolean strings. Non-finite strings
#'   such as `"NaN"`, `"Inf"`, or `"-Infinity"` are always rejected. Default
#'   `FALSE` (strict).
#' @param max_bytes Maximum allowed byte length of a raw JSON response string.
#'   Default `262144L` (256 KiB).
#'
#' @return A list with elements:
#'   - `valid`: logical scalar indicating whether validation succeeded.
#'   - `data`: named list of validated R values (or `NULL` if invalid).
#'   - `errors`: character vector of validation failure messages.
#' @export
#'
#' @examples
#' s <- coding_schema(score = schema_field("integer", min = 1, max = 5))
#' validate_coded_output('{"score": 4}', s)
#' validate_coded_output('{"score": 9}', s)
validate_coded_output <- function(x, schema, coerce = FALSE, max_bytes = 262144L) {
  if (!inherits(schema, "llm_coding_schema")) {
    cli::cli_abort("{.arg schema} must be an {.cls llm_coding_schema} object.")
  }
  assert_flag(coerce, "coerce")
  assert_scalar_numeric(max_bytes, "max_bytes", min = 16)
  max_bytes <- as.integer(round(max_bytes))

  parsed <- NULL
  if (is.character(x) && length(x) == 1L) {
    extracted <- extract_json_payload(x, max_bytes = max_bytes)
    if (!isTRUE(extracted$ok)) {
      return(list(valid = FALSE, data = NULL, errors = extracted$error))
    }
    parsed <- tryCatch(
      jsonlite::fromJSON(extracted$json, simplifyVector = FALSE),
      error = function(e) {
        structure(list(message = conditionMessage(e)), class = "json_parse_error")
      }
    )
    if (inherits(parsed, "json_parse_error")) {
      return(list(
        valid = FALSE,
        data = NULL,
        errors = paste0("JSON parse error: ", parsed$message)
      ))
    }
  } else if (is.list(x)) {
    parsed <- x
  } else {
    return(list(
      valid = FALSE,
      data = NULL,
      errors = "Input must be a JSON string or named list."
    ))
  }

  if (!is.list(parsed) || (length(parsed) > 0L && is.null(names(parsed)))) {
    return(list(
      valid = FALSE,
      data = NULL,
      errors = "Parsed JSON must be an object (key-value map)."
    ))
  }

  if (has_duplicate_json_keys(parsed)) {
    return(list(
      valid = FALSE,
      data = NULL,
      errors = "Duplicate JSON key(s) detected (rejected to prevent key-smuggling attacks)."
    ))
  }

  errors <- character()
  out <- list()
  field_names <- names(schema$fields)

  if (isTRUE(schema$strict)) {
    extra <- setdiff(names(parsed), field_names)
    if (length(extra) > 0L) {
      errors <- c(errors, paste0("Unexpected field(s): ", paste(extra, collapse = ", ")))
    }
  }

  for (nm in field_names) {
    f <- schema$fields[[nm]]
    has_key <- nm %in% names(parsed)

    if (!has_key) {
      if (isTRUE(f$required)) {
        errors <- c(errors, paste0("Missing required field '", nm, "'."))
        next
      } else {
        out[[nm]] <- switch(
          f$type,
          numeric = NA_real_,
          integer = NA_integer_,
          factor = factor(NA_character_, levels = f$levels),
          logical = NA,
          character = NA_character_
        )
        next
      }
    }

    val <- parsed[[nm]]
    is_null_val <- is.null(val) || (length(val) == 1L && is.logical(val) && is.na(val)) ||
      (length(val) == 1L && !is.list(val) && !is.character(val) && is.na(val) && !is.nan(val))
    if (is_null_val) {
      if (isTRUE(f$nullable)) {
        out[[nm]] <- switch(
          f$type,
          numeric = NA_real_,
          integer = NA_integer_,
          factor = factor(NA_character_, levels = f$levels),
          logical = NA,
          character = NA_character_
        )
        next
      } else {
        errors <- c(errors, paste0("Field '", nm, "' cannot be null/NA."))
        next
      }
    }

    if (length(val) != 1L || is.list(val)) {
      errors <- c(errors, paste0("Field '", nm, "' must be a scalar value."))
      next
    }

    if (f$type == "numeric") {
      if (is.numeric(val) && !is.logical(val)) {
        num_val <- as.numeric(val)
      } else if (isTRUE(coerce) && is.character(val)) {
        v_trim <- trimws(val)
        if (tolower(v_trim) %in% c("nan", "inf", "-inf", "+inf", "infinity", "-infinity", "+infinity", "na")) {
          errors <- c(errors, paste0("Field '", nm, "' contains non-finite numeric token '", val, "'."))
          next
        }
        num_val <- suppressWarnings(as.numeric(v_trim))
        if (is.na(num_val)) {
          errors <- c(errors, paste0("Field '", nm, "' could not be coerced to numeric."))
          next
        }
      } else {
        errors <- c(errors, paste0("Field '", nm, "' must be numeric."))
        next
      }
      if (!is.finite(num_val)) {
        errors <- c(errors, paste0("Field '", nm, "' must be finite."))
        next
      }
      abs_cap <- max(1e15, abs(f$min %||% 0), abs(f$max %||% 0))
      if (abs(num_val) > abs_cap) {
        errors <- c(errors, paste0("Field '", nm, "' exceeds safe finite magnitude limit."))
        next
      }
      if (!is.null(f$min) && num_val < f$min) {
        errors <- c(errors, paste0("Field '", nm, "' (", num_val, ") is below min (", f$min, ")."))
        next
      }
      if (!is.null(f$max) && num_val > f$max) {
        errors <- c(errors, paste0("Field '", nm, "' (", num_val, ") exceeds max (", f$max, ")."))
        next
      }
      out[[nm]] <- num_val
    } else if (f$type == "integer") {
      if (is.numeric(val) && !is.logical(val)) {
        num_val <- as.numeric(val)
      } else if (isTRUE(coerce) && is.character(val)) {
        v_trim <- trimws(val)
        if (tolower(v_trim) %in% c("nan", "inf", "-inf", "+inf", "infinity", "-infinity", "+infinity", "na")) {
          errors <- c(errors, paste0("Field '", nm, "' contains non-finite integer token '", val, "'."))
          next
        }
        num_val <- suppressWarnings(as.numeric(v_trim))
        if (is.na(num_val)) {
          errors <- c(errors, paste0("Field '", nm, "' could not be coerced to integer."))
          next
        }
      } else {
        errors <- c(errors, paste0("Field '", nm, "' must be an integer."))
        next
      }
      if (!is.finite(num_val) || abs(num_val) > .Machine$integer.max || abs(num_val - round(num_val)) > 1e-8) {
        errors <- c(errors, paste0("Field '", nm, "' (", num_val, ") is not a valid finite integer."))
        next
      }
      int_val <- as.integer(round(num_val))
      if (!is.null(f$min) && int_val < f$min) {
        errors <- c(errors, paste0("Field '", nm, "' (", int_val, ") is below min (", f$min, ")."))
        next
      }
      if (!is.null(f$max) && int_val > f$max) {
        errors <- c(errors, paste0("Field '", nm, "' (", int_val, ") exceeds max (", f$max, ")."))
        next
      }
      out[[nm]] <- int_val
    } else if (f$type == "factor") {
      if (!is.character(val)) {
        if (isTRUE(coerce) && !is.logical(val)) {
          val <- as.character(val)
        } else {
          errors <- c(errors, paste0("Field '", nm, "' must be a character label."))
          next
        }
      }
      if (!(val %in% f$levels)) {
        errors <- c(
          errors,
          paste0(
            "Field '", nm, "' ('", val, "') is not in allowed levels: ",
            paste(f$levels, collapse = ", "), "."
          )
        )
        next
      }
      out[[nm]] <- factor(val, levels = f$levels)
    } else if (f$type == "logical") {
      if (is.logical(val) && !is.na(val)) {
        out[[nm]] <- val
      } else if (isTRUE(coerce) && is.character(val) && tolower(trimws(val)) %in% c("true", "false")) {
        out[[nm]] <- tolower(trimws(val)) == "true"
      } else {
        errors <- c(errors, paste0("Field '", nm, "' must be logical (boolean)."))
        next
      }
    } else if (f$type == "character") {
      if (is.character(val) && !is.na(val)) {
        out[[nm]] <- val
      } else if (isTRUE(coerce) && !is.na(val)) {
        out[[nm]] <- as.character(val)
      } else {
        errors <- c(errors, paste0("Field '", nm, "' must be a character string."))
        next
      }
    }
  }

  if (length(errors) > 0L) {
    return(list(valid = FALSE, data = NULL, errors = errors))
  }

  list(valid = TRUE, data = out, errors = character())
}

#' @export
print.llm_coding_schema <- function(x, ...) {
  cli::cli_h3("LLM Coding Schema: {x$name}")
  cli::cli_text("Strict: {x$strict} | Fields: {length(x$fields)} | Hash: {substr(x$hash, 1L, 12L)}")
  for (nm in names(x$fields)) {
    f <- x$fields[[nm]]
    extra <- character()
    if (!is.null(f$min) || !is.null(f$max)) {
      extra <- c(extra, paste0("[", f$min %||% "-Inf", ", ", f$max %||% "Inf", "]"))
    }
    if (!is.null(f$levels)) {
      extra <- c(extra, paste0("{", paste(f$levels, collapse = ", "), "}"))
    }
    if (!isTRUE(f$required)) {
      extra <- c(extra, "optional")
    }
    if (isTRUE(f$nullable)) {
      extra <- c(extra, "nullable")
    }
    detail <- if (length(extra) > 0L) paste0(" ", paste(extra, collapse = " ")) else ""
    cli::cli_li("{.field {nm}}: {.val {f$type}}{detail}")
  }
  invisible(x)
}
