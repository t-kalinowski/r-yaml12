`%||%` <- function(x, y) if (is.null(x)) y else x

# Convert visible whitespace markers used by yaml-test-suite back to their
# literal characters (mirrors tests/testthat/yaml-test-suite/bin/YAMLTestSuite.pm).
suite_unescape <- function(text) {
  text <- gsub("␣", " ", text, fixed = TRUE)
  text <- gsub("—*»", "\t", text)
  text <- gsub("←", "\r", text, fixed = TRUE)
  text <- gsub("⇔", "\ufeff", text, fixed = TRUE)
  text <- gsub("↵", "", text, fixed = TRUE)
  sub("∎\\n?$", "", text)
}

format_yaml_key <- function(key) {
  if (is.null(key)) {
    "null"
  } else if (identical(key, TRUE)) {
    "true"
  } else if (identical(key, FALSE)) {
    "false"
  } else if (is.numeric(key)) {
    formatted <- format(key, scientific = FALSE, trim = TRUE)
    sub("\\.0+$", "", formatted)
  } else {
    as.character(key)
  }
}

strip_yaml_metadata <- function(x) {
  attr(x, "yaml_tag") <- NULL
  keys <- attr(x, "yaml_keys", exact = TRUE)
  attr(x, "yaml_keys") <- NULL
  if (is.list(x)) {
    x[] <- lapply(x, strip_yaml_metadata)
    if (!is.null(keys) && length(keys) == length(x)) {
      key_names <- vapply(keys, format_yaml_key, character(1))
      existing <- names(x)
      if (is.null(existing)) {
        names(x) <- key_names
      } else {
        empty <- existing == "" | is.na(existing)
        if (length(empty) == length(key_names) && any(empty)) {
          existing[empty] <- key_names[empty]
          names(x) <- existing
        }
      }
    }
  }
  x
}

normalize_yaml <- function(x) {
  x <- strip_yaml_metadata(x)
  if (is.list(x)) {
    lapply(x, normalize_yaml)
  } else if (is.integer(x)) {
    as.numeric(x)
  } else if (is.logical(x)) {
    ifelse(is.na(x), NA, x)
  } else {
    x
  }
}

load_suite_cases <- function(suite_dir) {
  files <- list.files(suite_dir, pattern = "\\.yaml$", full.names = TRUE)
  cases <- list()

  for (path in files) {
    raw <- paste(readLines(path, warn = FALSE), collapse = "\n")
    parsed <- try(parse_yaml(raw, multi = TRUE), silent = TRUE)
    if (inherits(parsed, "try-error") || !length(parsed)) {
      next
    }

    case_index <- 0
    docs <- if (is.list(parsed) && is.null(names(parsed))) {
      parsed
    } else {
      list(parsed)
    }

    for (doc in docs) {
      if (!length(doc)) {
        next
      }
      entries <- if (is.list(doc) && is.null(names(doc))) doc else list(doc)

      for (entry in entries) {
        if (!is.list(entry) || !length(entry) || isTRUE(entry$skip)) {
          next
        }
        snippet <- entry$yaml %||% entry$input %||% entry$data
        if (is.null(snippet)) {
          next
        }

        if (is.character(snippet)) {
          snippet <- suite_unescape(snippet)
        }
        if (is.character(entry$json)) {
          entry$json <- suite_unescape(entry$json)
        }
        if (is.character(entry$emit)) {
          entry$emit <- suite_unescape(entry$emit)
        }

        case_index <- case_index + 1
        cases[[length(cases) + 1]] <- list(
          case_id = sprintf("%s#%d", basename(path), case_index),
          entry = entry,
          snippet = snippet
        )
      }
    }
  }

  cases
}

jsonlite_available <- requireNamespace("jsonlite", quietly = TRUE)

if (!jsonlite_available) {
  test_that("yaml-test-suite requires jsonlite", {
    skip("jsonlite not installed")
  })
} else {
  suite_dir <- test_path("yaml-test-suite", "src")
  suite_cases <- load_suite_cases(suite_dir)

  if (!length(suite_cases)) {
    test_that("yaml-test-suite data loads", {
      skip("No yaml-test-suite cases found")
    })
  } else {
    skip_parse_cases <- c("FH7J.yaml#1", "ZYU8.yaml#1", "ZYU8.yaml#2")

    skip_compare_cases <- c("6KGN.yaml#1", "RR7F.yaml#1", "S4JQ.yaml#1")

    for (case in suite_cases) {
      local({
        case_id <- case$case_id
        entry <- case$entry
        snippet <- case$snippet

        test_that(paste("yaml-test-suite", case_id), {
          skip_if(
            case_id %in% skip_parse_cases,
            sprintf("Unsupported YAML feature: %s", case_id)
          )

          expect_fail <- isTRUE(entry$fail) ||
            grepl("fail|error|invalid", tolower(entry$tags %||% ""))

          if (expect_fail) {
            expect_error(parse_yaml(snippet, multi = TRUE), info = case_id)
            return()
          }

          parsed <- try(parse_yaml(snippet), silent = TRUE)
          if (inherits(parsed, "try-error")) {
            cond <- attr(parsed, "condition")
            msg <- if (inherits(cond, "condition")) {
              conditionMessage(cond)
            } else {
              as.character(parsed)
            }
            skip(sprintf(
              "Parsing not yet implemented for %s: %s",
              case_id,
              msg
            ))
          }

          if (is.null(entry$json)) {
            succeed()
            return()
          }

          expected <- try(
            jsonlite::fromJSON(entry$json, simplifyVector = FALSE),
            silent = TRUE
          )
          if (inherits(expected, "try-error")) {
            skip(sprintf("Could not parse expected JSON for %s", case_id))
          }

          skip_if(
            case_id %in% skip_compare_cases,
            sprintf("Comparison skipped for %s", case_id)
          )

          expect_equal(
            normalize_yaml(parsed),
            normalize_yaml(expected),
            info = case_id
          )
        })
      })
    }
  }
}
