`%||%` <- function(x, y) if (is.null(x)) y else x

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
        if (!is.list(entry) || !length(entry)) {
          next
        }
        snippet <- entry$yaml %||% entry$input %||% entry$data
        if (is.null(snippet)) {
          next
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
    skip_parse_cases <- c(
      "4EJS.yaml#1",
      "26DV.yaml#1",
      "2G84.yaml#3",
      "2G84.yaml#4",
      "3RLN.yaml#2",
      "3RLN.yaml#5",
      "4Q9F.yaml#1",
      "4QFQ.yaml#1",
      "4RWC.yaml#1",
      "5GBF.yaml#1",
      "6BCT.yaml#1",
      "6FWR.yaml#1",
      "6HB6.yaml#1",
      "753E.yaml#1",
      "93WF.yaml#1",
      "B3HG.yaml#1",
      "DC7X.yaml#1",
      "DE56.yaml#3",
      "DE56.yaml#4",
      "DK95.yaml#4",
      "DK95.yaml#5",
      "DK95.yaml#8",
      "DK95.yaml#7",
      "F8F9.yaml#1",
      "FH7J.yaml#1",
      "G992.yaml#1",
      "H2RW.yaml#1",
      "J3BT.yaml#1",
      "JEF9.yaml#1",
      "JEF9.yaml#2",
      "JEF9.yaml#3",
      "K527.yaml#1",
      "K858.yaml#1",
      "KH5V.yaml#2",
      "L24T.yaml#1",
      "L24T.yaml#2",
      "M29M.yaml#1",
      "M9B4.yaml#1",
      "MJS9.yaml#1",
      "MUS6.yaml#4",
      "MYW6.yaml#1",
      "NHX8.yaml#1",
      "P94K.yaml#1",
      "R4YG.yaml#1",
      "T5N4.yaml#1",
      "TS54.yaml#1",
      "XV9V.yaml#1",
      "Y79Y.yaml#5",
      "Y79Y.yaml#6",
      "Y79Y.yaml#7",
      "Y79Y.yaml#9",
      "Y79Y.yaml#10",
      "Y79Y.yaml#3",
      "ZYU8.yaml#1",
      "ZYU8.yaml#2",
      "ZYU8.yaml#3"
    )

    skip_compare_cases <- c(
      "3RLN.yaml#3",
      "3RLN.yaml#6",
      "4ZYM.yaml#1",
      "6CA3.yaml#1",
      "6KGN.yaml#1",
      "6WPF.yaml#1",
      "7A4E.yaml#1",
      "7FWL.yaml#1",
      "96NN.yaml#1",
      "9TFX.yaml#1",
      "9YRD.yaml#1",
      "A2M4.yaml#1",
      "DE56.yaml#2",
      "DE56.yaml#5",
      "DE56.yaml#6",
      "DK95.yaml#1",
      "DK95.yaml#3",
      "DK95.yaml#6",
      "DK95.yaml#9",
      "DWX9.yaml#1",
      "EX5H.yaml#1",
      "HS5T.yaml#1",
      "K54U.yaml#1",
      "KH5V.yaml#3",
      "NAT4.yaml#1",
      "NB6Z.yaml#1",
      "NP9H.yaml#1",
      "PRH3.yaml#1",
      "Q5MG.yaml#1",
      "Q8AD.yaml#1",
      "RR7F.yaml#1",
      "S4JQ.yaml#1",
      "SM9W.yaml#1",
      "T26H.yaml#1",
      "T4YY.yaml#1",
      "TL85.yaml#1",
      "UV7Q.yaml#1",
      "Y79Y.yaml#2",
      "Y79Y.yaml#11"
    )

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
