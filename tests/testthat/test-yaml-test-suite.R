test_cases <- dirname(list.files(
  test_path("yaml-test-suite/data"),
  recursive = TRUE,
  pattern = "in.yaml$",
  full.names = TRUE
))


sort_named_lists <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  if (!is.null(nms <- names(x))) {
    # Sort by names to make map comparisons order-insensitive
    x <- x[order(nms)]
  }
  lapply(x, sort_named_lists)
}

collect_yaml_tags <- function(x) {
  tags <- character()

  collect <- function(value) {
    if (!is.null(tag <- attr(value, "yaml_tag", exact = TRUE))) {
      tags <<- c(tags, tag)
    }

    if (!is.list(value)) {
      return()
    }

    yaml_keys <- attr(value, "yaml_keys", exact = TRUE)
    if (is.list(yaml_keys)) {
      lapply(yaml_keys, collect)
    }

    lapply(value, collect)
  }

  collect(x)
  tags
}

extract_event_tags <- function(case_dir) {
  event_path <- file.path(case_dir, "test.event")
  if (!file.exists(event_path)) {
    return(character())
  }

  event_lines <- readLines(event_path, warn = FALSE)
  tokens <- strsplit(event_lines, "[[:space:]]+")
  tags <- unlist(
    lapply(tokens, function(parts) {
      parts <- parts[nzchar(parts)]
      parts[startsWith(parts, "<") & endsWith(parts, ">")]
    }),
    use.names = FALSE
  )
  trimws(gsub("^<|>$", "", tags))
}


for (case in test_cases) {
  case_id <- basename(case)
  title_path <- file.path(case, "===")
  case_title <- case_id
  if (file.exists(title_path)) {
    title_text <- trimws(paste(
      readLines(title_path, warn = FALSE),
      collapse = " "
    ))
    if (nzchar(title_text)) {
      case_title <- paste(case_id, title_text, sep = ": ")
    }
  }

  test_that(paste(case, ":", title_text), {
    if (file.exists(file.path(case, "error"))) {
      expect_error(read_yaml(file.path(case, "in.yaml"), multi = TRUE))
      return()
    }

    parsed <- expect_no_error(read_yaml(
      file.path(case, "in.yaml"),
      multi = TRUE,
      simplify = FALSE
    ))

    if (file.exists(file.path(case, "in.json"))) {
      expected <- tryCatch(
        list(jsonlite::read_json(
          file.path(case, "in.json"),
          simplifyVector = FALSE
        )),
        error = function(e) {
          docs <- list()
          lines <- character()
          con <- file(file.path(case, "in.json"), open = "r")
          on.exit(close(con))
          while (length(next_line <- readLines(con, n = 1))) {
            lines <- c(lines, next_line)
            tryCatch(
              {
                doc <- jsonlite::parse_json(
                  lines,
                  simplifyVector = FALSE
                )
                docs[length(docs) + 1L] <- list(doc)
                lines <- character()
              },
              error = function(e) NULL
            )
          }
          docs
        }
      )

      parsed_tags <- collect_yaml_tags(parsed)
      expected_tags <- extract_event_tags(case)
      allowed_core_tags <- c(
        "tag:yaml.org,2002:timestamp",
        "tag:yaml.org,2002:set",
        "tag:yaml.org,2002:omap",
        "tag:yaml.org,2002:pairs",
        "tag:yaml.org,2002:binary"
      )
      expected_non_core_tags <- expected_tags[
        !startsWith(expected_tags, "tag:yaml.org,2002:") |
          expected_tags %in% allowed_core_tags
      ]
      expect_equal(
        sort(unique(parsed_tags)),
        sort(unique(expected_non_core_tags)),
        info = paste("yaml_tag mismatch for", case_title)
      )

      parsed <- zap_yaml_tags(parsed)

      # Sort names to make map comparisons order-insensitive
      parsed <- sort_named_lists(parsed)
      expected <- sort_named_lists(expected)

      expect_equal(parsed, expected)

      return()
      if (length(waldo::compare(parsed, expected, tolerance = 0))) {
        # message("failing case: ", case)
        # withr::with_dir(case, {
        #   cat("case files: \n")
        #   print(list.files())
        #   cat("in.yaml:\n")
        #   print(readLines("in.yaml"))
        #   cat("in.json:\n")
        #   print(readLines("in.json"))
        #   browser()
        #   waldo::compare(parsed, expected)
        #   read_yaml("in.yaml")
        # })
        # fail(paste("case fails:", case))
      }
    }
  })
}
