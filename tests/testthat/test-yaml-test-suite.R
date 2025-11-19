test_cases <- dirname(list.files(
  test_path("yaml-test-suite/data"),
  recursive = TRUE,
  pattern = "in.yaml$",
  full.names = TRUE
))

# unlink("skip-cases.txt")

skip_cases <- c(
  "name/tags-on-empty-scalars",
  "name/spec-example-6-24-verbatim-tags",
  NULL
)

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
    if (any(endsWith(case, skip_cases))) {
      skip(case)
    }
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

      # TODO: some of these don't make a whole lot of sense...
      # attr(,"yaml_tag")
      # [1] "!!"
      parsed <- zap_yaml_tags(parsed)

      # Sort names to make map comparisons order-insensitive
      parsed <- sort_named_lists(parsed)
      expected <- sort_named_lists(expected)

      expect_equal(parsed, expected)

      return()
      if (length(waldo::compare(parsed, expected, tolerance = 0))) {
        # browser()
        # cat(case, "\n", sep = "", file = "skip-cases.txt", append = TRUE)
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
# readLines("skip-cases.txt") |> TKutils::dput_cb()
