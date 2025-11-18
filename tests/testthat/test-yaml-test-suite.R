skip_reasons <- list(
  "6KGN" = "anchor parsed as R '' instead of NULL",
  "6XDY" = "empty document stream returned as zero vector list, not list of NULLs",
  "6ZKB" = paste(
    "document start/end markers left in content,",
    "resulting structure doesn't match expected null/doc map"
  ),
  "7FWL" = paste(
    "tagged mapping key/values lose custom tags,",
    "parsed as plain strings"
  ),
  "9DXL" = paste(
    "document markers parsed as content alongside map",
    "so docs/null separation does not match expected"
  ),
  "PUW8" = "extra document markers parsed as literal strings",
  "RR7F" = "!float tag not preserved, numeric parsed as character",
  "S4JQ" = "ambiguous numeric tags resolved differently",
  "UGM3" = "tagged document + anchors not preserved, merge flattens to plain map",
  "UT92" = "document start/end parsed as content not separate docs",
  "W4TN" = "literal block folded into string, extra docs ignored",
  "anchor-for-empty-node" = "empty anchor parsed as empty string not NULL",
  "document-start-on-last-line" = "trailing marker parsed as content string",
  "mixed-block-mapping-implicit-to-explicit" = "flow key parsed as string, tag ignored",
  "spec-example-2-27-invoice" = "custom tag lost and anchors merged to plain map",
  "spec-example-6-24-verbatim-tags" = "verbatim tags dropped on key/value",
  "spec-example-6-28-non-specific-tags" = "tag resolution differs for scalars",
  "spec-example-9-4-explicit-documents" = "document markers parsed as content not docs",
  "spec-example-9-5-directives-documents" = "directives/doc markers parsed into scalar",
  "spec-example-9-6-stream-1-3" = "document markers parsed as content alongside map",
  "spec-example-9-6-stream" = "document markers parsed as content alongside map",
  "FH7J" = "scalar value rejected as invalid YAML",
  "tags-on-empty-scalars" = "tag on empty scalar rejected by parser",
  "26DV" = paste(
    "anchors mapped to mapped key,",
    "parser resolves to merged scalar name instead of map"
  ),
  "27NA" = "directive folded into document content",
  "2AUY" = "sequence tag handling yields coerced types",
  "2EBW" = paste(
    "punctuation-heavy keys parsed as separate scalars,",
    "not preserved as literal mapping keys"
  )
)

test_cases <- dirname(list.files(
  test_path("yaml-test-suite/data"),
  recursive = TRUE,
  pattern = "in.yaml$",
  full.names = TRUE
))

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

  test_that(case_title, {
    if (!is.null(skip_reasons[[case_id]])) {
      skip(paste0(case_id, ": ", skip_reasons[[case_id]]))
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

      if (!identical(parsed, expected)) {
        message("failing case: ", case)
        withr::with_dir(case, {
          print(list.files())
          print(readLines("in.yaml"))
          print(readLines("in.json"))
        })
        fail(paste("case fails:", case))
      }
    }
  })
}
