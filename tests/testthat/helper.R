zap_yaml_tags <- function(x) {
  attr(x, "yaml_tag") <- NULL
  if (is.list(x)) {
    x <- lapply(x, zap_yaml_tags)
  }
  x
}
