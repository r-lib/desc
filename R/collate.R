
collate_fields <- c("Collate", "Collate.unix", "Collate.windows")

parse_collate <- function(str) {
  scan(
    text = gsub("\n", " ", str),
    what = "",
    strip.white = TRUE,
    quiet = TRUE
  )
}


deparse_collate <- function(list) {
  paste0(
    "    '",
    list,
    "'",
    collapse = "\n"
  )
}
