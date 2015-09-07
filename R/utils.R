
str_trim <- function(x) {
  sub("^\\s+", "", sub("\\s+$", "", x))
}
