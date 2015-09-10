
## TODO: handle empty files

read_dcf <- function(file, ...) {
  lines <- readLines(file)

  con <- textConnection(lines, local = TRUE)
  fields <- colnames(read.dcf(con))
  close(con)

  con <- textConnection(lines, local = TRUE)
  read.dcf(con, keep.white = fields)
}
