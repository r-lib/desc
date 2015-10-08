
## TODO: handle empty files

read_dcf <- function(file) {
  lines <- readLines(file)

  con <- textConnection(lines, local = TRUE)
  fields <- colnames(read.dcf(con))
  close(con)

  con <- textConnection(lines, local = TRUE)
  res <- read.dcf(con, keep.white = fields)
  close(con)

  create_fields(fields, res[1, ])
}
