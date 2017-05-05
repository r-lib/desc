
## TODO: handle empty files

read_dcf <- function(file) {
  lines <- readLines(file)
  no_tws_fields <- sub(
    ":$",
    "",
    grep("^[^\\s]+:$", lines, perl = TRUE, value = TRUE)
  )

  con <- textConnection(lines, local = TRUE)
  fields <- colnames(read.dcf(con))
  close(con)

  con <- textConnection(lines, local = TRUE)
  res <- read.dcf(con, keep.white = fields)
  close(con)

  con <- textConnection(lines, local = TRUE)
  res2 <- read.dcf(con, keep.white = fields, all = TRUE)
  close(con)

  if (any(mismatch <- res != res2)) {
    stop("Duplicate DESCRIPTION fields: ",
         paste(sQuote(colnames(res)[mismatch]), collapse = ", "))
  }

  notws <- res[1, match(no_tws_fields, fields)]

  list(
    dcf = create_fields(fields, res[1, ]),
    notws = notws
  )
}
