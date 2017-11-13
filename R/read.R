
## TODO: handle empty files

portable_dcf_encodings <- function() c("latin1", "latin2", "UTF-8")

read_dcf <- function(file) {
  lines <- readLines(file)

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

  res <- fix_dcf_encoding_read(res)

  lines <- enc2utf8(lines)
  no_tws_fields <- sub(
    ":$",
    "",
    grep("^[^\\s]+:$", lines, perl = TRUE, value = TRUE)
  )

  notws <- res[1, match(no_tws_fields, fields)]

  list(
    dcf = create_fields(fields, res[1, ]),
    notws = notws
  )
}
