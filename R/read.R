read_dcf <- function(file) {
  lines <- readLines(file)

  encoding <- sub(
    "^Encoding:\\s*",
    "",
    grep("^Encoding:", lines, value = TRUE, useBytes = TRUE),
    useBytes = TRUE
  )
  declared <- length(encoding) > 0
  if (declared) {
    Encoding(lines) <- encoding[[1]]
    lines <- enc2utf8(lines)
    con_encoding <- "UTF-8"
  } else if (all(validUTF8(lines))) {
    # No declared encoding, but the bytes are valid UTF-8 already (e.g.
    # unmarked strings from UTF-8 R source or a UTF-8 locale). Declare it
    # explicitly, since read.dcf() no longer assumes UTF-8 for unmarked
    # ("unknown") strings and otherwise mangles non-ASCII bytes.
    Encoding(lines) <- "UTF-8"
    con_encoding <- "UTF-8"
  } else {
    con_encoding <- "unknown"
  }

  con <- textConnection(lines, local = TRUE, encoding = con_encoding)
  fields <- colnames(read.dcf(con))
  close(con)

  if (!length(fields)) {
    return(list(
      dcf = create_fields(character(), character()),
      notws = character()
    ))
  }

  con <- textConnection(lines, local = TRUE, encoding = con_encoding)
  res <- read.dcf(con, keep.white = fields)
  close(con)

  if (nrow(res) > 1) {
    stop("Empty lines found in DESCRIPTION file", call. = FALSE)
  }

  con <- textConnection(lines, local = TRUE, encoding = con_encoding)
  res2 <- read.dcf(con, keep.white = fields, all = TRUE)
  close(con)

  if (any(mismatch <- res != res2)) {
    stop(
      "Duplicate DESCRIPTION fields: ",
      paste(sQuote(colnames(res)[mismatch]), collapse = ", ")
    )
  }

  if (declared) {
    # read.dcf() does not reliably propagate the UTF-8 marking of its
    # input to the parsed values across R versions, so mark explicitly.
    Encoding(res) <- "UTF-8"
    res[] <- enc2utf8(res)
  }

  no_tws_fields <- sub(
    ":$",
    "",
    grep("^[^\\s]+:$", lines, perl = TRUE, value = TRUE, useBytes = TRUE),
    useBytes = TRUE
  )

  notws <- res[1, match(no_tws_fields, fields)]

  list(
    dcf = create_fields(fields, res[1, ]),
    notws = notws
  )
}
