
## TODO: handle empty files

read_dcf <- function(file, encoding = "") {
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
  res <- re_encode(as.list(res[1, ]), encoding)
  list(
    dcf = create_fields(fields, res),
    notws = notws
  )
}

# From https://github.com/wch/r-source/blob/93b33adbcecd4a2577e1d5b873bef9a52fd734b8/src/library/utils/R/indices.R#L76
re_encode <- function(desc, encoding = "") {
  enc <- desc[["Encoding"]]
  if(!is.null(enc) && !is.na(encoding)) {
    ## Determine encoding and re-encode if necessary and possible.
    if (missing(encoding) && Sys.getlocale("LC_CTYPE") == "C")
      encoding <- "ASCII//TRANSLIT"
    if(encoding != enc) { # try to translate from 'enc' to 'encoding' --------
      ## might have an invalid encoding ...
      newdesc <- try(lapply(desc, iconv, from = enc, to = encoding))
    dOk <- function(nd) !inherits(nd, "error") && !anyNA(nd)
    ok <- dOk(newdesc)
    if(!ok) # try again
      ok <- dOk(newdesc <- try(lapply(desc, iconv, from = enc,
            to = paste0(encoding,"//TRANSLIT"))))
    if(!ok) # try again
      ok <- dOk(newdesc <- try(lapply(desc, iconv, from = enc,
            to = "ASCII//TRANSLIT", sub = "?")))
    if(ok)
      desc <- newdesc
    else
      warning("'DESCRIPTION' file has an 'Encoding' field and re-encoding is not possible", call. = FALSE)
    }
  }
  desc
}
