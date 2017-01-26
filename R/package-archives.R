
is_package_archive <- function(file) {
  (is_zip_file(file) || is_tar_gz_file(file)) &&
    is_valid_package_file_name(file)
}

is_zip_file <- function(file) {
  buf <- readBin(file, what = "raw", n = 4)
  length(buf) == 4 &&
    buf[1] == 0x50 &&
    buf[2] == 0x4b &&
    (buf[3] == 0x03 || buf[3] == 0x05 || buf[5] == 0x07) &&
    (buf[4] == 0x04 || buf[4] == 0x06 || buf[4] == 0x08)
}

is_gz_file <- function(file) {
  buf <- readBin(file, what = "raw", n = 3)
  length(buf) == 3 &&
    buf[1] == 0x1f &&
    buf[2] == 0x8b &&
    buf[3] == 0x08
}

is_tar_gz_file <- function(file) {
  if (!is_gz_file(file)) return(FALSE)
  con <- gzfile(file, open = "rb")
  on.exit(close(con))
  buf <- readBin(con, what = "raw", n = 262)
  length(buf) == 262 &&
    buf[258] == 0x75 &&
    buf[259] == 0x73 &&
    buf[260] == 0x74 &&
    buf[261] == 0x61 &&
    buf[262] == 0x72
}

is_valid_package_file_name <- function(filename) {
  grepl(valid_package_archive_name, basename(filename))
}

#' @importFrom utils untar unzip

get_description_from_package <- function(file) {
  uncompress <- if (is_zip_file(file)) unzip else untar
  package_name <- sub("_.*$", "", basename(file))

  tmp <- tempfile()

  suppressWarnings(uncompress(
    file,
    files = paste(package_name, sep = "/", "DESCRIPTION"),
    exdir = tmp
  ))

  desc <- file.path(tmp, package_name, "DESCRIPTION")

  if (!file.exists(desc)) {
    stop("Cannot extract DESCRIPTION from ", sQuote(file))
  }

  desc
}
