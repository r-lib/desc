
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

#' @importFrom utils tar untar unzip zip

con_unzip <- function(archive, pkgname) {
  filename <-  paste0(pkgname, "/", "DESCRIPTION")
  con <- unz(archive, filename)
  on.exit(close(con), add = TRUE)
  tmp <- tempfile()
  writeLines(readLines(con), tmp)
  tmp
}

con_untar <- function(archive, pkgname) {
  filename <- paste0(pkgname, "/", "DESCRIPTION")
  tmp <- tempfile()
  suppressWarnings(
    untar(con <- gzfile(archive, open = "rb"), files = filename, exdir = tmp)
  )
  on.exit(close(con), add = TRUE)
  file.path(tmp, pkgname, "DESCRIPTION")
}

get_description_from_package <- function(file) {
  package_name <- sub("_.*$", "", basename(file))

  uncompress <- if (is_zip_file(file)) con_unzip else con_untar
  desc <- uncompress(file, package_name)

  if (!file.exists(desc)) {
    stop("Cannot extract DESCRIPTION from ", sQuote(file))
  }

  desc
}

update_metadata <- function(meta_file, private) {
  # TODO: tools:::.read_description() encodes this slightly differently than
  # idesc_as_matrix() as it does not keep whitespace for fields other than those
  # in tools:::.keep_white_description_fields.
  mat <- idesc_as_matrix(private$data)
  if ("Encoding" %in% colnames(mat)) {
    encoding <- mat[, "Encoding"]
    mat[] <- iconv(mat[], from = "UTF-8", to = encoding)
    Encoding(mat) <- encoding
  }
  db <- mat[1,]
  # Drop the Archs field, which is not present in Meta_File/packages.rds.
  db <- db[names(db) != "Archs"]

  pkg_desc <- readRDS(meta_file)
  pkg_desc$DESCRIPTION <- db
  saveRDS(pkg_desc, meta_file)
}

write_description_to_archive <- function(file, desc, private) {
  package_name <- sub("_.*$", "", basename(file))

  # Mirror the conditions under which the package archive was created so that we
  # match the path structure.
  dir.create(dir <- tempfile("desc"))
  oldwd <- getwd()
  setwd(dir)
  on.exit({
    setwd(oldwd)
    unlink(dir, recursive = TRUE)
  })

  # The easy case: zip supports replacing files individually.
  if (is_zip_file(file)) {
    dir.create(file.path(dir, package_name, "Meta"), recursive = TRUE)
    file.create(file.path(dir, package_name, "DESCRIPTION"))
    desc$write(
      file = file.path(dir, package_name, "DESCRIPTION")
    )

    # We also need to extract and modify Meta/package.rds to keep it consistent
    # with any updates to the DESCRIPTION file.
    con <- unz(file, file.path(package_name, "Meta", "package.rds"), "rb")
    on.exit(close(con), add = TRUE)
    meta_file <- file.path(dir, package_name, "Meta", "package.rds")
    writeBin(readBin(con, "raw", 1e5), meta_file)
    update_metadata(meta_file, private)

    files <- c(
      file.path(package_name, "DESCRIPTION"),
      file.path(package_name, "Meta", "package.rds")
    )
    # Note: the -r flag is the key here.
    zip(file, files = files, flags = "-r9Xq")
  } else {
    # The hard case: tar does not support replacing files, especially not with
    # the "internal" method. So we untar everything, then tar it back up again.
    suppressWarnings(
      untar(con <- gzfile(file, open = "rb"), exdir = dir)
    )
    on.exit(close(con), add = TRUE)

    desc$write(
      file = file.path(dir, package_name, "DESCRIPTION")
    )

    # As above, keep Meta/package.rds consistent with any updates to the
    # DESCRIPTION file.
    meta_file <- file.path(dir, package_name, "Meta", "packages.rds")
    if (file.exists(meta_file)) {
      update_metadata(meta_file, private)
    }

    # Match the tools/build.R invocation.
    tar(
      file, package_name, compression = "gzip", compression_level = 9L,
      tar = Sys.getenv("R_BUILD_TAR"), extra_flags = NULL
    )
  }

  invisible(desc)
}
