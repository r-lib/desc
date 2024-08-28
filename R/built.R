
idesc_get_built <- function(self, private) {
  built <- gsub("\\n", " ", unname(self$get("Built")))
  if (is.na(built)) stop("No ", sQuote('Built'), " field found")
  built <- as.list(trimws(strsplit(built, "; ")[[1L]]))
  if (length(built) != 4L) {
    stop(sQuote('Built'), " field is corrupted")
  }
  names(built) <- c("R", "Platform", "Date", "OStype")
  built[["R"]] <- R_system_version(sub("^R ([0-9.]+)", "\\1", built[["R"]]))
  built
}
