
parse_urls <- function(urls) {
  str_trim(strsplit(urls, "[,\\s]+", perl = TRUE)[[1]])
}

deparse_urls <- function(urls) {
  paste(urls, collapse = ",\n    ")
}

idesc_get_urls <- function(self, private) {
  urls <- self$get("URL")
  if (is.na(urls)) {
    character()
  } else {
    parse_urls(urls)
  }
}

idesc_set_urls <- function(self, private, urls) {
  self$set(URL = deparse_urls(urls))
  invisible(self)
}

idesc_add_urls <- function(self, private, urls) {
  urls <- unique(c(self$get_urls(), urls))
  self$set(URL = deparse_urls(urls))
  invisible(self)
}

idesc_del_urls <- function(self, private, pattern) {
  urls <- self$get_urls()
  filt <- grep(pattern, urls, invert = TRUE, value = TRUE, perl = TRUE)
  if (length(filt) > 0) {
    self$set(URL = deparse_urls(filt))
  } else {
    self$del("URL")
  }
  invisible(self)
}

idesc_clear_urls <- function(self, private) {
  self$del("URL")
  invisible(self)
}
