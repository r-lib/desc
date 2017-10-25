
idesc_set_dep <- function(self, private, package, type, version) {
  assert_that(is_string(package), is_string(version))
  deps <- self$get_deps()
  has <- which(deps$package == package & deps$type == type)

  if (length(has)) {
    deps[ has, "version" ] <- version

  } else {
    deps <- rbind(
      deps,
      data.frame(
        stringsAsFactors = FALSE,
        type = type, package = package, version = version
      )
    )
  }

  idesc_set_deps(self, private, deps)
}


idesc_set_deps <- function(self, private, deps) {
  assert_that(is_deps_df(deps))
  depdeps <- deparse_deps(deps)
  for (d in names(depdeps)) {
    if (! same_deps(depdeps[[d]], private$data[[d]]$value)) {
      self$set(d, depdeps[[d]])
    }
  }

  deldeps <- setdiff(dep_types, names(depdeps))
  self$del(deldeps)

  invisible(self)
}


same_deps <- function(d1, d2) {
  if (is.null(d1) + is.null(d2) == 1) return(FALSE)

  d1 <- parse_deps("foo", d1)
  d2 <- parse_deps("foo", d2)

  d1 <- d1[ order(d1$type, d1$package, d1$version), ]
  d2 <- d2[ order(d2$type, d2$package, d2$version), ]
  nrow(d1) == nrow(d2) &&
    all(d1$type == d2$type) &&
    all(d1$package == d2$package) &&
    all(d1$version == d2$version)
}

#' Get dependencies
#'
#' In case the package has no dependencies at all, we `rbind` the
#' list of data frames with the various dependency types, with an
#' empty data frame. This ensures that we don't get `NULL` for the edge
#' case, but a nice data frame with zero rows.
#'
#' @param self self
#' @param private private self
#' @return data frame of dependencies
#'
#' @keywords internal
#' @noRd

idesc_get_deps <- function(self, private) {
  types <- intersect(names(private$data), dep_types)
  res <- lapply(types, function(type)
    parse_deps(type, private$data[[type]]$value))
  empty <- data.frame(
    stringsAsFactors = FALSE,
    type = character(),
    package = character(),
    version = character()
  )
  do.call(rbind, c(list(empty), res))
}


parse_deps <- function(type, deps) {
  deps <- str_trim(strsplit(deps, ",")[[1]])
  deps <- lapply(strsplit(deps, "\\("), str_trim)
  deps <- lapply(deps, sub, pattern = "\\)$", replacement = "")
  res <- data.frame(
    stringsAsFactors = FALSE,
    type = if (length(deps)) type else character(),
    package = vapply(deps, "[", "", 1),
    version = vapply(deps, "[", "", 2)
  )
  res [ is.na(res) ] <- "*"
  res
}


deparse_deps <- function(deps) {
  tapply(seq_len(nrow(deps)), deps$type, function(x) {
    pkgs <- paste0(
      "    ",
      deps$package[x],
      ifelse(
        deps$version[x] == "*",
        "",
        paste0(" (", deps$version[x], ")")
      ),
      collapse = ",\n"
    )
    paste0("\n", pkgs)
  })
}


idesc_del_dep <- function(self, private, package, type) {
  assert_that(is_string(package))
  deps <- self$get_deps()

  if (type == "all") {
    has <- which(deps$package == package)
  } else {
    has <- which(deps$package == package & deps$type == type)
  }

  if (length(has)) {
    deps <- deps[-has, ]
    idesc_set_deps(self, private, deps)

  } else {
    invisible(self)
  }
}


idesc_del_deps <- function(self, private) {
  self$del(dep_types)
}


idesc_has_dep <- function(self, private, package, type) {
  assert_that(is_string(package))

  deps <- self$get_deps()
  if (type == "any") {
    package %in% deps$package

  } else {
    package %in% deps$package &&
      type %in% deps[match(package, deps$package), "type"]
  }
}
