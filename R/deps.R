
idesc_set_dep <- function(self, private, package, type, version) {
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


idesc_get_deps <- function(self, private) {
  types <- intersect(names(private$data), dep_types)
  res <- lapply(types, function(type)
    parse_deps(type, private$data[[type]]$value))
  do.call(rbind, res)
}


parse_deps <- function(type, deps) {
  deps <- str_trim(strsplit(deps, ",")[[1]])
  deps <- lapply(strsplit(deps, "\\("), str_trim)
  deps <- lapply(deps, sub, pattern = "\\)$", replacement = "")
  res <- data.frame(
    stringsAsFactors = FALSE,
    type = type,
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
