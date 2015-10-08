
desc_set_dep <- function(self, private, package, type, version) {
  deps <- self$get_deps()
  has <- which(deps$package == package & deps$type == type)

  if (length(has)) {
    deps[ has, "version" ] <- version

  } else {
    deps <- rbind(
      deps,
      c(type = type, package = package, version = version)
    )
  }

  desc_set_deps(self, private, deps)
}


desc_set_deps <- function(self, private, deps) {
  depdeps <- deparse_deps(deps)
  for (d in names(depdeps)) {
    self$set(d, depdeps[[d]])
  }

  deldeps <- setdiff(dep_types, names(depdeps))
  self$del(deldeps)

  invisible(self)
}


desc_get_deps <- function(self, private) {
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


desc_del_dep <- function(self, private, package, type) {
  deps <- self$get_deps()

  if (type == "all") {
    has <- which(deps$package == package)
  } else {
    has <- which(deps$package == package & deps$type == type)
  }

  if (length(has)) {
    deps <- deps[-has, ]
    desc_set_deps(self, private, deps)

  } else {
    invisible(self)
  }
}


desc_del_deps <- function(self, private) {
  self$del(dep_types)
}
