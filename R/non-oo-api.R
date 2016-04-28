
# No coverage calculating here, since this code
# runs during install time only.
# nocov start

#' @include description.R
#' @importFrom utils packageName

generate_api <- function(member, self = TRUE, norm = TRUE,
                         invisible = FALSE) {

  res <- function() { }

  func <- description$public_methods[[member]]

  ## Arguments
  xargs <- list(file = "DESCRIPTION")
  if (self && norm) xargs <- c(xargs, list(normalize = FALSE))
  formals(res) <- c(formals(func), xargs)

  ## Call to member function
  member_call <- substitute(
    desc[[`_member`]](),
    list(`_member` = member)
  )
  argnames <- names(formals(func))
  dargs <- structure(lapply(argnames, as.name), names = argnames)
  if (!is.null(formals(func))) {
    member_call[1 + 1:length(formals(func))] <- dargs
  }
  desc_call <- substitute(
    result <- `_member`,
    list(`_member` = member_call)
  )

  ## Call to write, or just return the result
  write_call <- if (self && norm) {
    quote({
      if (normalize) desc$normalize()
      desc$write(file = file)
    })
  } else if (self) {
    quote(desc$write(file = file))
  }

  ## Put together

  body(res) <- substitute(
    { `_read`; `_trans`; `_write`; `_return` },
    list(
      `_read` = quote(desc <- description$new(file = file)),
      `_trans` = desc_call,
      `_write` = write_call,
      `_return` = if (invisible) quote(invisible(result)) else quote(result)
    )
  )

  environment(res) <- asNamespace(packageName())

  res
}

## -------------------------------------------------------------------

#' List all fields in a DESCRIPTION file
#'
#' @inheritParams desc_set
#' @return Character vector of fields.
#'
#' @family simple queries
#' @export

desc_fields <- generate_api("fields", self = FALSE)

#' Check if some fields are present in a DESCRIPTION file
#'
#' @param keys Character vector of keys to check.
#' @inheritParams desc_set
#' @return Logical vector.
#'
#' @family simple queries
#' @export

desc_has_fields <- generate_api("has_fields", self = FALSE)

#' Get a field from a DESCRIPTION file
#'
#' @param keys Character vector of fields to get.
#' @inheritParams desc_set
#' @return List, values of the specified keys. Non-existing keys
#'   return \code{NA}.
#'
#' @family simple queries
#' @export

desc_get <- generate_api("get", self = FALSE)

#' Set one or more fields in a DESCRIPTION file
#'
#' @details
#' \code{desc_set} supports two forms, the first is two unnamed
#' arguments: the key and its value to set.
#'
#' The second form requires named arguments: names are used as keys
#' and values as values to set.
#'
#' @param ... Values to set, see details below.
#' @param file DESCRIPTION file to use.
#' @param normalize Whether to normalize the write when writing back
#'   the result. See \code{\link{desc_normalize}}.
#'
#' @family simple queries
#' @export

desc_set <- generate_api("set")

#' Remove fields from a DESCRIPTION file
#'
#' @param keys Character vector of keys to remove.
#' @inheritParams desc_set
#'
#' @family simple queries
#' @export

desc_del <- generate_api("del")

## -------------------------------------------------------------------

#' Print the contents of a DESCRIPTION file to the screen
#'
#' @inheritParams desc_set
#' @export

desc_print <- generate_api("print", self = FALSE, invisible = TRUE)

#' Converts a DESCRIPTION file to LaTeX
#'
#' Returns the contents of the DESCRIPTION in LaTeX format.
#'
#' @inheritParams desc_set
#' @export

desc_to_latex <- generate_api("to_latex", self = FALSE)

#' Normalize a DESCRIPTION file
#'
#' Reformats and reorders fields in DESCRIPTION in a standard way.
#'
#' @inheritParams desc_set
#' @family repair functions
#' @export

desc_normalize <- generate_api("normalize", self = TRUE, norm = FALSE)

#' Reformat fields in a DESCRIPTION file
#'
#' Reformat the fields in DESCRIPTION in a standard way.
#'
#' @inheritParams desc_set
#' @family repair functions
#' @export

desc_reformat_fields <- generate_api("reformat_fields", self = TRUE, norm = FALSE)

#' Reorder fields in a DESCRIPTION file
#'
#' Reorder the fields in DESCRIPTION in a standard way.
#'
#' @inheritParams desc_set
#' @family repair functions
#' @export

desc_reorder_fields <- generate_api("reorder_fields", self = TRUE, norm = FALSE)

#' Validate a DESCRIPTION file
#'
#' This function is not implemented yet.
#'
#' @inheritParams desc_set
#'
#' @export

desc_validate <- generate_api("validate", self = FALSE)

## -------------------------------------------------------------------

#' List all package dependencies from a DESCRIPTION file
#'
#' @inheritParams desc_set
#' @return Data frame with columns: \code{type} (dependency type),
#'   \code{package}, and \code{version}. For non-versioned dependencies
#'   \code{version} is \code{*}.
#'
#' @family dependencies
#' @export

desc_get_deps <- generate_api("get_deps", self = FALSE)

#' Add a package dependency to a DESCRIPTION file
#'
#' @param package Package to depend on.
#' @param type Dependency type.
#' @param version Version to depend on, for versioned dependencies.
#' @inheritParams desc_set
#'
#' @family dependencies
#' @export

desc_set_dep <- generate_api("set_dep")

#' Set all package dependencies in DESCRIPTION
#'
#' @param deps Package dependency data frame, in the same format
#'    returned by \code{\link{desc_get_deps}}.
#' @inheritParams desc_set
#'
#' @family dependencies
#' @export

desc_set_deps <- generate_api("set_deps")

#' Remove a package dependency from DESCRIPTION
#'
#' @param package Package dependency to remove.
#' @param type Dependency type to remove. Sometimes a package is depended
#'   on via multiple dependency types, e.g. \code{LinkingTo} and
#'   \code{Imports}. Defaults to all types.
#' @inheritParams desc_set
#'
#' @family dependencies
#' @export

desc_del_dep <- generate_api("del_dep")

#' Remove all dependencies from DESCRIPTION
#'
#' @inheritParams desc_set
#'
#' @family dependencies
#' @export

desc_del_deps <- generate_api("del_deps")

## -------------------------------------------------------------------

#' Set the Collate field in DESCRIPTION
#'
#' @param files Collate field to set, as a character vector.
#' @param which Which collate field to use. Collate fields can
#'   be operating system type specific.
#' @inheritParams desc_set
#'
#' @family Collate field
#' @export

desc_set_collate <- generate_api("set_collate")

#' Query the Collate field in DESCRIPTION
#'
#' @inheritParams desc_set_collate
#' @return Character vector of file names.
#'
#' @family Collate field
#' @export

desc_get_collate <- generate_api("get_collate", self = FALSE)

#' Delete the Collate field from DESCRIPTION
#'
#' @inheritParams desc_set_collate
#'
#' @family Collate field
#' @export

desc_del_collate <- generate_api("del_collate")

#' Add one or more files to the Collate field, in DESCRIPTION
#'
#' @param files Character vector, files to add.
#' @inheritParams desc_set_collate
#'
#' @family Collate field
#' @export

desc_add_to_collate <- generate_api("add_to_collate")

#' Remove files from the Collate field.
#'
#' @param files Files to remove from the Collate field.
#' @inheritParams desc_set_collate
#'
#' @family Collate field
#' @export

desc_del_from_collate <- generate_api("del_from_collate")

## -------------------------------------------------------------------

#' Query all authors in Authors@R, in DESCRIPTION
#'
#' @inheritParams desc_set
#' @return A \code{\link[utils]{person}} object.
#'
#' @family Authors@R
#' @export

desc_get_authors <- generate_api("get_authors", self = FALSE)

#' Query authors by role in Authors@R, in DESCRIPTION
#'
#' @param role Role to query. Defaults to the package maintainer.
#' @inheritParams desc_set
#' @return A \code{\link[utils]{person}} object.
#'
#' @family Authors@R
#' @export

desc_get_author <- generate_api("get_author", self = FALSE)

#' Set authors in Authors@R, in DESCRIPTION
#'
#' @param authors Authors, to set, a \code{\link[utils]{person}} object.
#' @inheritParams desc_set
#'
#' @family Authors@R
#' @export

desc_set_authors <- generate_api("set_authors")

#' Add an author to Authors@R in DESCRIPTION
#'
#' @param given Given name.
#' @param family Family name.
#' @param email Email address.
#' @param role Role.
#' @param comment Comment.
#' @inheritParams desc_set
#'
#' @family Authors@R
#' @export

desc_add_author <- generate_api("add_author")

#' Add a role to one or more authors in Authors@R, in DESCRIPTION
#'
#' The author(s) can be specified by a combination of the \code{given},
#' \code{family}, \code{email} and \code{comment} fields. If multiple
#' filters are specified, then all must match to identify the author(s).
#'
#' @param role Role to add.
#' @param given Given name to filter on. Regular expression.
#' @param family Family name to filter on. Regular expression.
#' @param email Email address to filter on. Regular expression.
#' @param comment Comment field to filter on. Regular expression.
#' @inheritParams desc_set
#'
#' @family Authors@R
#' @export

desc_add_role <- generate_api("add_role")

#' Remove one or more authors from DESCRIPTION.
#'
#' It uses the Authors@R field. The author(s) to be removed
#' can be specified via any field(s). All authors matching all
#' specifications will be removed. E.g. if only \code{given = "Joe"}
#' is supplied, then all authors whole given name matches \code{Joe} will
#' be removed. The specifications can be (PCRE) regular expressions.
#'
#' @param role Role to filter on. Regular expression.
#' @inheritParams desc_add_role
#'
#' @family Authors@R
#' @export

desc_del_author <- generate_api("del_author")

#' Delete a role of an author, in DESCRIPTION
#'
#' The author(s) can be specified by a combination of the \code{given},
#' \code{family}, \code{email} and \code{comment} fields. If multiple
#' filters are specified, then all must match to identify the author(s).
#'
#' @param role Role to remove.
#' @inheritParams desc_add_role
#'
#' @family Authors@R
#' @export

desc_del_role <- generate_api("del_role")

#' Change maintainer of the package, in DESCRIPTION
#'
#' Only works with the Authors@R field.
#'
#' The current maintainer is kept if they have at least another role.
#'
#' @inheritParams desc_add_author
#'
#' @family Authors@R
#' @export

desc_change_maintainer <- generate_api("change_maintainer")

#' Add the current user as an author to DESCRIPTION
#'
#' Uses the Authors@R field.
#'
#' @param role Role to set for the user, defaults to contributor.
#' @param comment Comment, empty by default.
#' @inheritParams desc_set
#'
#' @family Authors@R
#' @export

desc_add_me <- generate_api("add_me")

#' Query the package maintainer in DESCRIPTION
#'
#' Either from the \sQuote{Maintainer} or the \sQuote{Authors@R} field.
#' @inheritParams desc_set
#' @return A character scalar.
#'
#' @family Authors@R
#' @export

desc_get_maintainer <- generate_api("get_maintainer", self = FALSE)

## -------------------------------------------------------------------

# nocov end
