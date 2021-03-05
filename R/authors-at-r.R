
#' @importFrom utils as.person

parse_authors_at_r <- function(x) {

  if (is.null(x) || is.na(x)) return(NULL)

  # Need a connection on R 3.6 and before, because the encoding will
  # be messed up. Also need to set the input to `unknown`.
  Encoding(x) <- "unknown"
  con <- textConnection(x)
  on.exit(close(con), add = TRUE)
  out <- tryCatch(
    eval(parse(con, encoding = "UTF-8")),
    error = identity
  )

  if (inherits(out, "error")) NULL else out
}


deparse_authors_at_r <- function(x) {
  fmt <- lapply(unclass(x), deparse_author_at_r)
  if (length(fmt) == 1) {
    paste0("\n", paste0("    ", fmt[[1]], collapse = "\n"))
  } else {
    for (i in seq_along(fmt)) {
      fmt[[i]] <- paste0("  ", fmt[[i]])
      fmt[[i]][[length(fmt[[i]])]] <- paste0(fmt[[i]][[length(fmt[[i]])]], ",")
    }
    fmt[[1]][[1]] <- sub("^  ", "c(", fmt[[1]][[1]])
    n <- length(fmt)
    fmt[[n]][[length(fmt[[n]])]] <- sub(",$", ")", fmt[[n]][[length(fmt[[n]])]])
    paste0("\n", paste0("    ", unlist(fmt), collapse = "\n"))
  }
}

deparse_author_at_r <- function(x1) {
  x1 <- x1[! vapply(x1, is.null, TRUE)]
  paste0(
    c("person(", rep("       ", length(x1) - 1)),
    names(x1), " = ", vapply(x1, fixed_deparse1, ""),
    c(rep(",", length(x1) - 1), ")")
  )
}

set_author_field <- function(authors, which, field, value) {
  rval <- unclass(authors)
  for (w in which) rval[[w]][[field]] <- value
  class(rval) <- class(authors)
  rval
}


ensure_authors_at_r <- function(obj) {
  if (! obj$has_fields("Authors@R")) {
    stop("No 'Authors@R' field!\n",
         "You can create one with $add_author.\n",
         "You can also use $coerce_authors_at_r() to change Author fields")
  }
}

## Find an author in the Authors@R field, based on a particular
## specification. E.g. it is enough to give the first name.

search_for_author <- function(authors, given = NULL, family = NULL,
                              email = NULL, role = NULL, comment = NULL,
                              orcid = NULL) {

  matching <-
    ngrepl(given, authors$given) &
    ngrepl(family, authors$family) &
    ngrepl(email, authors$email) &
    ngrepl(role, authors$role) &
    ngrepl(comment, authors$comment) &
    ngrepl(orcid, authors$comment)

  list(index = which(matching), authors = authors[matching])
}


idesc_get_authors <- function(self, private, ensure = TRUE) {
  stopifnot(is_flag(ensure))
  if (ensure) ensure_authors_at_r(self)
  parse_authors_at_r(self$get("Authors@R"))
}


idesc_get_author <- function(self, private, role) {
  stopifnot(is_string(role))
  if (self$has_fields("Authors@R")) {
    aut <- self$get_authors()
    roles <- aut$role
    ## Broken person() API, vector for 1 author, list otherwise...
    if (!is.list(roles)) roles <- list(roles)
    selected <- vapply(roles, function(r) all(role %in% r), TRUE)
    aut[selected]
  } else {
    NULL
  }
}

idesc_set_authors <- function(self, private, authors) {
  stopifnot(is_authors(authors))
  self$set("Authors@R", deparse_authors_at_r(authors))
}

check_author_args <- function(given = NULL, family = NULL, email = NULL,
                              role = NULL, comment = NULL,
                              orcid = NULL) {
  stopifnot(
    is_string_or_null(given),
    is_string_or_null(family),
    is_string_or_null(email),
    is_character_or_null(role),
    is_named_character_or_null(comment),
    is_string_or_null(orcid)
  )
}

#' @importFrom utils person

idesc_add_author <- function(self, private, given, family, email, role,
                             comment, orcid = NULL) {
  check_author_args(given, family, email, role, comment, orcid)
  orig <- idesc_get_authors(self, private, ensure = FALSE)

  if (!is.null(orcid)) {
    comment["ORCID"] <- orcid
  }

  newp <- person(given = given, family = family, email = email,
                 role = role, comment = comment)
  new_authors <- if (is.null(orig)) newp else c(orig, newp)
  self$set_authors(new_authors)
}


idesc_add_role <- function(self, private, role, given, family, email,
                           comment, orcid = NULL) {

  stopifnot(is.character(role))
  check_author_args(given, family, email, comment = comment,
                    orcid = orcid)

  orig <- idesc_get_authors(self, private, ensure = FALSE)
  wh <- search_for_author(
    orig, given = given, family = family, email = email, comment = comment,
    orcid = orcid,
    role = NULL
  )

  for (w in wh$index) {
    orig <- set_author_field(
      orig,
      w,
      "role",
      unique(c(orig[[w]]$role, role))
    )
  }

  self$set_authors(orig)
}

idesc_add_orcid <- function(self, private, orcid, given, family, email,
                            comment, role) {

  check_author_args(given = given, family = family,
                    email = email,
                    comment = comment,
                    orcid = orcid, role = role)

  orig <- idesc_get_authors(self, private, ensure = FALSE)
  wh <- search_for_author(
    orig, given = given, family = family, email = email, comment = comment,
    orcid = NULL,
    role = role
  )

  if (length(wh$index) > 1) {
    stop("More than one author correspond to the provided arguments.
         ORCID IDs have to be distinct.",
         call. = FALSE)
  }

  orig <- set_author_field(
      orig,
      wh$index,
      "comment",
      add_orcid_to_comment(orig[wh$index]$comment,
                           orcid)
    )

  self$set_authors(orig)
}

idesc_del_author <- function(self, private, given, family, email, role,
                            comment, orcid = NULL) {

  check_author_args(given, family, email, role, comment, orcid)

  orig <- idesc_get_authors(self, private, ensure = FALSE)
  wh <- search_for_author(
    orig, given = given, family = family, email = email,
    comment = comment, orcid = orcid
  )

  if (length(wh$index) == 0) {
    desc_message("Could not find author to remove.")
  } else {
    au <- if (length(wh$index) == 1) "Author" else "Authors"
    desc_message(
      au, "removed: ",
      paste(wh$authors$given, wh$authors$family, collapse = ", "),
      "."
    )
    self$set_authors(orig[-wh$index])
  }

  invisible(self)
}


idesc_del_role <- function(self, private, role, given, family, email,
                          comment, orcid = NULL) {

  stopifnot(is.character(role))
  check_author_args(given, family, email, role = NULL, comment, orcid)

  orig <- idesc_get_authors(self, private, ensure = FALSE)
  wh <- search_for_author(
    orig, given = given, family = family, email = email, comment = comment,
    orcid = orcid, role = NULL
  )

  for (w in wh$index) {
    orig <- set_author_field(
      orig,
      w,
      "role",
      setdiff(orig[[w]]$role, role)
    )
  }

  self$set_authors(orig)
}


idesc_change_maintainer <- function(self, private, given, family, email,
                                   comment, orcid = NULL) {
  check_author_args(given, family, email, role = NULL, comment, orcid)
  ensure_authors_at_r(self)
  self$del_role(role = "cre")
  self$add_role(role = "cre", given = given, family = family,
                email = email, comment = comment, orcid = orcid)
}


#' @importFrom utils tail

idesc_add_me <- function(self, private, role, comment, orcid = NULL) {
  stopifnot(
    is_string_or_null(role),
    is_named_character_or_null(comment),
    is_string_or_null(orcid)
  )
  check_for_package("whoami", "$add_me needs the 'whoami' package")

 # guess ORCID
  if (is.null(orcid)) {
    orcid <- Sys.getenv("ORCID_ID")
    if (orcid == "") {
      orcid <- NULL
    }
  }

  fn <- parse_full_name(whoami::fullname())
  family <- fn$family
  given <- fn$given
  email <- whoami::email_address()
  self$add_author(given = given, family = family, email = email,
                  comment = comment, role = role, orcid = orcid)
}

idesc_add_author_gh <- function(self, private, username, role, comment, orcid = NULL) {
  stopifnot(
    is_string_or_null(role),
    is.character(username),
    is_named_character_or_null(comment),
    is_string_or_null(orcid)
  )
  check_for_package("gh", "$add_author_gh needs the 'gh' package")

  gh_info <- gh::gh("GET /users/:username",
                    username = username)

  fn <- parse_full_name(gh_info$name)
  family <- fn$family
  given <- fn$given
  email <- gh_info$email
  self$add_author(given = given, family = family, email = email,
                  comment = comment, role = role, orcid = orcid)
}


idesc_get_maintainer <- function(self, private) {
  if (self$has_fields("Maintainer")) {
    unname(self$get("Maintainer"))
  } else if (self$has_fields("Authors@R")) {
    format(
      self$get_author(role = "cre"),
      include = c("given", "family", "email")
    )
  } else {
    NA_character_
  }
}


idesc_coerce_authors_at_r <- function(self, private) {
  has_authors_at_r = self$has_fields("Authors@R")
  has_author = self$has_fields("Author")
  if (! (has_authors_at_r | has_author) ) {
    stop("No 'Authors@R' or 'Author' field!\n",
         "You can create one with $add_author")
  }

  if ( !has_authors_at_r & has_author) {
    # Get author field
    auth = self$get("Author")
    auth = as.person(auth)
    auth$role = "aut"

    # Get maintainer field - set creator role
    man = self$get_maintainer()
    man = as.person(man)
    man$role = c("cre")

    # Set author as maintainer
    auths = man

    # If Maintainer in Author field, remove it and keep the maintainer one
    # may want to use del_author
    check_same = function(x) {
      identical(c(man$given, man$family),
                c(x$given, x$family))
    }
    same_auth = sapply(auth, check_same)
    auth = auth[!same_auth]
    if (length(auth) > 0) {
      auths = c(auths, auth)
    }
    self$set_authors(auths)
  }
}


# helper to add or replace ORCID in comment
add_orcid_to_comment <- function(comment, orcid){

  comment["ORCID"] <- orcid
  comment
}

