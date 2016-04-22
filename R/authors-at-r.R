
#' @importFrom utils as.person

parse_authors_at_r <- function(x) {

  if (is.null(x)) return(NULL)

  out <- tryCatch(
    eval(parse(text = x)),
    error = identity
  )
  if (inherits(out, "error")) NULL else out
}


deparse_authors_at_r <- function(x) {
  fmt <- format(x, style = "R")
  paste0(paste0("    ", fmt, collapse = "\n"), "\n")
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
         "You can create one with $add_author")
  }
}


## Find an author in the Authors@R field, based on a partical
## specification. E.g. it is enough to give the first name.

search_for_author <- function(authors, given = NULL, family = NULL,
                              email = NULL, role = NULL, comment = NULL) {

  matching <-
    ngrepl(given, authors$given) &
    ngrepl(family, authors$family) &
    ngrepl(email, authors$email) &
    ngrepl(role, authors$role) &
    ngrepl(comment, authors$comment)

  list(index = which(matching), authors = authors[matching])
}


idesc_get_authors <- function(self, private, ensure = TRUE) {
  if (ensure) ensure_authors_at_r(self)
  parse_authors_at_r(self$get("Authors@R"))
}


idesc_get_author <- function(self, private, role) {
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
  self$set("Authors@R", deparse_authors_at_r(authors))
}


#' @importFrom utils person

idesc_add_author <- function(self, private, given, family, email, role,
                            comment) {
  orig <- idesc_get_authors(self, private, ensure = FALSE)
  newp <- person(given = given, family = family, email = email,
                 role = role, comment = comment)
  self$set_authors(c(orig, newp))
}


idesc_add_role <- function(self, private, role, given, family, email,
                          comment) {
  orig <- idesc_get_authors(self, private, ensure = FALSE)
  wh <- search_for_author(
    orig, given = given, family = family, email = email, comment = comment,
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


idesc_del_author <- function(self, private, given, family, email, role,
                            comment) {

  orig <- idesc_get_authors(self, private, ensure = FALSE)
  wh <- search_for_author(
    orig, given = given, family = family, email = email, comment = comment
  )

  if (length(wh$index) == 0) {
    message("Could not find author to remove.")
  } else {
    au <- if (length(wh$index) == 1) "Author" else "Authors"
    message(
      au, "removed: ",
      paste(wh$authors$given, wh$authors$family, collapse = ", "),
      "."
    )
    self$set_authors(orig[-wh$index])
  }

  invisible(self)
}


idesc_del_role <- function(self, private, role, given, family, email,
                          comment) {

  orig <- idesc_get_authors(self, private, ensure = FALSE)
  wh <- search_for_author(
    orig, given = given, family = family, email = email, comment = comment,
    role = NULL
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
                                   comment) {
  ensure_authors_at_r(self)
  self$del_role(role = "cre")
  self$add_role(role = "cre", given = given, family = family,
                email = email, comment = comment)
}


#' @importFrom utils tail

idesc_add_me <- function(self, private, role, comment) {
  check_for_package("whoami", "$add_me needs the 'whoami' package")
  fn <- strsplit(whoami::fullname(), "[ ]+")[[1]]
  family <- tail(fn, 1)
  given <- paste(fn[-length(fn)], collapse = " ")
  email <- whoami::email_address()
  self$add_author(given = given, family = family, email = email,
                  comment = comment, role = role)
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
