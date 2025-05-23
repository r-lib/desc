% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/non-oo-api.R
\name{desc_has_fields}
\alias{desc_has_fields}
\title{Check if some fields are present in a DESCRIPTION file}
\usage{
desc_has_fields(keys, file = ".")
}
\arguments{
\item{keys}{Character vector of keys to check.}

\item{file}{DESCRIPTION file to use. By default the DESCRIPTION
file of the current package (i.e. the package the working directory
is part of) is used.}
}
\value{
Logical vector.
}
\description{
Check if some fields are present in a DESCRIPTION file
}
\seealso{
Other simple queries: 
\code{\link{desc_del}()},
\code{\link{desc_fields}()},
\code{\link{desc_get}()},
\code{\link{desc_get_field}()},
\code{\link{desc_set}()}
}
\concept{simple queries}
