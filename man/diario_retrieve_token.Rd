% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diario.R
\name{diario_retrieve_token}
\alias{diario_retrieve_token}
\title{Retrieve the API token for Diario}
\usage{
diario_retrieve_token()
}
\value{
A character string containing the API token, or \code{NULL} if no valid token is found.
}
\description{
This function retrieves the stored authentication token using the \code{keyring} package.
If no valid token (or keyring) is found, it will return \code{NULL} and print a message
in English indicating that no valid token was found.
}
\examples{
token <- diario_retrieve_token()
}
