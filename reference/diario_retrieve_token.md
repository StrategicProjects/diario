# Retrieve the API token for Diario

This function retrieves the stored authentication token using the
`keyring` package. If no valid token (or keyring) is found, it will
return `NULL` and (unless `quiet = TRUE`) emit an informational message
indicating that no valid token was found.

## Usage

``` r
diario_retrieve_token(quiet = FALSE)
```

## Arguments

- quiet:

  A logical flag. If `TRUE`, suppresses the informational message
  emitted when no token is found. Default is `FALSE`.

## Value

A character string containing the API token, or `NULL` if no valid token
is found.

## Examples

``` r
if (FALSE) { # \dontrun{
token <- diario_retrieve_token()
} # }
```
