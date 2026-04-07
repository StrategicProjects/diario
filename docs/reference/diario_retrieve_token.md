# Retrieve the API token for Diario

This function retrieves the stored authentication token using the
`keyring` package. If no valid token (or keyring) is found, it will
return `NULL` and emit an informational message indicating that no valid
token was found.

## Usage

``` r
diario_retrieve_token()
```

## Value

A character string containing the API token, or `NULL` if no valid token
is found.

## Examples

``` r
if (FALSE) { # \dontrun{
token <- diario_retrieve_token()
} # }
```
