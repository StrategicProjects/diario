# Store the API token for Diario

This function stores the provided authentication token using the
`keyring` package. If the token cannot be stored (for example, because
the keyring is not accessible), it emits a warning instead of throwing
an error, and returns `FALSE`.

## Usage

``` r
diario_store_token(token)
```

## Arguments

- token:

  A character string containing the API token to be stored.

## Value

`TRUE` (invisibly) if the token was stored successfully; `FALSE`
otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
# Attempt to store a token:
diario_store_token("your-api-token")
} # }
```
