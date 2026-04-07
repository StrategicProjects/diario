# Store the API token securely for Diario

This function securely stores the authentication token using the
`keyring` package.

## Usage

``` r
store_token(token)
```

## Arguments

- token:

  A non-empty character string containing the API token.

## Value

No return value, called for side effects.

## Examples

``` r
store_token("your-api-token")
#> Token successfully stored.
```
