# Get list of projects (obras)

This function retrieves a list of projects from the Diario API.

## Usage

``` r
get_projects(query = list())
```

## Arguments

- query:

  A named list of query parameters (optional).

## Value

A tibble containing the projects data.

## Examples

``` r
if (FALSE) { # \dontrun{
projects <- get_projects(query = list(status = "active"))
} # }
```
