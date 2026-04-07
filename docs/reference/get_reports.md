# Get reports of a specific project

This function retrieves reports of a specific project with optional
parameters for limit and order.

## Usage

``` r
get_reports(project_id, limit = 50, order = "desc")
```

## Arguments

- project_id:

  A valid non-empty string with the project ID.

- limit:

  An integer specifying the maximum number of reports to retrieve.
  Default is 50.

- order:

  A character string specifying the order of the reports (e.g., "asc" or
  "desc"). Default is "desc".

## Value

A tibble containing the reports.

## Examples

``` r
if (FALSE) { # \dontrun{
reports <- get_reports("12345", limit = 10, order = "asc")
} # }
```
