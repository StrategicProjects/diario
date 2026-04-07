# Get details of a specific report

This function retrieves details of a specific report by report ID within
a project.

## Usage

``` r
get_report_details(project_id, report_id)
```

## Arguments

- project_id:

  A valid non-empty string with the project ID.

- report_id:

  A valid non-empty string with the report ID.

## Value

A list containing the report details.

## Examples

``` r
if (FALSE) { # \dontrun{
report <- get_report_details("12345", "67890")
} # }
```
