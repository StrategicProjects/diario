# Get details of a specific report

This function retrieves details of a specific report by report ID within
a project.

## Usage

``` r
diario_get_report_details(project_id, report_id)
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
report <- diario_get_report_details("6717f864d163f517ae06e242", "67648080f0971de9d00324c2")
} # }
```
