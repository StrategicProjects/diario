# Get the task list of a specific project

This function retrieves the task list of a specific project by its ID.

## Usage

``` r
get_task_list(project_id)
```

## Arguments

- project_id:

  A valid non-empty string with the project ID.

## Value

A tibble containing the task list.

## Examples

``` r
if (FALSE) { # \dontrun{
tasks <- get_task_list("12345")
} # }
```
