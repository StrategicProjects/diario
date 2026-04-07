# Get the task list of a specific project

This function retrieves the task list of a specific project by its ID.

## Usage

``` r
diario_get_task_list(project_id)
```

## Arguments

- project_id:

  A valid non-empty string with the project ID.

## Value

A tibble containing the task list.

## Examples

``` r
if (FALSE) { # \dontrun{
tasks <- diario_get_task_list("66cf438223aa80386306e647")
} # }
```
