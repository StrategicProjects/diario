# Get the task list of a specific project

This function retrieves the task list (schedule, or *cronograma*) of a
specific project by its ID. The underlying API wraps the schedule items
in a `cronograma` field alongside summary counters; this function
returns the schedule items themselves as one row per task.

## Usage

``` r
diario_get_task_list(project_id)
```

## Arguments

- project_id:

  A valid non-empty string with the project ID.

## Value

A tibble with one row per schedule item (task). Returns an empty tibble
if the project has no tasks.

## Examples

``` r
if (FALSE) { # \dontrun{
tasks <- diario_get_task_list("66cf438223aa80386306e647")
} # }
```
