# Get details of a specific task

This function retrieves details of a specific task by task ID within a
project.

## Usage

``` r
get_task_details(project_id, task_id)
```

## Arguments

- project_id:

  A valid non-empty string with the project ID.

- task_id:

  A valid non-empty string with the task ID.

## Value

A list containing task details.

## Examples

``` r
if (FALSE) { # \dontrun{
task <- get_task_details("12345", "67890")
} # }
```
