# Perform an API request to Diario

This function performs an authenticated request to the specified
endpoint of the Diario API.

## Usage

``` r
perform_request(
  endpoint,
  query = list(),
  method = "GET",
  body = NULL,
  verbosity = 0
)
```

## Arguments

- endpoint:

  A non-empty character string specifying the API endpoint.

- query:

  A named list of query parameters (optional).

- method:

  The HTTP method to use (e.g., "GET", "POST", "PUT", "DELETE"). Default
  is "GET".

- body:

  A list representing the JSON body for request methods that support a
  body (e.g., POST).

- verbosity:

  Verbosity level for the request (0 = none, 1 = minimal). Default is 0.

## Value

A list (by default) containing the response from the API. If the content
is JSON, it will be returned as an R object. If not, an error is raised.

## Examples

``` r
if (FALSE) { # \dontrun{
perform_request("v1/obras", query = list(status = "active"))
} # }
```
