# Changelog

## diario 0.1.2

### Bug fixes

- Fixed a bug where query parameters were passed to
  [`httr2::req_url_query()`](https://httr2.r-lib.org/reference/req_url.html)
  as a single list argument, which errored out (“All components of `...`
  must be named”). They are now spliced in correctly, so
  [`diario_get_reports()`](https://strategicprojects.github.io/diario/reference/diario_get_reports.md)’s
  `limit`/`order` arguments work as documented.

- All argument validators now reject missing values (`NA`). Previously
  `nzchar(NA)` returned `TRUE`, so an `NA` token could be passed
  straight to `keyring`, and `NA` ids could be pasted into request URLs.

### Potentially breaking changes

- [`diario_get_task_list()`](https://strategicprojects.github.io/diario/reference/diario_get_task_list.md)
  now returns the schedule items (one row per task, with `_id`,
  `descricao`, etc.) instead of the raw API envelope. Previously the
  summary counters were recycled across rows and the actual tasks were
  hidden inside a nested `cronograma` column.

### Improvements

- [`diario_perform_request()`](https://strategicprojects.github.io/diario/reference/diario_perform_request.md)
  now sends the HTTP method in upper case, matching the validation, so
  lower-case input (e.g. `"get"`) no longer leaks through.

- The API base URL is now configurable via the `diario.base_url` option,
  making it possible to target staging environments or mock requests in
  tests.

- HTTP errors now surface the message returned by the Diario API (via
  [`httr2::req_error()`](https://httr2.r-lib.org/reference/req_error.html)),
  and empty response bodies (e.g. `204 No Content` from `DELETE`) are
  handled gracefully instead of raising a content-type error.

- [`diario_get_reports()`](https://strategicprojects.github.io/diario/reference/diario_get_reports.md)
  now validates that `order` is one of `"asc"` or `"desc"` up front.

- [`diario_retrieve_token()`](https://strategicprojects.github.io/diario/reference/diario_retrieve_token.md)
  gained a `quiet` argument;
  [`diario_perform_request()`](https://strategicprojects.github.io/diario/reference/diario_perform_request.md)
  uses it to avoid emitting a duplicate “No valid token found” message.

- Added a `testthat` test suite covering argument validation and the
  request layer (network mocked with
  [`httr2::with_mocked_responses()`](https://httr2.r-lib.org/reference/with_mocked_responses.html)).

## diario 0.1.1

CRAN release: 2026-04-07

### Improvements

- Migrated all user-facing messages, warnings, and errors to use the
  `cli` package
  ([`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html),
  [`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html),
  [`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html),
  [`cli::cli_alert_success()`](https://cli.r-lib.org/reference/cli_alert.html),
  [`cli::cli_alert_warning()`](https://cli.r-lib.org/reference/cli_alert.html)),
  replacing base R [`stop()`](https://rdrr.io/r/base/stop.html) and
  [`message()`](https://rdrr.io/r/base/message.html) calls. This
  provides richer, more informative output with inline markup for
  arguments, values, functions, and packages.

- Fixed a bug in
  [`diario_get_reports()`](https://strategicprojects.github.io/diario/reference/diario_get_reports.md)
  where query parameters (`limite`, `ordem`) were constructed but never
  passed to
  [`diario_perform_request()`](https://strategicprojects.github.io/diario/reference/diario_perform_request.md).

- Fixed incorrect example in
  [`diario_get_projects()`](https://strategicprojects.github.io/diario/reference/diario_get_projects.md)
  documentation that referenced a `query` parameter not accepted by the
  function.

- Wrapped
  [`diario_store_token()`](https://strategicprojects.github.io/diario/reference/diario_store_token.md)
  and
  [`diario_retrieve_token()`](https://strategicprojects.github.io/diario/reference/diario_retrieve_token.md)
  examples in `\dontrun{}` to avoid CRAN check failures on systems
  without keyring support.

- Removed `LazyData: true` from DESCRIPTION (no `data/` directory
  exists).

- Added `BugReports` field to DESCRIPTION.

- Updated authors list.

## diario 0.1.0

CRAN release: 2025-01-15

- Initial CRAN release.
- Token management via `keyring`
  ([`diario_store_token()`](https://strategicprojects.github.io/diario/reference/diario_store_token.md),
  [`diario_retrieve_token()`](https://strategicprojects.github.io/diario/reference/diario_retrieve_token.md)).
- Authenticated API requests with `httr2`
  ([`diario_perform_request()`](https://strategicprojects.github.io/diario/reference/diario_perform_request.md)).
- Convenience wrappers:
  [`diario_get_company()`](https://strategicprojects.github.io/diario/reference/diario_get_company.md),
  [`diario_get_entities()`](https://strategicprojects.github.io/diario/reference/diario_get_entities.md),
  [`diario_get_projects()`](https://strategicprojects.github.io/diario/reference/diario_get_projects.md),
  [`diario_get_project_details()`](https://strategicprojects.github.io/diario/reference/diario_get_project_details.md),
  [`diario_get_task_list()`](https://strategicprojects.github.io/diario/reference/diario_get_task_list.md),
  [`diario_get_task_details()`](https://strategicprojects.github.io/diario/reference/diario_get_task_details.md),
  [`diario_get_reports()`](https://strategicprojects.github.io/diario/reference/diario_get_reports.md),
  [`diario_get_report_details()`](https://strategicprojects.github.io/diario/reference/diario_get_report_details.md).
