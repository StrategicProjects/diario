# Changelog

## diario 0.1.1

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
  [`diario_get_reports()`](https://monitoramento.sepe.pe.gov.br/diario/reference/diario_get_reports.md)
  where query parameters (`limite`, `ordem`) were constructed but never
  passed to
  [`diario_perform_request()`](https://monitoramento.sepe.pe.gov.br/diario/reference/diario_perform_request.md).

- Fixed incorrect example in
  [`diario_get_projects()`](https://monitoramento.sepe.pe.gov.br/diario/reference/diario_get_projects.md)
  documentation that referenced a `query` parameter not accepted by the
  function.

- Wrapped
  [`diario_store_token()`](https://monitoramento.sepe.pe.gov.br/diario/reference/diario_store_token.md)
  and
  [`diario_retrieve_token()`](https://monitoramento.sepe.pe.gov.br/diario/reference/diario_retrieve_token.md)
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
  ([`diario_store_token()`](https://monitoramento.sepe.pe.gov.br/diario/reference/diario_store_token.md),
  [`diario_retrieve_token()`](https://monitoramento.sepe.pe.gov.br/diario/reference/diario_retrieve_token.md)).
- Authenticated API requests with `httr2`
  ([`diario_perform_request()`](https://monitoramento.sepe.pe.gov.br/diario/reference/diario_perform_request.md)).
- Convenience wrappers:
  [`diario_get_company()`](https://monitoramento.sepe.pe.gov.br/diario/reference/diario_get_company.md),
  [`diario_get_entities()`](https://monitoramento.sepe.pe.gov.br/diario/reference/diario_get_entities.md),
  [`diario_get_projects()`](https://monitoramento.sepe.pe.gov.br/diario/reference/diario_get_projects.md),
  [`diario_get_project_details()`](https://monitoramento.sepe.pe.gov.br/diario/reference/diario_get_project_details.md),
  [`diario_get_task_list()`](https://monitoramento.sepe.pe.gov.br/diario/reference/diario_get_task_list.md),
  [`diario_get_task_details()`](https://monitoramento.sepe.pe.gov.br/diario/reference/diario_get_task_details.md),
  [`diario_get_reports()`](https://monitoramento.sepe.pe.gov.br/diario/reference/diario_get_reports.md),
  [`diario_get_report_details()`](https://monitoramento.sepe.pe.gov.br/diario/reference/diario_get_report_details.md).
