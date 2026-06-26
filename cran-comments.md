## Submission

This is a minor update of the 'diario' package (0.1.1 -> 0.1.2).

### Changes in this version

* Fixed a bug where query parameters were not passed correctly to
  `httr2::req_url_query()`, so `diario_get_reports()`'s `limit`/`order`
  arguments now work as documented.
* Argument validators now reject missing values (`NA`).
* `diario_get_task_list()` now returns the schedule items (one row per task)
  instead of the raw API envelope.
* HTTP errors now surface the message returned by the API, and empty response
  bodies are handled gracefully.
* The API base URL is now configurable via the `diario.base_url` option.
* Added a `testthat` test suite (network mocked, no internet access required).

See `NEWS.md` for the full list of changes.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local macOS, R 4.6.0, R CMD check --as-cran: 0 errors | 0 warnings | 0 notes

## Notes

* Examples that require a valid authentication token and the external Diario
  service are wrapped in `\dontrun{}`. The bundled `testthat` tests mock all
  network access and do not contact the external API, so they run without
  internet or credentials.
