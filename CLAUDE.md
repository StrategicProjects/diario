# diario

R package: a thin wrapper around the [diariodeobras.net](https://diariodeobras.net)
("Diário de Obras") construction-log platform. It stores an API token securely and
performs authenticated requests, returning projects, tasks, reports, etc.

Published on CRAN. Maintainer: André Leite.

**Sibling:** a Python port lives at `../diariopy` (PyPI `diariopy`, repo
`StrategicProjects/diariopy`). Keep behaviour in parity when changing either. Both
READMEs cross-link.

**Current status (2026-06-26):** v0.1.2 submitted to CRAN, awaiting review (0.1.1 is
the live version). Working tree is clean.

## Layout

- `R/diario.R` — the entire implementation (all exported functions live here).
- `tests/testthat/` — `test-validation.R` (argument checks) and `test-request.R`
  (request layer, network mocked).
- `man/` — roxygen-generated `.Rd` (do not edit by hand).
- `_pkgdown.yml` + `pkgdown/` — site config; the built site is **not** committed
  (see "Website" below).
- `NEWS.md`, `cran-comments.md` — keep updated on every release.

## Conventions

- **All user-facing output goes through `cli`** (`cli_abort`, `cli_warn`,
  `cli_inform`, `cli_alert_success`, `cli_alert_warning`) — never base `stop()`,
  `warning()`, `message()`, `cat()`, or `print()`.
- **Token storage**: `keyring`, service `"DiarioAPI_Token"`, username `"global"`.
- **HTTP**: `httr2`. Central helper `diario_perform_request()` builds/auth/perform.
  - Base URL is overridable via `getOption("diario.base_url", ...)` (default
    `https://apiexterna.diariodeobra.app/`) — used to mock in tests.
  - Auth header is `token`, not `Authorization`.
- **Argument validators** must reject `NA` (note `nzchar(NA)` is `TRUE`, so always
  include an explicit `is.na()` check alongside `nzchar()`).
- Depends on R >= 4.1, so avoid base-only-in-4.4 features like `%||%`.
- The API is Portuguese: response column names and many data values come back in
  Portuguese even though function/arg names are English. Some endpoints wrap data
  in an envelope (e.g. `lista-de-tarefas` returns counters + a `cronograma` field;
  `diario_get_task_list()` unwraps `cronograma`).

## Dev workflow

```r
devtools::load_all()
roxygen2::roxygenise()                  # after changing roxygen comments
testthat::test_dir("tests/testthat")    # or devtools::test()
devtools::check(document = TRUE, args = "--as-cran")   # CRAN gate; aim for 0/0/0
devtools::build_readme()                # README.md is generated from README.Rmd
```

Tests need **no** network or token — `test-request.R` stubs the token with
`testthat::local_mocked_bindings(diario_retrieve_token = ...)` and mocks HTTP with
`httr2::with_mocked_responses()`.

## Website

pkgdown site is built and deployed by GitHub Actions
(`.github/workflows/pkgdown.yaml`) to the **`gh-pages`** branch on every push to
`main`. `docs/` is git-ignored — do not commit it. Live at
https://strategicprojects.github.io/diario/

## Release

1. Bump `Version`/`Date` in `DESCRIPTION` and the README badge.
2. Update `NEWS.md` and `cran-comments.md`.
3. `devtools::check(args = "--as-cran")` → 0/0/0.
4. Optionally `devtools::check_win_devel()`.
5. `devtools::submit_cran()`.
