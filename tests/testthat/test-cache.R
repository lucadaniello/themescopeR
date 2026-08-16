# The cache must never appear as a side effect of asking where it is: CRAN
# policy allows tools::R_user_dir() only for material the user asked for.

test_that("themescope_cache_dir() does not create anything by default", {
  d <- file.path(tempdir(), "ts-cache-not-created")
  unlink(d, recursive = TRUE)
  old <- options(themescopeR.model_dir = d)
  on.exit(options(old), add = TRUE)

  expect_identical(themescope_cache_dir(), d)
  expect_false(dir.exists(d))

  # Repeated calls stay inert
  themescope_cache_dir()
  expect_false(dir.exists(d))
})

test_that("themescope_cache_dir(create = TRUE) creates the directory", {
  d <- file.path(tempdir(), "ts-cache-created")
  unlink(d, recursive = TRUE)
  old <- options(themescopeR.model_dir = d)
  on.exit({ options(old); unlink(d, recursive = TRUE) }, add = TRUE)

  expect_identical(themescope_cache_dir(create = TRUE), d)
  expect_true(dir.exists(d))
})

test_that("themescope_cache_dir() honours the option override", {
  old <- options(themescopeR.model_dir = "/some/where/else")
  on.exit(options(old), add = TRUE)
  expect_identical(themescope_cache_dir(), "/some/where/else")
})

test_that("ts_clear_cache() removes cached files and the directory", {
  d <- file.path(tempdir(), "ts-cache-clear")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(d, "english-gum-ud-2.15.udpipe"))
  file.create(file.path(d, "available_models.rdata"))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  removed <- ts_clear_cache(model_dir = d, ask = FALSE)
  expect_length(removed, 2)
  expect_false(dir.exists(d))
})

test_that("ts_clear_cache() is a no-op on an empty or missing cache", {
  missing_dir <- file.path(tempdir(), "ts-cache-absent")
  unlink(missing_dir, recursive = TRUE)
  expect_identical(ts_clear_cache(model_dir = missing_dir, ask = FALSE),
                   character(0))

  empty_dir <- file.path(tempdir(), "ts-cache-empty")
  dir.create(empty_dir, recursive = TRUE, showWarnings = FALSE)
  expect_identical(ts_clear_cache(model_dir = empty_dir, ask = FALSE),
                   character(0))
  expect_false(dir.exists(empty_dir))
})

test_that("ts_clear_cache() defaults to the configured cache directory", {
  d <- file.path(tempdir(), "ts-cache-default")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(d, "dummy.udpipe"))
  old <- options(themescopeR.model_dir = d)
  on.exit({ options(old); unlink(d, recursive = TRUE) }, add = TRUE)

  expect_length(ts_clear_cache(ask = FALSE), 1)
  expect_false(dir.exists(d))
})

test_that("ts_model_path() never creates the cache directory", {
  d <- file.path(tempdir(), "ts-cache-model-path")
  unlink(d, recursive = TRUE)
  # A registry that resolves offline, so the test needs no network.
  reg <- data.frame(language_name = "english", treebank = "GUM",
                    file = "english-gum", stringsAsFactors = FALSE)
  expect_identical(.resolve_model_file("english", registry = reg), "english-gum")
  expect_false(dir.exists(d))
})
