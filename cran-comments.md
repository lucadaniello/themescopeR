# cran-comments

## Test environments

* local macOS, R 4.6.0

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes on the submission

* The package downloads udpipe language models on demand from
  <https://github.com/massimoaria/tall.language.models> and caches them in
  `tools::R_user_dir("themescopeR", "cache")`. Nothing is written outside that
  cache, and no example or test needs the network: everything that would require
  a model download is wrapped in `\dontrun{}`, and the tests use small
  self-contained fixtures.
* Examples that need a corpus, a fitted analysis or an internet connection are
  wrapped in `\dontrun{}` for the same reason.
