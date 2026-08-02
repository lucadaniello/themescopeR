# cran-comments

## Test environments

* local macOS 15, R 4.6.0
* win-builder (devel and release)
* macOS builder (R release)
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1), windows-latest,
  macOS-latest

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* The note also lists four possibly misspelled words in DESCRIPTION:
  "D'Aniello", "Misuraca" and "Spano" are the surnames of the three authors, and
  "ThemeScope" is the name of the method the package implements. All four are
  spelled correctly.

## Notes on the submission

* The package downloads udpipe language models on demand from
  <https://github.com/massimoaria/tall.language.models> and caches them under
  `tools::R_user_dir("themescopeR", "cache")`. Nothing is written anywhere else,
  and the download only happens when the user calls `ts_download_model()` or
  passes a language name to `preprocess_texts()`. No example, test or vignette
  needs the network: the few examples that would require a model download are
  wrapped in `\dontrun{}`, and everything else runs on the small annotated
  corpus bundled in `inst/extdata`.
* `themescope_app()` starts an interactive Shiny application, so its example is
  wrapped in `\dontrun{}`. It restores `options()` on exit.
* `detect_communities()` and `plot_network()` accept a `seed` argument for
  reproducibility. When one is supplied they restore the previous value of
  `.Random.seed` on exit, so the user's random stream is left untouched.
* The URL checker may report the DOI <https://doi.org/10.1177/01655515261454276>
  and the Kaggle dataset page as 403 or 404. Both are reachable from a browser;
  the publishers block automated requests. The DOI is given in its canonical
  form.
