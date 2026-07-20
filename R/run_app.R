#' Launch the themescopeR Shiny application
#'
#' Opens the optional graphical interface to the ThemeScope pipeline. The app is
#' a thin front-end: it delegates all computation to the exported package
#' functions ([read_collection()], [preprocess_texts()], [themescope()],
#' [top_terms()]), so the GUI reproduces the console results exactly.
#'
#' @param display_mode Passed to [shiny::runApp()] (`"normal"` by default;
#'   `"showcase"` displays the app code alongside it).
#' @param max_upload_size_mb Optional numeric. Baseline maximum file-upload size
#'   (in megabytes) for the GUI. Shiny's own default is only 5 MB, far too small
#'   for typical ThemeScope collections, so the app already raises this to 50 MB.
#'   Set a larger value here to lift the baseline further from R (e.g. `2048` for
#'   2 GB). Users can also raise it interactively via the app's "large-file mode"
#'   toggle, which first warns about the memory implications.
#' @param ... Further arguments passed to [shiny::runApp()] (e.g. `port`,
#'   `launch.browser`).
#'
#' @return Called for its side effect of launching the app; does not return a
#'   useful value.
#'
#' @details
#' The GUI requires a few additional packages declared in `Suggests`:
#' \pkg{shiny}, \pkg{bslib}, \pkg{DT}, \pkg{plotly}, \pkg{visNetwork} and
#' \pkg{shinycssloaders}. Install any that are missing with
#' `install.packages()`.
#'
#' @examples
#' \dontrun{
#' run_themescope()
#' }
#'
#' @export
run_themescope <- function(display_mode = "normal", max_upload_size_mb = NULL, ...) {
  if (!is.null(max_upload_size_mb)) {
    if (!is.numeric(max_upload_size_mb) || length(max_upload_size_mb) != 1 ||
        is.na(max_upload_size_mb) || max_upload_size_mb <= 0) {
      cli::cli_abort("{.arg max_upload_size_mb} must be a single positive number (megabytes).")
    }
    options(shiny.maxRequestSize = max_upload_size_mb * 1024^2)
  }

  app_dir <- system.file("shiny-examples", "themescope_app", package = "themescopeR")
  if (app_dir == "") {
    cli::cli_abort(c(
      "x" = "Could not find the Shiny app directory.",
      "i" = "Try re-installing {.pkg themescopeR}."
    ))
  }

  needed  <- c("shiny", "bslib", "DT", "plotly", "visNetwork", "shinycssloaders")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "x" = "The Shiny GUI needs package{?s}: {.pkg {missing}}.",
      "i" = 'Install with {.code install.packages(c({paste0("\\"", missing, "\\"", collapse = ", ")}))}.'
    ))
  }

  shiny::runApp(app_dir, display.mode = display_mode, ...)
}
