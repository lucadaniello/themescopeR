#' Launch the themescopeR Shiny application
#'
#' Opens the optional graphical interface to the ThemeScope pipeline. The app is
#' a thin front-end: it delegates all computation to the exported package
#' functions ([read_collection()], [preprocess_texts()], [themescope()],
#' [top_terms()], [save_themescope()], [read_themescope()]), so the GUI
#' reproduces the console results exactly.
#'
#' @param display_mode Passed to [shiny::runApp()] (`"normal"` by default;
#'   `"showcase"` displays the app code alongside it).
#' @param max_upload_size_mb Optional numeric. Baseline maximum file-upload size
#'   (in megabytes) for the GUI. Shiny's own default is only 5 MB, far too small
#'   for typical ThemeScope collections, so the app already raises this to 50 MB.
#'   Set a larger value here to lift the baseline further from R (e.g. `2048` for
#'   2 GB). The app also raises it interactively: picking a file larger than the
#'   current limit opens a window that suggests a new limit, warns about the
#'   memory implications, and imports the file once confirmed.
#' @param ... Further arguments passed to [shiny::runApp()] (e.g. `port`,
#'   `launch.browser`).
#'
#' @return Called for its side effect of launching the app; does not return a
#'   useful value.
#'
#' @details
#' The interface follows the pipeline step by step: import (raw text, a saved
#' `.themescope` archive, or the bundled demo corpus, which ships already
#' annotated), preprocess, run, and refine the look of the network without
#' recomputing anything. Results are shown as an interactive map and network, a
#' filterable community table and per-cluster term lists, and the whole study can
#' be exported as a `.themescope` archive.
#'
#' The GUI requires a few additional packages declared in `Suggests`:
#' \pkg{shiny}, \pkg{bslib}, \pkg{DT}, \pkg{plotly}, \pkg{visNetwork} and
#' \pkg{shinycssloaders}. Install any that are missing with
#' `install.packages()`.
#'
#' @examples
#' \dontrun{
#' themescope_app()
#' }
#'
#' @export
themescope_app <- function(display_mode = "normal", max_upload_size_mb = NULL, ...) {
  if (!is.null(max_upload_size_mb)) {
    if (!is.numeric(max_upload_size_mb) || length(max_upload_size_mb) != 1 ||
        is.na(max_upload_size_mb) || max_upload_size_mb <= 0) {
      cli::cli_abort("{.arg max_upload_size_mb} must be a single positive number (megabytes).")
    }
    # CRAN policy: leave the user's options as we found them.
    old_opts <- options(shiny.maxRequestSize = max_upload_size_mb * 1024^2)
    on.exit(options(old_opts), add = TRUE)
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
