# The `.themescope` archive: a self-contained record of one analysis, so that a
# finished study can be reopened (in the console or in the GUI) without
# re-annotating the corpus and without re-running the pipeline.

.themescope_format  <- "themescope"
.themescope_version <- 1L

# Internal: pretty-print a value for the summary table.
.fmt_val <- function(x, digits = 3) {
  if (is.null(x) || length(x) == 0) return("not recorded")
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  if (is.numeric(x)) {
    x <- ifelse(is.na(x), NA, ifelse(x == round(x), format(x, big.mark = ","),
                                     format(round(x, digits), nsmall = 0)))
  }
  x <- as.character(x)
  x[is.na(x)] <- "NA"
  paste(x, collapse = ", ")
}


#' Save an analysis as a `.themescope` archive
#'
#' @description
#' Writes a finished analysis, together with the data it was computed from, to a
#' single `.themescope` file. The archive is a compressed RDS holding the
#' `themescope` object, the annotated words it was built on, the raw collection
#' (optional), the term-level table with the scores used to draw the maps, the
#' community-level table, and the parameters of the run.
#'
#' Reopening the archive with [read_themescope()] restores maps, communities and
#' top terms without re-annotating the corpus or re-running the pipeline.
#'
#' @param x A `themescope` object, as returned by [themescope()].
#' @param file Destination path. The extension `.themescope` is added when
#'   `file` has none.
#' @param words Optional annotated words data frame (the [preprocess_texts()]
#'   output the analysis was run on). Storing it lets the archive be re-analysed
#'   with different parameters.
#' @param collection Optional raw document collection (the [read_collection()]
#'   output), stored so the original texts travel with the results.
#' @param meta Optional named list of extra information to record, for example
#'   `list(language = "english", treebank = "GUM", lexicon = "Brysbaert")`.
#'   Whatever is supplied is shown by [summary()] and by the import window of
#'   the Shiny GUI.
#' @param compress Compression passed to [saveRDS()] (default `"xz"`).
#'
#' @return The path of the written file, invisibly.
#'
#' @seealso [read_themescope()] to load an archive back.
#'
#' @examples
#' \dontrun{
#' result <- themescope(tokens, seed = 1)
#' save_themescope(result, "climate_study.themescope",
#'                 words = tokens, collection = coll,
#'                 meta = list(language = "english", lexicon = "Brysbaert"))
#' }
#'
#' @export
save_themescope <- function(x, file, words = NULL, collection = NULL,
                            meta = list(), compress = "xz") {
  if (!inherits(x, "themescope")) {
    cli::cli_abort("{.arg x} must be a {.cls themescope} object.")
  }
  if (!is.character(file) || length(file) != 1) {
    cli::cli_abort("{.arg file} must be a single path.")
  }
  if (!is.list(meta)) {
    cli::cli_abort("{.arg meta} must be a list.")
  }
  if (!nzchar(tools::file_ext(file))) file <- paste0(file, ".themescope")
  if (!is.null(words) && !is.data.frame(words)) {
    cli::cli_abort("{.arg words} must be a data frame or {.code NULL}.")
  }
  if (!is.null(collection) && !is.data.frame(collection)) {
    cli::cli_abort("{.arg collection} must be a data frame or {.code NULL}.")
  }

  # Term-level scores: exactly the table behind the maps and the top-term lists.
  terms <- term_relevance(x$graph, x$membership, x$presence)

  archive <- structure(
    list(
      format          = .themescope_format,
      version         = .themescope_version,
      created         = Sys.time(),
      package_version = as.character(utils::packageVersion("themescopeR")),
      result          = x,
      words           = words,
      collection      = collection,
      terms           = terms,
      communities     = as.data.frame(x),
      meta            = meta
    ),
    class = "themescope_archive"
  )

  saveRDS(archive, file, compress = compress, version = 3)
  invisible(file)
}


#' Read a `.themescope` archive
#'
#' Loads an analysis written by [save_themescope()]. Use [summary()] on the
#' result for a table describing what the file contains, and `archive$result`
#' for the `themescope` object itself (which plots, prints and coerces to a data
#' frame as usual).
#'
#' @param file Path to a `.themescope` file.
#'
#' @return An object of class `themescope_archive`: a list with `result` (the
#'   `themescope` object), `words`, `collection`, `terms`, `communities`,
#'   `meta`, `created`, `package_version` and `version`.
#'
#' @seealso [save_themescope()].
#'
#' @examples
#' \dontrun{
#' archive <- read_themescope("climate_study.themescope")
#' summary(archive)
#' plot(archive$result, type = "map", label = "terms")
#' }
#'
#' @export
read_themescope <- function(file) {
  if (!is.character(file) || length(file) != 1) {
    cli::cli_abort("{.arg file} must be a single path.")
  }
  if (!file.exists(file)) {
    cli::cli_abort("File not found: {.path {file}}.")
  }
  obj <- tryCatch(readRDS(file), error = function(e) NULL)
  if (is.null(obj) || !is.list(obj) ||
      !identical(obj$format, .themescope_format)) {
    cli::cli_abort(c(
      "x" = "{.path {basename(file)}} is not a themescope archive.",
      "i" = "Archives are written by {.fn save_themescope} and usually end in {.val .themescope}."
    ))
  }
  if (!inherits(obj$result, "themescope")) {
    cli::cli_abort("The archive does not contain a {.cls themescope} object.")
  }
  if (isTRUE(obj$version > .themescope_version)) {
    cli::cli_warn(c(
      "!" = "The archive was written by a newer version of {.pkg themescopeR} (format {obj$version}).",
      "i" = "Update the package if anything looks missing."
    ))
  }
  structure(obj, class = "themescope_archive")
}


#' @describeIn read_themescope Print a one-screen overview of the archive.
#' @param x A `themescope_archive` object.
#' @param ... Ignored.
#' @export
print.themescope_archive <- function(x, ...) {
  cli::cli_h1("ThemeScope archive")
  info <- summary(x)
  for (k in seq_len(nrow(info))) {
    cli::cli_bullets(stats::setNames(
      paste0(info$field[k], ": ", info$value[k]), "*"))
  }
  invisible(x)
}


#' @describeIn read_themescope Describe the archive as a two-column data frame
#'   (`field`, `value`), covering the corpus, the parameters of the run, the
#'   vocabulary, the network and the lexicon. This is what the Shiny import
#'   window displays.
#' @param object A `themescope_archive` object.
#' @export
summary.themescope_archive <- function(object, ...) {
  res <- object$result
  p   <- res$params
  gs  <- res$network_stats$global_stats
  m   <- object$meta %||% list()

  n_docs <- if (!is.null(object$collection)) nrow(object$collection)
            else if (!is.null(object$words)) length(unique(object$words$doc_id))
            else NULL
  n_sent <- if (!is.null(object$words)) {
    length(unique(paste(object$words$doc_id, object$words$sentence_id)))
  } else NULL

  algo <- p$community_algorithm
  algo_detail <- if (identical(algo, "walktrap")) {
    paste0(algo, " (", p$walktrap_steps, " steps)")
  } else {
    paste0(algo, " (resolution ", p$resolution, ")")
  }

  fields <- list(
    c("Created",              format(object$created, "%Y-%m-%d %H:%M")),
    c("Written by",           paste0("themescopeR ", object$package_version)),
    c("Corpus source",        .fmt_val(m$source)),
    c("Documents",            .fmt_val(n_docs)),
    c("Sentences",            .fmt_val(n_sent)),
    c("Annotated tokens",     .fmt_val(if (!is.null(object$words)) nrow(object$words))),
    c("Annotation language",  .fmt_val(m$language)),
    c("Model (treebank)",     .fmt_val(m$treebank)),
    c("Analysis unit",        .fmt_val(p$unit)),
    c("Included word classes", .fmt_val(p$pos_filter)),
    c("Vocabulary size",      .fmt_val(p$vocab_size %||% "all terms")),
    c("Terms in the network", .fmt_val(gs$n_nodes)),
    c("Co-occurrence window", .fmt_val(p$window)),
    c("Similarity measure",   .fmt_val(p$normalization)),
    c("Edge threshold",       .fmt_val(p$threshold_percentile)),
    c("Edges kept",           .fmt_val(gs$n_edges)),
    c("Community detection",  algo_detail),
    c("Min community size",   .fmt_val(p$min_community_size)),
    c("Random seed",          .fmt_val(p$seed)),
    c("Communities",          .fmt_val(gs$n_communities)),
    c("Modularity",           .fmt_val(gs$modularity)),
    c("Concreteness lexicon", .fmt_val(m$lexicon)),
    c("Lexicon coverage",     if (is.null(m$lexicon_coverage)) "not recorded"
                              else paste0(round(100 * as.numeric(m$lexicon_coverage)),
                                          "% of network terms")),
    c("Scored terms stored",  .fmt_val(nrow(object$terms))),
    c("Terms ranked by",      .fmt_val(m$label_by)),
    c("Raw texts stored",     if (is.null(object$collection)) "no" else "yes")
  )

  out <- data.frame(
    field = vapply(fields, `[`, character(1), 1),
    value = vapply(fields, `[`, character(1), 2),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}
