# Source of the updated UDPipe models / lexicons maintained for TALL.
# See https://github.com/massimoaria/tall.language.models
.ts_models_version <- "2.15"
.ts_models_base_url <- "https://github.com/massimoaria/tall.language.models/raw/main/2.15/"
.ts_registry_url <- "https://github.com/massimoaria/tall.language.models/raw/main/data/available_models.rdata"


#' Location of the themescopeR model cache
#'
#' Returns (and creates, if needed) the directory where downloaded \pkg{udpipe}
#' language models are cached, so they are downloaded only once. Defaults to
#' `tools::R_user_dir("themescopeR", "cache")`; override with
#' `options(themescopeR.model_dir = "/path")`.
#'
#' @return A character path to the cache directory.
#'
#' @examples
#' \dontrun{
#' themescope_cache_dir()
#' }
#'
#' @export
themescope_cache_dir <- function() {
  dir <- getOption("themescopeR.model_dir",
                   tools::R_user_dir("themescopeR", which = "cache"))
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}


#' List the available updated language models
#'
#' Retrieves the registry of updated \pkg{udpipe} models (Universal Dependencies
#' 2.15) maintained for TALL. The registry is cached in
#' [themescope_cache_dir()] and reused on later calls.
#'
#' @param refresh Logical. Force a re-download of the registry (default `FALSE`).
#'
#' @return A data frame with one row per model, including `language_name`,
#'   `treebank` and `file` (the model code used to build the file name).
#'
#' @examples
#' \dontrun{
#' models <- ts_list_models()
#' subset(models, language_name == "english")[, c("language_name", "treebank", "file")]
#' }
#'
#' @export
ts_list_models <- function(refresh = FALSE) {
  cache <- file.path(themescope_cache_dir(), "available_models.rdata")
  if (refresh || !file.exists(cache)) {
    ok <- tryCatch({
      utils::download.file(.ts_registry_url, cache, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) FALSE)
    if (!ok && !file.exists(cache)) {
      cli::cli_abort(c(
        "x" = "Could not download the model registry.",
        "i" = "Check your internet connection or pass a model file directly to {.fn preprocess_texts}."
      ))
    }
  }
  e <- new.env(parent = emptyenv())
  load(cache, envir = e)
  df <- mget(ls(e), envir = e)
  df <- df[[which(vapply(df, is.data.frame, logical(1)))[1]]]
  as.data.frame(df, stringsAsFactors = FALSE)
}

# Internal: resolve a (language, treebank) request to a model file code.
.resolve_model_file <- function(language, treebank = NULL, registry = NULL) {
  if (is.null(registry)) registry <- ts_list_models()
  rows <- registry[tolower(registry$language_name) == tolower(language), , drop = FALSE]
  if (nrow(rows) == 0) {
    cli::cli_abort(c(
      "x" = "No model found for language {.val {language}}.",
      "i" = "See {.run themescopeR::ts_list_models()} for available languages."
    ))
  }
  if (!is.null(treebank)) {
    tb <- rows[tolower(rows$treebank) == tolower(treebank), , drop = FALSE]
    if (nrow(tb) == 0) {
      cli::cli_abort(c(
        "x" = "No treebank {.val {treebank}} for language {.val {language}}.",
        "i" = "Available: {.val {rows$treebank}}."
      ))
    }
    rows <- tb
  }
  rows$file[1]
}


#' Download and cache an updated udpipe language model
#'
#' Downloads a Universal Dependencies 2.15 \pkg{udpipe} model from the TALL
#' language-model collection into [themescope_cache_dir()]. If the model is
#' already cached it is **not** re-downloaded, so models are fetched only once
#' and reused across analyses.
#'
#' @param language Character language name (e.g. `"english"`, `"italian"`). See
#'   [ts_list_models()].
#' @param treebank Optional treebank name (e.g. `"EWT"`, `"ISDT"`). If `NULL`,
#'   the first model listed for the language is used.
#' @param model_dir Cache directory (default [themescope_cache_dir()]).
#' @param overwrite Logical. Re-download even if cached (default `FALSE`).
#'
#' @return The path to the cached `.udpipe` model file (invisibly).
#'
#' @examples
#' \dontrun{
#' path <- ts_download_model("english")
#' path <- ts_download_model("italian", treebank = "ISDT")
#' }
#'
#' @export
ts_download_model <- function(language,
                              treebank  = NULL,
                              model_dir = themescope_cache_dir(),
                              overwrite = FALSE) {
  file_code <- .resolve_model_file(language, treebank)
  filename  <- paste0(file_code, "-ud-", .ts_models_version, ".udpipe")
  dest      <- file.path(model_dir, filename)

  if (file.exists(dest) && !overwrite) {
    themescope_progress("Using cached model: {.file {filename}}", verbose = TRUE)
    return(invisible(dest))
  }

  url <- paste0(.ts_models_base_url, filename)
  themescope_progress("Downloading model {.file {filename}} ...", verbose = TRUE)
  utils::download.file(url, dest, mode = "wb", quiet = TRUE)
  invisible(dest)
}


#' Resolve the cached path of an updated udpipe model
#'
#' Returns the path to a cached model without downloading it.
#'
#' @inheritParams ts_download_model
#'
#' @return The path to the cached `.udpipe` file, or `NA` if it is not cached.
#'
#' @examples
#' \dontrun{
#' ts_model_path("english")
#' }
#'
#' @export
ts_model_path <- function(language, treebank = NULL, model_dir = themescope_cache_dir()) {
  file_code <- .resolve_model_file(language, treebank)
  filename  <- paste0(file_code, "-ud-", .ts_models_version, ".udpipe")
  dest      <- file.path(model_dir, filename)
  if (file.exists(dest)) dest else NA_character_
}

# Internal: turn `model` (object | path | language name) into a loaded model.
.load_model <- function(model, verbose = TRUE) {
  if (inherits(model, "udpipe_model")) return(model)
  if (is.character(model) && length(model) == 1) {
    if (file.exists(model)) {
      themescope_progress("Loading udpipe model from {.path {model}} ...", verbose)
      return(udpipe::udpipe_load_model(model))
    }
    # Treat as a language name: download/cache then load.
    path <- ts_download_model(model)
    return(udpipe::udpipe_load_model(path))
  }
  cli::cli_abort("{.arg model} must be a udpipe model object, a path to a .udpipe file, or a language name.")
}


#' Annotate a document collection with udpipe
#'
#' Tokenises, lemmatises and POS-tags a collection of raw documents using a
#' \pkg{udpipe} language model, returning a tokens data frame ready for
#' [themescope()] and the rest of the backend.
#'
#' @param collection Data frame with a text column and a document-id column,
#'   typically the output of [read_collection()].
#' @param model A \pkg{udpipe} model object, a path to a `.udpipe` file, or a
#'   language name (e.g. `"english"`) which is downloaded and cached via
#'   [ts_download_model()].
#' @param text_col Name of the text column (default `"text"`).
#' @param doc_id_col Name of the document-id column (default `"doc_id"`).
#' @param batch_size Integer. Documents processed per batch (default `500`).
#' @param verbose Logical. Print progress messages (default `TRUE`).
#'
#' @return A data frame with columns `doc_id`, `sentence_id` (globally unique),
#'   `token`, `lemma` (lower-cased) and `upos`.
#'
#' @examples
#' \dontrun{
#' coll   <- read_collection("sample_collection.csv")
#' tokens <- preprocess_texts(coll, model = "english")
#' result <- themescope(tokens)
#' }
#'
#' @export
preprocess_texts <- function(collection,
                             model,
                             text_col   = "text",
                             doc_id_col = "doc_id",
                             batch_size = 500,
                             verbose    = TRUE) {
  if (!is.data.frame(collection)) {
    cli::cli_abort("{.arg collection} must be a data frame.")
  }
  if (!text_col %in% names(collection)) {
    cli::cli_abort("Column {.val {text_col}} not found in {.arg collection}.")
  }
  if (!doc_id_col %in% names(collection)) {
    cli::cli_abort("Column {.val {doc_id_col}} not found in {.arg collection}.")
  }

  model <- .load_model(model, verbose = verbose)

  texts  <- as.character(collection[[text_col]])
  docids <- as.character(collection[[doc_id_col]])
  n      <- length(texts)

  themescope_progress(
    paste0("Annotating ", n, " documents (batch size = ", batch_size, ") ..."), verbose
  )

  n_batches <- ceiling(n / batch_size)
  chunks    <- vector("list", n_batches)

  for (i in seq_len(n_batches)) {
    idx_start <- (i - 1L) * batch_size + 1L
    idx_end   <- min(i * batch_size, n)
    if (verbose && n_batches > 1) {
      themescope_progress(
        paste0("  Batch ", i, "/", n_batches, " (docs ", idx_start, "-", idx_end, ")"),
        verbose
      )
    }
    ann <- udpipe::udpipe_annotate(
      object = model,
      x      = texts[idx_start:idx_end],
      doc_id = docids[idx_start:idx_end]
    )
    chunks[[i]] <- as.data.frame(ann, detailed = FALSE)
  }

  result <- do.call(rbind, chunks)

  result$sentence_id <- paste0(result$doc_id, "_s", result$sentence_id)
  no_lemma <- is.na(result$lemma) | result$lemma == ""
  result$lemma[no_lemma] <- result$token[no_lemma]
  result$lemma <- tolower(result$lemma)

  out <- result[, c("doc_id", "sentence_id", "token", "lemma", "upos"), drop = FALSE]
  rownames(out) <- NULL

  themescope_progress(
    paste0("Annotation complete: ", nrow(out), " tokens across ",
           length(unique(out$sentence_id)), " sentences."), verbose
  )

  out
}
