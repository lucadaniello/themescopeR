# Source of the updated UDPipe models / lexicons maintained for TALL.
# See https://github.com/massimoaria/tall.language.models
.ts_models_version <- "2.15"
.ts_models_base_url <- "https://github.com/massimoaria/tall.language.models/raw/main/2.15/"
.ts_registry_url <- "https://github.com/massimoaria/tall.language.models/raw/main/data/available_models.rdata"


#' Location of the themescopeR model cache
#'
#' Returns the directory where downloaded \pkg{udpipe} language models are
#' cached, so a model is fetched only once and reused across analyses. Defaults
#' to `tools::R_user_dir("themescopeR", "cache")`, the location R reserves for
#' package caches; override it with `options(themescopeR.model_dir = "/path")`.
#'
#' @details
#' Asking where the cache is does **not** create anything: the directory is
#' created only when a model is actually downloaded, that is when you call
#' [ts_download_model()] or hand a language name to [preprocess_texts()].
#' Nothing else in the package writes outside the session temporary directory.
#' Use [ts_clear_cache()] to delete what has been cached.
#'
#' @param create Logical. Create the directory if it does not exist
#'   (default `FALSE`).
#'
#' @return A character path to the cache directory.
#'
#' @seealso [ts_clear_cache()] to remove cached models,
#'   [ts_download_model()] to populate the cache.
#'
#' @examples
#' # Where models would be cached; this call creates nothing
#' themescope_cache_dir()
#'
#' @export
themescope_cache_dir <- function(create = FALSE) {
  dir <- getOption("themescopeR.model_dir",
                   tools::R_user_dir("themescopeR", which = "cache"))
  if (isTRUE(create) && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  dir
}

# Internal: session flag so the cache location is announced only once.
.ts_env <- new.env(parent = emptyenv())

# Internal: resolve a directory to download into, creating it at the last
# possible moment and telling the user where it is the first time.
.cache_dir_for_writing <- function(model_dir = NULL) {
  if (!is.null(model_dir)) {
    if (!dir.exists(model_dir)) {
      dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
    }
    return(model_dir)
  }
  dir <- themescope_cache_dir()
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (is.null(.ts_env$announced)) {
    cli::cli_alert_info(
      "Caching language models in {.path {dir}}. Remove them with {.fn ts_clear_cache}."
    )
    .ts_env$announced <- TRUE
  }
  dir
}


#' Delete cached language models
#'
#' Removes the models and the registry downloaded by [ts_download_model()] and
#' [ts_list_models()], and the cache directory itself. Nothing else in the
#' package writes persistently, so this clears everything themescopeR has ever
#' stored on disk.
#'
#' @param model_dir Directory to clear. Defaults to [themescope_cache_dir()].
#' @param ask Logical. Ask for confirmation before deleting. Defaults to
#'   [interactive()].
#'
#' @return The paths that were removed, invisibly (`character(0)` if the cache
#'   was empty or the user declined).
#'
#' @seealso [themescope_cache_dir()], [ts_download_model()].
#'
#' @examples
#' # Point the cache at a throwaway directory and clear it again
#' d <- file.path(tempdir(), "themescope-cache-example")
#' dir.create(d, showWarnings = FALSE)
#' file.create(file.path(d, "example-ud-2.15.udpipe"))
#' ts_clear_cache(model_dir = d, ask = FALSE)
#' dir.exists(d)
#'
#' @export
ts_clear_cache <- function(model_dir = NULL, ask = interactive()) {
  dir <- if (is.null(model_dir)) themescope_cache_dir() else model_dir
  if (!dir.exists(dir)) {
    cli::cli_alert_info("Nothing cached in {.path {dir}}.")
    return(invisible(character(0)))
  }
  files <- list.files(dir, full.names = TRUE, recursive = TRUE)
  if (length(files) == 0) {
    unlink(dir, recursive = TRUE)
    return(invisible(character(0)))
  }
  size_mb <- sum(file.size(files), na.rm = TRUE) / 1024^2
  if (isTRUE(ask)) {
    ok <- utils::askYesNo(
      sprintf("Delete %d cached file(s) (%.1f MB) from %s?",
              length(files), size_mb, dir),
      default = FALSE
    )
    if (!isTRUE(ok)) {
      cli::cli_alert_info("Cache left untouched.")
      return(invisible(character(0)))
    }
  }
  unlink(dir, recursive = TRUE)
  cli::cli_alert_success(
    "Removed {length(files)} file{?s} ({round(size_mb, 1)} MB) from {.path {dir}}."
  )
  invisible(files)
}


#' List the available updated language models
#'
#' Retrieves the registry of updated \pkg{udpipe} models (Universal Dependencies
#' 2.15) maintained for TALL. The registry is downloaded once, cached in
#' [themescope_cache_dir()] and reused on later calls; the cache directory is
#' created only at that point.
#'
#' @param refresh Logical. Force a re-download of the registry (default `FALSE`).
#' @param cache_dir Directory to keep the registry in. If `NULL` (default) it
#'   goes to [themescope_cache_dir()], created at that point and only then.
#'
#' @return A data frame with one row per model, including `language_name`,
#'   `treebank` and `file` (the model code used to build the file name).
#'
#' @examples
#' \donttest{
#' # Needs an internet connection the first time; cached afterwards. Kept in the
#' # session temporary directory here, so nothing persists.
#' models <- try(ts_list_models(cache_dir = tempdir()), silent = TRUE)
#' if (!inherits(models, "try-error")) {
#'   head(models[, c("language_name", "treebank")])
#' }
#' }
#'
#' @export
ts_list_models <- function(refresh = FALSE, cache_dir = NULL) {
  # Read from the cache without creating anything ...
  dir    <- if (is.null(cache_dir)) themescope_cache_dir() else cache_dir
  cached <- file.path(dir, "available_models.rdata")
  if (!refresh && file.exists(cached)) return(.read_model_registry(cached))

  # ... and only create the directory when there is something to write into it.
  cache <- file.path(.cache_dir_for_writing(cache_dir), "available_models.rdata")
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
  .read_model_registry(cache)
}

# Internal: load the registry data frame out of the cached .rdata file.
.read_model_registry <- function(path) {
  e <- new.env(parent = emptyenv())
  load(path, envir = e)
  df <- mget(ls(e), envir = e)
  df <- df[[which(vapply(df, is.data.frame, logical(1)))[1]]]
  as.data.frame(df, stringsAsFactors = FALSE)
}

# Internal: resolve a (language, treebank) request to a model file code. The
# registry follows the caller's directory, so asking for a model in tempdir()
# does not leave the catalogue behind in the user cache.
.resolve_model_file <- function(language, treebank = NULL, registry = NULL,
                                cache_dir = NULL) {
  if (is.null(registry)) registry <- ts_list_models(cache_dir = cache_dir)
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
#' @details
#' The updated models and lexicons are curated and maintained by
#' **Massimo Aria** in the `tall.language.models` repository, part of the
#' \href{https://massimoaria.github.io/tall-app/}{TALL} project. We gratefully acknowledge that work
#' and redistribute nothing here: models are fetched on demand from the source
#' repository.
#'
#' To explore which models are available, together with their treebanks,
#' contributors, descriptions, corpus sizes and Universal Dependencies hub
#' pages, use [ts_list_models()] (whose returned data frame includes
#' `description` and `hub_page_link` columns), or browse the repository directly:
#' \url{https://github.com/massimoaria/tall.language.models}.
#'
#' @param language Character language name (e.g. `"english"`, `"italian"`). See
#'   [ts_list_models()].
#' @param treebank Optional treebank name (e.g. `"EWT"`, `"ISDT"`). If `NULL`,
#'   the first model listed for the language is used.
#' @param model_dir Directory to download into. If `NULL` (default) the model is
#'   cached in [themescope_cache_dir()], which is created at that point and only
#'   then. Pass a path of your own (for instance `tempdir()`) to keep the
#'   download inside the current session.
#' @param overwrite Logical. Re-download even if cached (default `FALSE`).
#'
#' @return The path to the cached `.udpipe` model file (invisibly).
#'
#' @seealso [ts_list_models()] for the full catalogue (descriptions and links),
#'   [ts_clear_cache()] to delete the downloaded models.
#'
#' @references
#' Aria, M. \emph{tall.language.models}: updated UDPipe models and lexicons for
#' TALL. \url{https://github.com/massimoaria/tall.language.models}
#'
#' @examples
#' \donttest{
#' # Downloaded into the session temporary directory, so nothing persists
#' path <- try(ts_download_model("english", model_dir = tempdir()), silent = TRUE)
#' if (!inherits(path, "try-error")) basename(path)
#' }
#'
#' @export
ts_download_model <- function(language,
                              treebank  = NULL,
                              model_dir = NULL,
                              overwrite = FALSE) {
  file_code <- .resolve_model_file(language, treebank, cache_dir = model_dir)
  filename  <- paste0(file_code, "-ud-", .ts_models_version, ".udpipe")

  # Look in the cache first; if the model is there, nothing has to be created.
  cached <- file.path(if (is.null(model_dir)) themescope_cache_dir() else model_dir,
                      filename)
  if (file.exists(cached) && !overwrite) {
    themescope_progress("Using cached model: {.file {filename}}", verbose = TRUE)
    return(invisible(cached))
  }

  dest <- file.path(.cache_dir_for_writing(model_dir), filename)
  url  <- paste0(.ts_models_base_url, filename)
  themescope_progress("Downloading model {.file {filename}} ...", verbose = TRUE)
  utils::download.file(url, dest, mode = "wb", quiet = TRUE)
  invisible(dest)
}


#' Resolve the cached path of an updated udpipe model
#'
#' Returns the path to a cached model without downloading anything and without
#' creating any directory.
#'
#' @inheritParams ts_download_model
#'
#' @return The path to the cached `.udpipe` file, or `NA` if it is not cached.
#'
#' @examples
#' \donttest{
#' # NA until the model has been downloaded (needs the catalogue, hence a
#' # connection on first use)
#' p <- try(ts_model_path("english"), silent = TRUE)
#' if (!inherits(p, "try-error")) p
#' }
#'
#' @export
ts_model_path <- function(language, treebank = NULL, model_dir = NULL) {
  file_code <- .resolve_model_file(language, treebank, cache_dir = model_dir)
  filename  <- paste0(file_code, "-ud-", .ts_models_version, ".udpipe")
  dir       <- if (is.null(model_dir)) themescope_cache_dir() else model_dir
  dest      <- file.path(dir, filename)
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
#'   Ignored when `parallel_cores > 1` (udpipe chunks the work itself).
#' @param parallel_cores Integer (default `1`). If greater than 1, annotation is
#'   parallelised across that many CPU cores via [udpipe::udpipe()], which
#'   speeds up large corpora considerably.
#' @param verbose Logical. Print progress messages (default `TRUE`).
#'
#' @return The **complete** \pkg{udpipe} annotation as a data frame (one row per
#'   token), with all columns returned by [udpipe::udpipe_annotate()] preserved
#'   (`doc_id`, `paragraph_id`, `sentence_id`, `sentence`, `token_id`, `token`,
#'   `lemma`, `upos`, `xpos`, `feats`, `head_token_id`, `dep_rel`, ...). Nothing
#'   is dropped, so the full linguistic annotation is available for inspection.
#'   Downstream functions ([build_vocab()], [build_cooccurrence_matrix()]) select
#'   and case-fold the relevant word column (`token` or `lemma`) themselves, and
#'   use `doc_id` + `sentence_id` as the sentence key.
#'
#' @examples
#' # The shape of the annotation this returns; the bundled demo corpus ships
#' # pre-annotated, so no model download is needed to inspect it
#' words <- readRDS(system.file("extdata", "demo_annotated.rds",
#'                              package = "themescopeR"))
#' head(words[, c("doc_id", "sentence_id", "token", "lemma", "upos")])
#'
#' \donttest{
#' # Annotating your own corpus. The language model is downloaded once and
#' # cached; here it goes to the session temporary directory instead.
#' coll  <- read_collection(system.file("extdata", "sample_collection.csv",
#'                                      package = "themescopeR"))
#' model <- try(ts_download_model("english", model_dir = tempdir()), silent = TRUE)
#' if (!inherits(model, "try-error")) {
#'   tokens <- preprocess_texts(utils::head(coll, 5), model = model)
#'   head(tokens[, c("doc_id", "token", "lemma", "upos")])
#' }
#' }
#'
#' @export
preprocess_texts <- function(collection,
                             model,
                             text_col       = "text",
                             doc_id_col     = "doc_id",
                             batch_size     = 500,
                             parallel_cores = 1,
                             verbose        = TRUE) {
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

  if (parallel_cores > 1) {
    themescope_progress(
      paste0("Annotating ", n, " documents on ", as.integer(parallel_cores), " cores ..."),
      verbose
    )
    out <- udpipe::udpipe(
      x      = data.frame(doc_id = docids, text = texts, stringsAsFactors = FALSE),
      object = model,
      parallel.cores = as.integer(parallel_cores)
    )
    out <- as.data.frame(out, stringsAsFactors = FALSE)
  } else {
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

    out <- dplyr::bind_rows(chunks)
  }
  rownames(out) <- NULL

  themescope_progress(
    paste0("Annotation complete: ", nrow(out), " tokens across ",
           length(unique(paste(out$doc_id, out$sentence_id))), " sentences."), verbose
  )

  out
}
