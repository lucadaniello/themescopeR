# Internal: case-insensitive detection of a column among candidates.
.detect_col <- function(nms, candidates) {
  hit <- which(tolower(nms) %in% tolower(candidates))
  if (length(hit) == 0) return(NA_character_)
  nms[hit[1]]
}

# Internal: read a single non-zip file into a raw data frame (no tidying yet).
.read_one_raw <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    csv = readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    tsv = readr::read_tsv(path, show_col_types = FALSE, progress = FALSE),
    txt = data.frame(
      doc_id = tools::file_path_sans_ext(basename(path)),
      text   = paste(readr::read_lines(path, progress = FALSE), collapse = "\n"),
      stringsAsFactors = FALSE
    ),
    xlsx = as.data.frame(readxl::read_excel(path)),
    xls  = as.data.frame(readxl::read_excel(path)),
    rdata = .read_rdata(path),
    rda   = .read_rdata(path),
    cli::cli_abort(c(
      "x" = "Unsupported file extension: {.val {ext}}.",
      "i" = "Supported: csv, tsv, txt, xlsx, xls, RData, rda (or a zip of these)."
    ))
  )
}

# Internal: load the first data frame found in an .RData/.rda file.
.read_rdata <- function(path) {
  e <- new.env(parent = emptyenv())
  load(path, envir = e)
  objs <- mget(ls(e), envir = e)
  is_df <- vapply(objs, is.data.frame, logical(1))
  if (!any(is_df)) {
    cli::cli_abort("No data frame found in {.path {basename(path)}}.")
  }
  as.data.frame(objs[[which(is_df)[1]]])
}

# Internal: coerce a raw data frame to the tidy (doc_id, text, ...) layout.
.tidy_collection <- function(df, text_col = NULL, id_col = NULL, source = NULL) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  nms <- names(df)

  # ---- text column ----
  tcol <- text_col %||% .detect_col(nms, c("text", "body", "content", "comment", "message"))
  if (is.na(tcol) || is.null(tcol)) {
    char_cols <- nms[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
    if (length(char_cols) == 1) {
      tcol <- char_cols
    } else {
      cli::cli_abort(c(
        "x" = "Could not detect a text column{if (!is.null(source)) paste0(' in ', source)}.",
        "i" = "Pass {.arg text_col} explicitly. Columns found: {.field {nms}}."
      ))
    }
  }
  if (!tcol %in% nms) {
    cli::cli_abort("Text column {.val {tcol}} not found. Columns: {.field {nms}}.")
  }

  # ---- id column ----
  icol <- id_col %||% .detect_col(nms, c("doc_id", "id", "document", "docid", "doc"))
  if (is.na(icol) || is.null(icol)) {
    df$doc_id <- as.character(seq_len(nrow(df)))
    icol <- "doc_id"
  }
  if (!icol %in% nms && icol != "doc_id") {
    cli::cli_abort("Id column {.val {icol}} not found. Columns: {.field {nms}}.")
  }

  out <- df
  names(out)[names(out) == tcol] <- "text"
  if (icol != "doc_id") names(out)[names(out) == icol] <- "doc_id"

  out$doc_id <- as.character(out$doc_id)
  out$text   <- as.character(out$text)

  if (anyDuplicated(out$doc_id)) {
    cli::cli_warn("Duplicated {.field doc_id} values found; making them unique.")
    out$doc_id <- make.unique(out$doc_id)
  }

  # doc_id, text first; preserve remaining columns
  rest <- setdiff(names(out), c("doc_id", "text"))
  out[, c("doc_id", "text", rest), drop = FALSE]
}

`%||%` <- function(x, y) if (is.null(x)) y else x


#' Read a document collection into a tidy data frame
#'
#' Imports a corpus of raw documents from a single file or a zip archive of many
#' files, returning a tidy data frame with a `doc_id` and a `text` column (plus
#' any metadata columns present in the source).
#'
#' Supported single-file formats: `.csv`, `.tsv`, `.txt`, `.xlsx`/`.xls`, and
#' `.RData`/`.rda`. A `.zip` may contain any number of these (e.g. many `.csv`
#' or many `.txt`); all are read and row-bound. For `.txt` files, **each file is
#' treated as one document** (its `doc_id` is the file name); this makes a zip of
#' `.txt` files a natural multi-document collection.
#'
#' @param path Path to a single file or a `.zip` archive.
#' @param text_col Optional name of the text column. If `NULL`, detected from
#'   common names (`text`, `body`, `content`, `comment`, `message`) or, failing
#'   that, the sole character column.
#' @param id_col Optional name of the document-id column. If `NULL`, detected
#'   from common names (`doc_id`, `id`, `document`, ...); if none is found, a
#'   sequential `doc_id` is generated.
#'
#' @return A data frame with `doc_id` (character) and `text` (character) as the
#'   first two columns, followed by any remaining source columns. Duplicated ids
#'   are made unique with a warning.
#'
#' @examples
#' \dontrun{
#' # Single CSV
#' coll <- read_collection("sample_collection.csv")
#'
#' # The three bundled formats yield identical tidy results
#' csv <- read_collection(system.file("extdata", "sample_collection.csv",
#'                                     package = "themescopeR"))
#'
#' # A zip of many text files (one document each)
#' coll <- read_collection("texts.zip")
#' }
#'
#' @export
read_collection <- function(path, text_col = NULL, id_col = NULL) {
  if (!is.character(path) || length(path) != 1) {
    cli::cli_abort("{.arg path} must be a single file path.")
  }
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}.")
  }

  ext <- tolower(tools::file_ext(path))

  if (ext == "zip") {
    tmp <- tempfile("themescope_unzip_")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    files <- utils::unzip(path, exdir = tmp)
    # Ignore directories and hidden/metadata files
    files <- files[!grepl("(^|/)__MACOSX/|/\\._", files)]
    files <- files[tolower(tools::file_ext(files)) %in%
                     c("csv", "tsv", "txt", "xlsx", "xls", "rdata", "rda")]
    if (length(files) == 0) {
      cli::cli_abort("No supported files found inside {.path {basename(path)}}.")
    }
    parts <- lapply(files, function(f) {
      .tidy_collection(.read_one_raw(f), text_col = text_col, id_col = id_col,
                       source = basename(f))
    })
    out <- dplyr::bind_rows(parts)
    if (anyDuplicated(out$doc_id)) {
      out$doc_id <- make.unique(out$doc_id)
    }
    return(out)
  }

  .tidy_collection(.read_one_raw(path), text_col = text_col, id_col = id_col,
                   source = basename(path))
}
