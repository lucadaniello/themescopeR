# Internal: extract the lower-cased "word" vector (the chosen unit) aligned to
# the rows of an annotated words data frame. When `unit = "lemma"`, missing or
# empty lemmas fall back to the corresponding token.
.word_vector <- function(words_df, unit) {
  if (!unit %in% names(words_df)) {
    cli::cli_abort(c(
      "x" = "Column {.field {unit}} not found in the words data frame.",
      "i" = "Available columns: {.field {names(words_df)}}."
    ))
  }
  w <- as.character(words_df[[unit]])
  if (unit == "lemma" && "token" %in% names(words_df)) {
    miss <- is.na(w) | !nzchar(trimws(w))
    w[miss] <- as.character(words_df$token[miss])
  }
  tolower(w)
}


#' Build a vocabulary data frame from an annotated words data frame
#'
#' Counts the most frequent terms in a POS-filtered, annotated data frame and
#' returns them as a tidy vocabulary. The analysis unit, the "word", can be the
#' surface `token` or the `lemma`.
#'
#' @param words_df An annotated data frame (e.g. the output of
#'   [preprocess_texts()]). The "word" used to build the vocabulary is the column
#'   named by `unit` (`token` or `lemma`); the data frame must also contain
#'   `doc_id`, `sentence_id` and `upos`. See [validate_words_df()].
#' @param unit Character. Which column to use as the word: `"lemma"` (default) or
#'   `"token"`. When `"lemma"`, missing lemmas fall back to the token.
#' @param vocab_size Integer or `NULL`. If `NULL` (default) the vocabulary
#'   contains **all** terms; otherwise only the top `vocab_size` by frequency.
#' @param pos_filter Character vector of Universal POS tags to retain (default
#'   `c("NOUN", "ADJ", "PROPN")`).
#'
#' @return A data frame ordered by decreasing frequency with columns:
#'   \describe{
#'     \item{`token` or `lemma`}{The word (lower-cased); the column is named
#'       after `unit`.}
#'     \item{`freq`}{Integer term frequency (number of occurrences).}
#'     \item{`upos`}{The dominant (most frequent) Universal POS tag of the term.}
#'   }
#'
#' @examples
#' df <- data.frame(
#'   doc_id = c(1, 1, 1, 1), sentence_id = c(1, 1, 1, 2),
#'   token = c("dogs", "cats", "dog", "birds"),
#'   lemma = c("dog", "cat", "dog", "bird"), upos = "NOUN"
#' )
#' build_vocab(df, unit = "lemma")
#'
#' @export
build_vocab <- function(words_df,
                        unit       = c("lemma", "token"),
                        vocab_size = NULL,
                        pos_filter = c("NOUN", "ADJ", "PROPN")) {
  validate_words_df(words_df)
  unit <- match.arg(unit)

  keep_pos <- !is.na(words_df$upos) & words_df$upos %in% pos_filter
  filtered <- words_df[keep_pos, , drop = FALSE]
  if (nrow(filtered) == 0) {
    cli::cli_abort(c(
      "x" = "No rows remain after POS filtering.",
      "i" = "POS filter applied: {.val {pos_filter}}.",
      "i" = "Unique UPOS tags found: {.val {unique(words_df$upos)}}."
    ))
  }

  w   <- .word_vector(filtered, unit)
  ok  <- !is.na(w) & nzchar(trimws(w))
  dat <- data.frame(word = w[ok], upos = filtered$upos[ok], stringsAsFactors = FALSE)
  if (nrow(dat) == 0) {
    cli::cli_abort("No non-empty terms found for unit {.field {unit}} after filtering.")
  }

  # One (word, upos) count, then the dominant POS per word: much faster than a
  # per-group table() on large vocabularies (ties resolved alphabetically, as
  # count() orders upos within word).
  pos_counts <- dplyr::count(dat, .data$word, .data$upos, name = "n_pos")
  agg <- dplyr::summarise(
    dplyr::group_by(pos_counts, .data$word),
    freq = sum(.data$n_pos),
    upos = .data$upos[which.max(.data$n_pos)],
    .groups = "drop"
  )
  agg <- dplyr::arrange(agg, dplyr::desc(.data$freq), .data$word)
  agg <- as.data.frame(agg, stringsAsFactors = FALSE)

  if (!is.null(vocab_size)) {
    agg <- utils::head(agg, as.integer(vocab_size))
  }

  names(agg)[names(agg) == "word"] <- unit
  agg <- agg[, c(unit, "freq", "upos"), drop = FALSE]
  rownames(agg) <- NULL
  agg
}


#' Normalise a co-occurrence matrix into a similarity matrix
#'
#' Re-weights raw co-occurrence counts with one of several similarity measures
#' commonly used in co-word analysis. Given the co-occurrence count
#' \eqn{c_{ij}} and the term presences \eqn{a_i, a_j}:
#' \describe{
#'   \item{`"association"`}{\eqn{c_{ij} / (a_i a_j)}, Association Strength
#'     (ThemeScope default; van Eck & Waltman).}
#'   \item{`"equivalence"`}{\eqn{c_{ij}^2 / (a_i a_j)}, equivalence index.}
#'   \item{`"jaccard"`}{\eqn{c_{ij} / (a_i + a_j - c_{ij})}.}
#'   \item{`"salton"`}{\eqn{c_{ij} / \sqrt{a_i a_j}}, cosine or Salton's measure.}
#'   \item{`"inclusion"`}{\eqn{c_{ij} / \min(a_i, a_j)}.}
#'   \item{`"frequency"`}{the raw counts, unchanged.}
#' }
#'
#' @param cooc_matrix Symmetric sparse \code{Matrix} of co-occurrence counts.
#' @param presence Named numeric vector of term presences \eqn{a_t}; names must
#'   match the rows/columns of `cooc_matrix`.
#' @param method One of `"association"` (default), `"equivalence"`, `"jaccard"`,
#'   `"salton"`, `"inclusion"`, `"frequency"`.
#'
#' @return A sparse `dgCMatrix` of the same dimensions as `cooc_matrix`.
#'
#' @examples
#' m <- Matrix::sparseMatrix(
#'   i = c(1, 2), j = c(2, 1), x = c(3, 3), dims = c(3, 3),
#'   dimnames = list(c("a", "b", "c"), c("a", "b", "c"))
#' )
#' normalize_cooccurrence(m, c(a = 5, b = 4, c = 2), method = "jaccard")
#'
#' @export
normalize_cooccurrence <- function(cooc_matrix, presence,
                                   method = c("association", "equivalence",
                                              "jaccard", "salton", "inclusion",
                                              "frequency")) {
  method <- match.arg(method)
  if (!inherits(cooc_matrix, "Matrix") && !is.matrix(cooc_matrix)) {
    cli::cli_abort("{.arg cooc_matrix} must be a {.cls Matrix} or base matrix object.")
  }
  if (!is.numeric(presence)) {
    cli::cli_abort("{.arg presence} must be a numeric vector.")
  }
  n <- nrow(cooc_matrix)
  if (length(presence) != n) {
    cli::cli_abort(
      "Length of {.arg presence} ({length(presence)}) must equal the number of rows in {.arg cooc_matrix} ({n})."
    )
  }

  cm <- Matrix::Matrix(cooc_matrix, sparse = TRUE)
  cm <- Matrix::drop0(methods::as(methods::as(cm, "generalMatrix"), "CsparseMatrix"))
  if (method == "frequency") return(cm)

  # Work directly on the stored triplets: one pass, no sparse re-indexing.
  trip <- Matrix::summary(cm)
  if (nrow(trip) == 0) return(cm)

  rows <- trip$i; cols <- trip$j; cij <- trip$x
  ai <- as.numeric(presence)[rows]
  aj <- as.numeric(presence)[cols]

  val <- switch(method,
    association = cij / (ai * aj),
    equivalence = cij^2 / (ai * aj),
    jaccard     = cij / (ai + aj - cij),
    salton      = cij / sqrt(ai * aj),
    inclusion   = cij / pmin(ai, aj)
  )
  val[!is.finite(val)] <- 0

  Matrix::sparseMatrix(i = rows, j = cols, x = val, dims = dim(cm),
                       dimnames = dimnames(cm))
}


#' Compute Association Strength from a co-occurrence matrix
#'
#' Convenience wrapper around [normalize_cooccurrence()] with
#' `method = "association"`: \eqn{AS(t,t') = c_{tt'} / (a_t a_{t'})} (ThemeScope
#' papers, Eq. 1). Values lie in \eqn{[0,1]}; higher values indicate stronger
#' association, reducing the bias of highly frequent terms.
#'
#' @param cooc_matrix Symmetric sparse \code{Matrix} of co-occurrence counts.
#' @param presence Named numeric vector of term presences \eqn{a_t}.
#'
#' @return A sparse `dgCMatrix` of Association Strength values.
#'
#' @seealso [normalize_cooccurrence()] for other similarity measures.
#'
#' @examples
#' m <- Matrix::sparseMatrix(
#'   i = c(1, 2), j = c(2, 1), x = c(3, 3), dims = c(3, 3),
#'   dimnames = list(c("a", "b", "c"), c("a", "b", "c"))
#' )
#' compute_association_strength(m, c(a = 5, b = 4, c = 2))
#'
#' @export
compute_association_strength <- function(cooc_matrix, presence) {
  normalize_cooccurrence(cooc_matrix, presence, method = "association")
}


#' Build a co-occurrence (similarity) matrix
#'
#' Counts how many text units (sentences or documents) each pair of vocabulary
#' terms co-occur in (each pair counted at most once per unit), then optionally
#' re-weights the counts with a similarity measure (see [normalize_cooccurrence()]).
#' Also returns the **presence** \eqn{a_t} of each term, the number of units
#' containing it, which is the correct marginal for the normalisation.
#'
#' @param words_df An annotated words data frame (e.g. from [preprocess_texts()]),
#'   containing `doc_id`, `sentence_id`, `upos` and the `unit` column.
#' @param vocab Optional vocabulary. If `NULL` (default) it is built from
#'   `words_df` with [build_vocab()] using `unit`, `vocab_size` and `pos_filter`.
#'   You may instead pass a [build_vocab()] data frame to reuse a fixed
#'   vocabulary; the analysis unit is then taken from its term column.
#' @param unit Character. Word column to use: `"lemma"` (default) or `"token"`.
#'   Ignored when `vocab` is a [build_vocab()] data frame (the unit is inferred
#'   from it).
#' @param vocab_size Integer or `NULL`. Passed to [build_vocab()] when `vocab` is
#'   `NULL` (`NULL` = keep all terms).
#' @param pos_filter Character vector of POS tags. Passed to [build_vocab()] when
#'   `vocab` is `NULL`.
#' @param window Co-occurrence unit: `"sentence"` (default) counts terms that
#'   share a sentence; `"document"` counts terms that share a document.
#' @param normalization Similarity measure applied to the counts: one of
#'   `"association"`, `"equivalence"`, `"jaccard"`, `"salton"`, `"inclusion"`, or
#'   `"frequency"`. `NULL` (default) is equivalent to `"frequency"` (raw counts).
#'
#' @return A named list with:
#'   \describe{
#'     \item{`cooc_matrix`}{Symmetric sparse matrix after `normalization`
#'       (raw counts if `NULL`/`"frequency"`).}
#'     \item{`counts`}{The raw sentence/document co-occurrence counts.}
#'     \item{`presence`}{Named integer vector \eqn{a_t} (units containing each term).}
#'     \item{`vocab`}{The vocabulary data frame used.}
#'     \item{`unit`, `window`, `normalization`}{The settings used.}
#'   }
#'
#' @examples
#' df <- data.frame(
#'   doc_id = c(1, 1, 1, 1, 1), sentence_id = c(1, 1, 1, 2, 2),
#'   lemma = c("dog", "cat", "dog", "bird", "cat"), upos = "NOUN"
#' )
#' res <- build_cooccurrence_matrix(df, normalization = "association")
#' res$cooc_matrix
#'
#' @export
build_cooccurrence_matrix <- function(words_df,
                                      vocab         = NULL,
                                      unit          = c("lemma", "token"),
                                      vocab_size    = NULL,
                                      pos_filter    = c("NOUN", "ADJ", "PROPN"),
                                      window        = c("sentence", "document"),
                                      normalization = NULL) {
  validate_words_df(words_df)
  unit   <- match.arg(unit)
  window <- match.arg(window)
  if (is.null(normalization)) normalization <- "frequency"
  normalization <- match.arg(normalization,
                             c("association", "equivalence", "jaccard",
                               "salton", "inclusion", "frequency"))

  # Resolve vocabulary + unit
  if (is.null(vocab)) {
    vocab <- build_vocab(words_df, unit = unit, vocab_size = vocab_size,
                         pos_filter = pos_filter)
  } else if (is.data.frame(vocab)) {
    found <- intersect(c("lemma", "token"), names(vocab))
    if (length(found) == 0) {
      cli::cli_abort("{.arg vocab} must be a {.fn build_vocab} data frame (a {.field lemma} or {.field token} column).")
    }
    unit <- found[1]
  } else {
    cli::cli_abort("{.arg vocab} must be {.code NULL} or a {.fn build_vocab} data frame.")
  }

  terms <- as.character(vocab[[unit]])
  n <- length(terms)
  if (n < 2) cli::cli_abort("Vocabulary must contain at least 2 terms.")
  term_idx <- stats::setNames(seq_len(n), terms)

  keep_pos <- !is.na(words_df$upos) & words_df$upos %in% pos_filter
  filtered <- words_df[keep_pos, , drop = FALSE]
  w <- .word_vector(filtered, unit)
  in_vocab <- !is.na(w) & w %in% terms
  filtered <- filtered[in_vocab, , drop = FALSE]
  w <- w[in_vocab]
  if (nrow(filtered) == 0) cli::cli_abort("No tokens in vocab remain after POS filtering.")

  unit_key <- if (window == "document") {
    as.character(filtered$doc_id)
  } else {
    paste(filtered$doc_id, filtered$sentence_id, sep = "__")
  }

  # Sparse unit x term incidence matrix (binary: term occurs in unit), built
  # from de-duplicated (unit, term) pairs. Co-occurrence counts are then a
  # single crossprod -- no per-sentence R loop.
  unit_id <- as.integer(factor(unit_key))
  term_id <- unname(term_idx[w])
  pair_key <- (as.numeric(unit_id) - 1) * n + term_id
  keep_pair <- !duplicated(pair_key)

  incidence <- Matrix::sparseMatrix(
    i = unit_id[keep_pair], j = term_id[keep_pair], x = 1,
    dims = c(max(unit_id), n)
  )

  presence <- stats::setNames(as.integer(Matrix::colSums(incidence)), terms)

  counts <- Matrix::crossprod(incidence)  # term x term; diagonal = presence
  counts <- methods::as(counts, "generalMatrix")
  Matrix::diag(counts) <- 0
  counts <- Matrix::drop0(counts)
  dimnames(counts) <- list(terms, terms)

  cooc_matrix <- normalize_cooccurrence(counts, presence, method = normalization)

  list(cooc_matrix = cooc_matrix, counts = counts, presence = presence,
       vocab = vocab, unit = unit, window = window, normalization = normalization)
}


#' Build a thresholded semantic co-occurrence network
#'
#' Constructs an undirected weighted \code{igraph} graph from a similarity
#' matrix, retaining only edges whose weight exceeds the `threshold_percentile`
#' quantile of all non-zero weights.
#'
#' @param as_matrix Symmetric sparse \code{Matrix} of edge weights (e.g. the
#'   `cooc_matrix` from [build_cooccurrence_matrix()] or the output of
#'   [normalize_cooccurrence()]).
#' @param threshold_percentile Numeric in \eqn{(0, 1)}. Quantile of non-zero
#'   weights used as the minimum edge weight (default `0.98`). This is an
#'   implementation choice for network sparsification, not part of the metric
#'   definitions; tune it for your corpus.
#' @param verbose Logical. Print progress messages (default `TRUE`).
#'
#' @return An undirected weighted \code{igraph} object; `E(graph)$weight` holds
#'   the edge weights and `V(graph)$name` the term labels. Only terms with at
#'   least one retained edge appear as vertices.
#'
#' @examples
#' \dontrun{
#' net <- build_cooccurrence_network(cooc$cooc_matrix, threshold_percentile = 0.98)
#' }
#'
#' @export
build_cooccurrence_network <- function(as_matrix,
                                       threshold_percentile = 0.98,
                                       verbose = TRUE) {
  if (!inherits(as_matrix, "Matrix") && !is.matrix(as_matrix)) {
    cli::cli_abort("{.arg as_matrix} must be a {.cls Matrix} or base matrix.")
  }
  if (!is.numeric(threshold_percentile) || length(threshold_percentile) != 1 ||
      threshold_percentile <= 0 || threshold_percentile >= 1) {
    cli::cli_abort("{.arg threshold_percentile} must be a single numeric in (0, 1).")
  }

  upper   <- Matrix::triu(methods::as(as_matrix, "CsparseMatrix"), k = 1)
  nz_vals <- upper@x[upper@x > 0]
  if (length(nz_vals) == 0) cli::cli_abort("No non-zero values found in {.arg as_matrix}.")

  threshold <- stats::quantile(nz_vals, probs = threshold_percentile, na.rm = TRUE)
  themescope_progress(
    "Edge threshold ({threshold_percentile * 100}th percentile): {round(threshold, 8)}",
    verbose
  )

  upper_thresh <- upper
  upper_thresh@x[upper_thresh@x <= threshold] <- 0
  upper_thresh <- Matrix::drop0(upper_thresh)

  edge_positions <- Matrix::which(upper_thresh > 0, arr.ind = TRUE)
  if (nrow(edge_positions) == 0) {
    cli::cli_abort(c(
      "x" = "No edges remain after applying the threshold.",
      "i" = "Try lowering {.arg threshold_percentile} (currently {threshold_percentile})."
    ))
  }

  term_names <- rownames(as_matrix)
  if (is.null(term_names)) term_names <- as.character(seq_len(nrow(as_matrix)))
  edge_weights <- upper_thresh[edge_positions]
  edge_list <- cbind(term_names[edge_positions[, 1]], term_names[edge_positions[, 2]])

  graph <- igraph::graph_from_edgelist(edge_list, directed = FALSE)
  igraph::E(graph)$weight <- as.numeric(edge_weights)

  themescope_progress(
    "Network built: {igraph::vcount(graph)} nodes, {igraph::ecount(graph)} edges.",
    verbose
  )

  graph
}
