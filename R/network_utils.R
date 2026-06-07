#' Build a vocabulary from a tokens data frame
#'
#' Identifies the most frequent terms in a POS-filtered tokens data frame. Uses
#' the `lemma` column when available, falling back to `token`.
#'
#' @param tokens_df Data frame with `doc_id`, `sentence_id`, `upos`, and at least
#'   one of `lemma` or `token`. See [validate_tokens_df()].
#' @param vocab_size Integer. Maximum number of terms to keep (default `1500`).
#' @param pos_filter Character vector of Universal POS tags to retain (default
#'   `c("NOUN", "ADJ", "PROPN")`).
#'
#' @return A character vector of at most `vocab_size` terms, ordered by
#'   decreasing frequency.
#'
#' @examples
#' df <- data.frame(
#'   doc_id = c(1, 1, 1, 1), sentence_id = c(1, 1, 1, 2),
#'   lemma = c("dog", "cat", "dog", "bird"), upos = "NOUN"
#' )
#' build_vocab(df, vocab_size = 10)
#'
#' @export
build_vocab <- function(tokens_df,
                        vocab_size = 1500,
                        pos_filter = c("NOUN", "ADJ", "PROPN")) {
  validate_tokens_df(tokens_df)

  word_col <- if ("lemma" %in% names(tokens_df)) "lemma" else "token"

  filtered <- tokens_df[!is.na(tokens_df$upos) & tokens_df$upos %in% pos_filter, ]

  if (nrow(filtered) == 0) {
    cli::cli_abort(c(
      "x" = "No tokens remain after POS filtering.",
      "i" = "POS filter applied: {.val {pos_filter}}.",
      "i" = "Unique UPOS tags found: {.val {unique(tokens_df$upos)}}."
    ))
  }

  words <- filtered[[word_col]]
  words <- words[!is.na(words) & nchar(trimws(words)) > 0]

  if (length(words) == 0) {
    cli::cli_abort("No non-empty terms found in column {.field {word_col}} after filtering.")
  }

  freq_table <- sort(table(words), decreasing = TRUE)
  n_keep <- min(vocab_size, length(freq_table))
  as.character(names(freq_table)[seq_len(n_keep)])
}


#' Build a sentence-level co-occurrence matrix
#'
#' Counts how many sentences each pair of vocabulary terms co-occur in (each
#' pair counted at most once per sentence). The matrix is symmetric with a zero
#' diagonal. Also returns the **presence** \eqn{a_t} of each term: the number of
#' sentences containing it. Presence (not raw token frequency) is the correct
#' marginal for the Association Strength normalisation, since the co-occurrence
#' counts are themselves sentence-based.
#'
#' @param tokens_df Data frame as in [build_vocab()].
#' @param vocab Character vector of vocabulary terms. If `NULL`, built with
#'   [build_vocab()] using `vocab_size` and `pos_filter`.
#' @param vocab_size Integer. Passed to [build_vocab()] when `vocab` is `NULL`.
#' @param pos_filter Character vector of POS tags. Passed to [build_vocab()] when
#'   `vocab` is `NULL`.
#' @param window Character. Co-occurrence window; only `"sentence"` is supported.
#'
#' @return A named list with:
#'   \describe{
#'     \item{`cooc_matrix`}{Symmetric sparse \code{\link[Matrix]{Matrix}}
#'       (`dgCMatrix`); cell \eqn{[i,j]} = number of sentences in which terms
#'       \eqn{i} and \eqn{j} co-occur.}
#'     \item{`presence`}{Named integer vector \eqn{a_t}: the number of sentences
#'       containing each vocabulary term.}
#'     \item{`vocab`}{Character vector of vocabulary terms (matrix row/col order).}
#'   }
#'
#' @examples
#' df <- data.frame(
#'   doc_id = c(1, 1, 1, 1, 1), sentence_id = c(1, 1, 1, 2, 2),
#'   lemma = c("dog", "cat", "dog", "bird", "cat"), upos = "NOUN"
#' )
#' res <- build_cooccurrence_matrix(df, vocab_size = 10)
#' res$cooc_matrix
#'
#' @export
build_cooccurrence_matrix <- function(tokens_df,
                                      vocab      = NULL,
                                      vocab_size = 1500,
                                      pos_filter = c("NOUN", "ADJ", "PROPN"),
                                      window     = "sentence") {
  validate_tokens_df(tokens_df)

  if (!identical(window, "sentence")) {
    cli::cli_abort(c(
      "x" = "Only the {.val sentence} window is currently supported.",
      "i" = "Got: {.val {window}}."
    ))
  }

  if (is.null(vocab)) {
    vocab <- build_vocab(tokens_df, vocab_size = vocab_size, pos_filter = pos_filter)
  }

  n <- length(vocab)
  if (n < 2) {
    cli::cli_abort("Vocabulary must contain at least 2 terms.")
  }

  term_idx <- stats::setNames(seq_len(n), vocab)
  word_col <- if ("lemma" %in% names(tokens_df)) "lemma" else "token"

  filtered <- tokens_df[!is.na(tokens_df$upos) & tokens_df$upos %in% pos_filter, ]
  filtered <- filtered[!is.na(filtered[[word_col]]), ]
  filtered <- filtered[filtered[[word_col]] %in% vocab, ]

  if (nrow(filtered) == 0) {
    cli::cli_abort("No tokens in vocab remain after POS filtering.")
  }

  filtered$sent_key <- paste(filtered$doc_id, filtered$sentence_id, sep = "__")
  sentences <- split(filtered[[word_col]], filtered$sent_key)

  # Unique terms per sentence (the co-occurrence unit)
  uniq_per_sent <- lapply(sentences, function(x) unique(x[x %in% vocab]))

  # Presence a_t = number of sentences containing each term
  presence <- tabulate(term_idx[unlist(uniq_per_sent, use.names = FALSE)], nbins = n)
  names(presence) <- vocab

  # Co-occurrence pairs (i < j) accumulated as sparse triplets
  row_idx_list <- vector("list", length(uniq_per_sent))
  col_idx_list <- vector("list", length(uniq_per_sent))

  for (s_i in seq_along(uniq_per_sent)) {
    terms_in_sent <- uniq_per_sent[[s_i]]
    m <- length(terms_in_sent)
    if (m < 2) next
    idx   <- term_idx[terms_in_sent]
    pairs <- which(lower.tri(matrix(0, m, m)), arr.ind = TRUE)
    row_idx_list[[s_i]] <- idx[pairs[, 1]]
    col_idx_list[[s_i]] <- idx[pairs[, 2]]
  }

  all_rows <- unlist(row_idx_list)
  all_cols <- unlist(col_idx_list)

  if (length(all_rows) == 0) {
    cooc <- Matrix::sparseMatrix(i = integer(0), j = integer(0), x = numeric(0),
                                 dims = c(n, n), dimnames = list(vocab, vocab))
    return(list(cooc_matrix = cooc, presence = presence, vocab = vocab))
  }

  cooc_upper <- Matrix::sparseMatrix(
    i = all_rows, j = all_cols, x = rep(1L, length(all_rows)),
    dims = c(n, n), dimnames = list(vocab, vocab)
  )
  cooc_matrix <- cooc_upper + Matrix::t(cooc_upper)

  list(cooc_matrix = cooc_matrix, presence = presence, vocab = vocab)
}


#' Compute Association Strength from a co-occurrence matrix
#'
#' Normalises raw sentence co-occurrence counts using the Association Strength
#' measure (ThemeScope papers, Eq. 1):
#' \deqn{AS(t,t') = \frac{a_{t,t'}}{a_t \cdot a_{t'}},}
#' where \eqn{a_{t,t'}} is the number of co-occurring sentences and \eqn{a_t} the
#' presence (sentence count) of term \eqn{t}. Values lie in \eqn{[0,1]}; higher
#' values indicate stronger association, reducing the bias of highly frequent
#' terms.
#'
#' @param cooc_matrix Symmetric sparse \code{Matrix} of co-occurrence counts, as
#'   returned by [build_cooccurrence_matrix()].
#' @param presence Named numeric vector of term presence \eqn{a_t}. Names must
#'   match the row/column names of `cooc_matrix`.
#'
#' @return A sparse `dgCMatrix` of Association Strength values, same dimensions
#'   as `cooc_matrix`.
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

  a_safe <- as.numeric(presence)
  a_safe[a_safe == 0] <- NA_real_

  cm <- methods::as(methods::as(cooc_matrix, "dgCMatrix"), "CsparseMatrix")

  nz <- Matrix::which(cm != 0, arr.ind = TRUE)
  if (nrow(nz) == 0) return(cm)

  rows <- nz[, 1]
  cols <- nz[, 2]
  vals <- cm[nz]

  denom   <- a_safe[rows] * a_safe[cols]
  as_vals <- ifelse(!is.na(denom) & denom > 0, vals / denom, 0)

  Matrix::sparseMatrix(
    i = rows, j = cols, x = as_vals,
    dims = dim(cm), dimnames = dimnames(cm)
  )
}


#' Build a thresholded semantic co-occurrence network
#'
#' Constructs an undirected weighted \code{igraph} graph from an Association
#' Strength matrix, retaining only edges whose AS value exceeds the
#' `threshold_percentile` quantile of all non-zero AS values.
#'
#' @param as_matrix Symmetric sparse \code{Matrix} of Association Strength
#'   values, from [compute_association_strength()].
#' @param threshold_percentile Numeric in \eqn{(0, 1)}. Quantile of non-zero AS
#'   values used as the minimum edge weight (default `0.98`). This is an
#'   implementation choice for network sparsification, not part of the metric
#'   definitions; tune it for your corpus.
#' @param verbose Logical. Print progress messages (default `TRUE`).
#'
#' @return An undirected weighted \code{igraph} object: `E(graph)$weight` holds
#'   AS values and `V(graph)$name` the term labels. Only terms with at least one
#'   retained edge appear as vertices.
#'
#' @examples
#' \dontrun{
#' net <- build_cooccurrence_network(as_mat, threshold_percentile = 0.98)
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

  upper   <- Matrix::triu(as_matrix, k = 1)
  nz_vals <- upper@x[upper@x > 0]
  if (length(nz_vals) == 0) {
    cli::cli_abort("No non-zero values found in {.arg as_matrix}.")
  }

  threshold <- stats::quantile(nz_vals, probs = threshold_percentile, na.rm = TRUE)
  themescope_progress(
    "AS threshold ({threshold_percentile * 100}th percentile): {round(threshold, 8)}",
    verbose
  )

  upper_thresh <- upper
  upper_thresh@x[upper_thresh@x <= threshold] <- 0
  upper_thresh <- Matrix::drop0(upper_thresh)

  edge_positions <- Matrix::which(upper_thresh > 0, arr.ind = TRUE)
  if (nrow(edge_positions) == 0) {
    cli::cli_abort(c(
      "x" = "No edges remain after applying the AS threshold.",
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
