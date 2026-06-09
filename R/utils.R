#' Compute z-scores
#'
#' Computes standardised (z-scored) values, handling `NA`s gracefully.
#'
#' @param x Numeric vector to standardise.
#'
#' @return Numeric vector of z-scores. `NA` values in the input remain `NA`.
#'   Returns a vector of `NA`s (with a warning) if the standard deviation is zero.
#'
#' @examples
#' zscore(c(1, 2, 3, 4, 5))
#' zscore(c(1, NA, 3))
#'
#' @export
zscore <- function(x) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.arg x} must be a numeric vector.")
  }
  mu <- mean(x, na.rm = TRUE)
  sigma <- stats::sd(x, na.rm = TRUE)
  if (is.na(sigma) || sigma == 0) {
    cli::cli_warn("Standard deviation is zero or NA; returning a vector of NAs.")
    return(rep(NA_real_, length(x)))
  }
  (x - mu) / sigma
}


#' Assign quadrant labels from z-scored PSI and CS
#'
#' Maps communities to the four quadrants of the ThemeScope representational
#' space based on their z-scored Prototypical Salience Index (PSI, anchoring) and
#' Concreteness Score (CS, objectification).
#'
#' @param psi_z Numeric vector of z-scored PSI values.
#' @param cs_z Numeric vector of z-scored CS values (same length as `psi_z`).
#'
#' @return A factor with levels:
#'   \itemize{
#'     \item `"Stable Core"`: high PSI, high CS.
#'     \item `"Ideological Core"`: high PSI, low CS.
#'     \item `"Emerging Practices"`: low PSI, high CS.
#'     \item `"Latent Representations"`: low PSI, low CS.
#'   }
#'
#' @examples
#' assign_quadrant(c(1, 1, -1, -1), c(1, -1, 1, -1))
#'
#' @export
assign_quadrant <- function(psi_z, cs_z) {
  if (!is.numeric(psi_z) || !is.numeric(cs_z)) {
    cli::cli_abort("{.arg psi_z} and {.arg cs_z} must be numeric vectors.")
  }
  if (length(psi_z) != length(cs_z)) {
    cli::cli_abort("{.arg psi_z} and {.arg cs_z} must have the same length.")
  }

  quadrant_levels <- c(
    "Stable Core",
    "Ideological Core",
    "Emerging Practices",
    "Latent Representations"
  )

  result <- dplyr::case_when(
    psi_z >= 0 & cs_z >= 0 ~ "Stable Core",
    psi_z >= 0 & cs_z < 0  ~ "Ideological Core",
    psi_z < 0  & cs_z >= 0 ~ "Emerging Practices",
    psi_z < 0  & cs_z < 0  ~ "Latent Representations",
    TRUE                   ~ NA_character_
  )

  factor(result, levels = quadrant_levels)
}


#' Match terms to a concreteness lexicon
#'
#' Looks up concreteness ratings for a set of terms. Matching is
#' case-insensitive.
#'
#' @param terms Character vector of terms to look up.
#' @param lexicon Data frame with columns `"word"` (character) and `"conc.m"`
#'   (numeric mean concreteness on a 1--5 scale). Defaults to the bundled
#'   [brysbaert] norms.
#'
#' @return Named numeric vector of concreteness values aligned to `terms`. Terms
#'   not found in the lexicon receive `NA`.
#'
#' @examples
#' lex <- data.frame(word = c("dog", "cat", "freedom"), conc.m = c(4.8, 4.7, 1.5))
#' match_concreteness(c("dog", "freedom", "unknown"), lex)
#'
#' @export
match_concreteness <- function(terms, lexicon = brysbaert) {
  if (!is.character(terms)) {
    cli::cli_abort("{.arg terms} must be a character vector.")
  }
  if (!is.data.frame(lexicon)) {
    cli::cli_abort("{.arg lexicon} must be a data frame.")
  }
  required_cols <- c("word", "conc.m")
  missing_cols <- setdiff(required_cols, names(lexicon))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Lexicon is missing required column{?s}: {.field {missing_cols}}.")
  }

  lookup <- stats::setNames(lexicon$conc.m, tolower(lexicon$word))
  conc_values <- lookup[tolower(terms)]
  names(conc_values) <- terms
  conc_values
}


#' Validate an annotated words data frame
#'
#' Checks that an annotated data frame has all columns required by the
#' ThemeScope backend. The "word" used downstream is either the `token` or the
#' `lemma` column (chosen via the `unit` argument of [build_vocab()] /
#' [build_cooccurrence_matrix()]), so at least one of them must be present.
#' Emits informative errors via \pkg{cli} when columns are missing.
#'
#' @param words_df Data frame to validate. Must contain `doc_id`,
#'   `sentence_id`, `upos`, and at least one of `token` or `lemma`.
#'
#' @return Invisibly returns `TRUE` if validation passes.
#'
#' @examples
#' df <- data.frame(doc_id = 1, sentence_id = 1, token = "dog", upos = "NOUN")
#' validate_words_df(df)
#'
#' @export
validate_words_df <- function(words_df) {
  if (!is.data.frame(words_df)) {
    cli::cli_abort(c(
      "x" = "{.arg words_df} must be a data frame.",
      "i" = "Got an object of class {.cls {class(words_df)}}."
    ))
  }

  required_always <- c("doc_id", "sentence_id", "upos")
  missing_always <- setdiff(required_always, names(words_df))
  if (length(missing_always) > 0) {
    cli::cli_abort(c(
      "x" = "{.arg words_df} is missing required column{?s}: {.field {missing_always}}.",
      "i" = "Required: {.field doc_id}, {.field sentence_id}, {.field upos}, and at least one of {.field token}/{.field lemma}."
    ))
  }

  if (!("token" %in% names(words_df)) && !("lemma" %in% names(words_df))) {
    cli::cli_abort(c(
      "x" = "{.arg words_df} must contain at least one of {.field token} or {.field lemma}.",
      "i" = "Found columns: {.field {names(words_df)}}."
    ))
  }

  if (nrow(words_df) == 0) {
    cli::cli_abort("{.arg words_df} must not be empty.")
  }

  invisible(TRUE)
}


#' Emit a progress message (if verbose)
#'
#' Thin wrapper around [cli::cli_alert_info()] that respects a verbosity flag.
#' Used internally throughout the pipeline.
#'
#' @param msg Character string. Supports \pkg{cli} inline markup.
#' @param verbose Logical. If `FALSE`, the message is suppressed.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
#' @export
themescope_progress <- function(msg, verbose = TRUE) {
  if (isTRUE(verbose)) {
    # Interpolate cli/glue markup in the caller's environment, not here.
    cli::cli_alert_info(msg, .envir = parent.frame())
  }
  invisible(NULL)
}
