# Internal: does this data frame already look like annotated words/tokens?
.is_words_df <- function(df) {
  all(c("doc_id", "sentence_id", "upos") %in% names(df)) &&
    any(c("token", "lemma") %in% names(df))
}


#' Run the full ThemeScope analysis pipeline
#'
#' @description
#' Executes the complete ThemeScope workflow in a single call. Accepts **either**
#' an annotated words data frame **or** a raw document collection (in which case
#' it is annotated with \pkg{udpipe} via [preprocess_texts()] first). The steps:
#' \enumerate{
#'   \item (Optional) annotate raw texts into words.
#'   \item Build the vocabulary from POS-filtered words ([build_vocab()]).
#'   \item Build the co-occurrence matrix + term presence, with the chosen
#'     `normalization` ([build_cooccurrence_matrix()]).
#'   \item Construct a thresholded network ([build_cooccurrence_network()]).
#'   \item Detect communities (walktrap / louvain / leiden).
#'   \item Compute PSI (anchoring) and CS (objectification) per community.
#'   \item Collect network statistics.
#' }
#'
#' @param data Either an annotated words data frame (columns `doc_id`,
#'   `sentence_id`, `upos`, and `token`/`lemma`) or a raw collection (a `text`
#'   and `doc_id` column, e.g. from [read_collection()]).
#' @param model Required only when `data` is a raw collection: a \pkg{udpipe}
#'   model object, a path to a `.udpipe` file, or a language name (see
#'   [preprocess_texts()]).
#' @param text_col,doc_id_col Column names used when annotating a raw collection.
#' @param unit Word unit for the vocabulary: `"lemma"` (default) or `"token"`.
#' @param concreteness_lexicon Data frame with columns `"word"` and `"conc.m"`.
#'   Defaults to the bundled [brysbaert] lexicon. Pass `NULL` to skip CS.
#' @param vocab_size Integer or `NULL`. Maximum vocabulary size (`NULL` = all).
#' @param pos_filter Character vector of Universal POS tags (default
#'   `c("NOUN", "ADJ", "PROPN")`).
#' @param window Co-occurrence unit: `"sentence"` (default) or `"document"`.
#' @param normalization Similarity measure for the co-occurrence matrix:
#'   `"association"` (default), `"equivalence"`, `"jaccard"`, `"salton"`,
#'   `"inclusion"` or `"frequency"`. See [normalize_cooccurrence()].
#' @param threshold_percentile Numeric in \eqn{(0, 1)}; network edge threshold
#'   (default `0.98`).
#' @param community_algorithm `"walktrap"` (default), `"louvain"` or `"leiden"`.
#' @param walktrap_steps Integer (default `4`); used by walktrap only.
#' @param resolution Numeric resolution for louvain/leiden (default `1`).
#' @param min_community_size Integer (default `10`).
#' @param seed Optional integer seed for reproducible community detection.
#' @param verbose Logical. Print progress messages (default `TRUE`).
#'
#' @return An S3 object of class `"themescope"` with elements `graph`,
#'   `communities`, `membership`, `psi`, `cs`, `presence`, `network_stats`,
#'   `vocab` (a [build_vocab()] data frame), `params`, and `call`.
#'
#' @examples
#' # The bundled demo corpus is shipped already tokenised and POS tagged
#' words  <- readRDS(system.file("extdata", "demo_annotated.rds",
#'                               package = "themescopeR"))
#' result <- themescope(words, vocab_size = 300, seed = 1, verbose = FALSE)
#' result
#' as.data.frame(result)
#'
#' \donttest{
#' # Straight from raw texts: annotated internally, which downloads a udpipe
#' # language model on first use (kept in the session temporary directory here)
#' coll  <- read_collection(system.file("extdata", "sample_collection.csv",
#'                                      package = "themescopeR"))
#' model <- try(ts_download_model("english", model_dir = tempdir()), silent = TRUE)
#' if (!inherits(model, "try-error")) {
#'   res <- themescope(utils::head(coll, 100), model = model, vocab_size = 100,
#'                     threshold_percentile = 0.9, min_community_size = 3,
#'                     seed = 1, verbose = FALSE)
#'   plot(res, type = "map")
#' }
#' }
#'
#' @export
themescope <- function(data,
                       model                = NULL,
                       text_col             = "text",
                       doc_id_col           = "doc_id",
                       unit                 = c("lemma", "token"),
                       concreteness_lexicon = brysbaert,
                       vocab_size           = 1500,
                       pos_filter           = c("NOUN", "ADJ", "PROPN"),
                       window               = c("sentence", "document"),
                       normalization        = "association",
                       threshold_percentile = 0.98,
                       community_algorithm  = "walktrap",
                       walktrap_steps       = 4,
                       resolution           = 1,
                       min_community_size   = 10,
                       seed                 = NULL,
                       verbose              = TRUE) {

  call   <- match.call()
  unit   <- match.arg(unit)
  window <- match.arg(window)

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame (annotated words or a raw collection).")
  }

  # ---- 0. Annotate raw texts if needed ----
  if (.is_words_df(data)) {
    words_df <- data
  } else if (text_col %in% names(data)) {
    if (is.null(model)) {
      cli::cli_abort(c(
        "x" = "{.arg data} looks like a raw collection but no {.arg model} was supplied.",
        "i" = "Pass a language (e.g. {.code model = \"english\"}) or a .udpipe path."
      ))
    }
    themescope_progress("Step 0: Annotating raw texts ...", verbose)
    words_df <- preprocess_texts(data, model = model, text_col = text_col,
                                 doc_id_col = doc_id_col, verbose = verbose)
  } else {
    cli::cli_abort(c(
      "x" = "{.arg data} is neither an annotated words data frame nor a raw collection.",
      "i" = "Words need {.field doc_id}, {.field sentence_id}, {.field upos} + {.field token}/{.field lemma}; a collection needs a {.field {text_col}} column."
    ))
  }

  validate_words_df(words_df)

  # ---- 1. Vocabulary ----
  themescope_progress("Step 1/6: Building vocabulary ...", verbose)
  vocab <- build_vocab(words_df, unit = unit, vocab_size = vocab_size, pos_filter = pos_filter)
  themescope_progress(paste0("  Vocabulary size: ", nrow(vocab), " terms."), verbose)

  # ---- 2. Co-occurrence + normalisation ----
  themescope_progress("Step 2/6: Building co-occurrence matrix ...", verbose)
  cooc <- build_cooccurrence_matrix(words_df, vocab = vocab, unit = unit,
                                    pos_filter = pos_filter, window = window,
                                    normalization = normalization)

  # ---- 3. Network ----
  themescope_progress("Step 3/6: Building network ...", verbose)
  graph <- build_cooccurrence_network(cooc$cooc_matrix,
                                      threshold_percentile = threshold_percentile,
                                      verbose = verbose)

  # ---- 4. Communities ----
  themescope_progress("Step 4/6: Detecting communities ...", verbose)
  comm <- detect_communities(graph, algorithm = community_algorithm,
                             steps = walktrap_steps, resolution = resolution,
                             min_size = min_community_size, seed = seed, verbose = verbose)
  communities <- comm$communities
  membership  <- comm$membership

  if (length(communities) == 0) {
    cli::cli_abort(c(
      "x" = "No communities with >= {min_community_size} members were found.",
      "i" = "Try lowering {.arg min_community_size} or {.arg threshold_percentile}."
    ))
  }

  # ---- 5. PSI ----
  themescope_progress("Step 5/6: Computing PSI ...", verbose)
  psi <- compute_psi(graph, communities, cooc$presence)

  # ---- 6. CS ----
  if (!is.null(concreteness_lexicon)) {
    themescope_progress("Step 6/6: Computing CS ...", verbose)
    cs <- compute_cs(graph, communities, concreteness_lexicon)
    cov_all <- mean(!is.na(match_concreteness(igraph::V(graph)$name, concreteness_lexicon)))
    if (is.finite(cov_all) && cov_all < 0.5) {
      cli::cli_warn(c(
        "!" = "Only {round(cov_all * 100)}% of network terms have a concreteness rating.",
        "i" = "CS may be unreliable. Check that the lexicon language matches the corpus (the bundled Brysbaert norms are English-only); see {.fn lexicon_coverage}."
      ))
    }
  } else {
    themescope_progress("Step 6/6: No concreteness lexicon -- CS set to NA.", verbose)
    cs <- stats::setNames(rep(NA_real_, length(communities)), names(communities))
  }

  network_stats <- compute_network_stats(graph, communities)

  params <- list(
    unit                 = unit,
    vocab_size           = vocab_size,
    pos_filter           = pos_filter,
    window               = window,
    normalization        = cooc$normalization,
    threshold_percentile = threshold_percentile,
    community_algorithm  = community_algorithm,
    walktrap_steps       = walktrap_steps,
    resolution           = resolution,
    min_community_size   = min_community_size,
    seed                 = seed
  )

  structure(
    list(
      graph         = graph,
      communities   = communities,
      membership    = membership,
      psi           = psi,
      cs            = cs,
      presence      = cooc$presence,
      network_stats = network_stats,
      vocab         = vocab,
      params        = params,
      call          = call
    ),
    class = "themescope"
  )
}


#' @describeIn themescope Print a concise summary of a `themescope` object.
#' @param x A `themescope` object.
#' @param ... Ignored.
#' @export
print.themescope <- function(x, ...) {
  n_comm <- length(x$communities)
  cli::cli_h1("ThemeScope Analysis")
  cli::cli_bullets(c(
    "*" = "Network: {igraph::vcount(x$graph)} nodes, {igraph::ecount(x$graph)} edges",
    "*" = "Communities: {n_comm}",
    "*" = "Vocabulary size: {nrow(x$vocab)} terms (unit: {x$params$unit})"
  ))

  if (n_comm > 0) {
    comm_sizes <- vapply(x$communities, length, integer(1))
    psi_z <- zscore(x$psi)
    cs_z  <- suppressWarnings(zscore(x$cs))
    quads <- assign_quadrant(psi_z, cs_z)
    cli::cli_h2("Communities")
    for (k in seq_len(n_comm)) {
      cname <- names(x$communities)[k]
      psi_v <- round(x$psi[cname], 4)
      cs_v  <- if (!is.na(x$cs[cname])) round(x$cs[cname], 3) else "NA"
      qv    <- if (!is.na(quads[k])) as.character(quads[k]) else "N/A"
      cli::cli_alert_info("{cname} (n={comm_sizes[k]}): PSI={psi_v}, CS={cs_v} [{qv}]")
    }
  }
  invisible(x)
}


#' @describeIn themescope Detailed summary with a community statistics table.
#' @param object A `themescope` object.
#' @export
summary.themescope <- function(object, ...) {
  x  <- object
  df <- as.data.frame(x)

  cli::cli_h1("ThemeScope Analysis -- Summary")
  cli::cli_h2("Parameters")
  cli::cli_bullets(c(
    "*" = "Word unit: {x$params$unit}",
    "*" = "Vocabulary size: {x$params$vocab_size}",
    "*" = "POS filter: {paste(x$params$pos_filter, collapse = ', ')}",
    "*" = "Co-occurrence window: {x$params$window}",
    "*" = "Normalization: {x$params$normalization}",
    "*" = "AS threshold percentile: {x$params$threshold_percentile}",
    "*" = "Community algorithm: {x$params$community_algorithm}",
    "*" = "Min community size: {x$params$min_community_size}"
  ))
  cli::cli_h2("Network")
  gs <- x$network_stats$global_stats
  cli::cli_bullets(c(
    "*" = "Nodes: {gs$n_nodes}",
    "*" = "Edges: {gs$n_edges}",
    "*" = "Mean degree: {round(gs$mean_degree, 2)}",
    "*" = "Modularity: {round(gs$modularity, 4)}",
    "*" = "Communities: {gs$n_communities}"
  ))
  cli::cli_h2("Community Table")
  print(df)
  invisible(df)
}


#' @describeIn themescope Plot a `themescope` object (`type = "map"` or
#'   `"network"`). With `label = "terms"`, communities on the map are labelled
#'   with their most representative terms (up to `n_label_terms`), chosen with
#'   the `label_by` method, instead of the community id; communities keep the
#'   same colour in both views.
#' @param type Character. `"map"` (default) or `"network"`.
#' @param label Map point labels: `"id"` (community id, default) or `"terms"`
#'   (the top terms of each community).
#' @param label_by Ranking used to pick the terms when `label = "terms"`:
#'   `"relevance"` (default, \eqn{R_t}), `"frequency"` (term presence) or
#'   `"degree"`. Passed to [top_terms()].
#' @param n_label_terms Integer. Number of terms per label when `label = "terms"`
#'   (default `3`).
#' @export
plot.themescope <- function(x, type = c("map", "network"),
                            label = c("id", "terms"),
                            label_by = c("relevance", "frequency", "degree"),
                            n_label_terms = 3, ...) {
  type     <- match.arg(type)
  label    <- match.arg(label)
  label_by <- match.arg(label_by)

  comm_names <- names(x$communities)
  palette <- .themescope_palette(length(comm_names))

  if (type == "map") {
    community_labels <- NULL
    if (label == "terms") {
      tt <- top_terms(x, n = n_label_terms, by = label_by)
      community_labels <- vapply(comm_names, function(cid) {
        paste(tt$term[tt$community == cid], collapse = ", ")
      }, character(1))
      names(community_labels) <- comm_names
    }
    plot_themescope(
      psi              = x$psi,
      cs               = x$cs,
      community_labels = community_labels,
      community_sizes  = vapply(x$communities, length, integer(1)),
      palette          = palette,
      ...
    )
  } else {
    plot_network(graph = x$graph, membership = x$membership, palette = palette, ...)
  }
}


#' @describeIn themescope Coerce to a tidy community-level data frame (raw and
#'   z-scored metrics plus quadrant).
#' @export
as.data.frame.themescope <- function(x, ...) {
  comm_names <- names(x$communities)
  sizes      <- vapply(x$communities, length, integer(1))
  psi_z <- zscore(x$psi)
  cs_z  <- suppressWarnings(zscore(x$cs))
  quad  <- assign_quadrant(psi_z, cs_z)

  data.frame(
    community_id = comm_names,
    label        = comm_names,
    size         = as.integer(sizes),
    psi          = as.numeric(x$psi[comm_names]),
    cs           = as.numeric(x$cs[comm_names]),
    psi_z        = as.numeric(psi_z),
    cs_z         = as.numeric(cs_z),
    quadrant     = quad,
    stringsAsFactors = FALSE
  )
}
