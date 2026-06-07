# Internal: does this data frame already look like annotated tokens?
.is_tokens_df <- function(df) {
  all(c("doc_id", "sentence_id", "upos") %in% names(df)) &&
    any(c("token", "lemma") %in% names(df))
}


#' Run the full ThemeScope analysis pipeline
#'
#' @description
#' Executes the complete ThemeScope workflow in a single call. Accepts **either**
#' a pre-annotated tokens data frame **or** a raw document collection (in which
#' case it is annotated with \pkg{udpipe} via [preprocess_texts()] first). The
#' steps are:
#' \enumerate{
#'   \item (Optional) annotate raw texts into tokens.
#'   \item Build the vocabulary from POS-filtered tokens.
#'   \item Build the sentence-level co-occurrence matrix and term presence.
#'   \item Normalise with Association Strength.
#'   \item Construct a thresholded co-occurrence network.
#'   \item Detect communities (walktrap by default).
#'   \item Compute PSI (anchoring) per community.
#'   \item Compute CS (objectification) per community.
#'   \item Collect network statistics.
#' }
#'
#' @param data Either a tokens data frame (columns `doc_id`, `sentence_id`,
#'   `upos`, and `token`/`lemma`) or a raw collection (a `text` and `doc_id`
#'   column, e.g. from [read_collection()]).
#' @param model Required only when `data` is a raw collection: a \pkg{udpipe}
#'   model object, a path to a `.udpipe` file, or a language name (see
#'   [preprocess_texts()]).
#' @param text_col,doc_id_col Column names used when annotating a raw collection.
#' @param concreteness_lexicon Data frame with columns `"word"` and `"conc.m"`.
#'   Defaults to the bundled [brysbaert] lexicon. Pass `NULL` to skip CS.
#' @param vocab_size Integer. Maximum vocabulary size (default `1500`).
#' @param pos_filter Character vector of Universal POS tags (default
#'   `c("NOUN", "ADJ", "PROPN")`).
#' @param threshold_percentile Numeric in \eqn{(0, 1)}; AS network threshold
#'   (default `0.98`).
#' @param community_algorithm `"walktrap"` (default) or `"louvain"`.
#' @param walktrap_steps Integer (default `4`); ignored for Louvain.
#' @param min_community_size Integer (default `10`).
#' @param seed Optional integer seed for reproducible community detection.
#' @param verbose Logical. Print progress messages (default `TRUE`).
#'
#' @return An S3 object of class `"themescope"` with elements `graph`,
#'   `communities`, `membership`, `psi`, `cs`, `presence`, `network_stats`,
#'   `vocab`, `params`, and `call`.
#'
#' @examples
#' \dontrun{
#' # From a pre-annotated tokens data frame
#' result <- themescope(tokens_df, vocab_size = 1000)
#' print(result)
#' plot(result, type = "map")
#'
#' # From raw texts (annotated internally)
#' coll   <- read_collection("sample_collection.csv")
#' result <- themescope(coll, model = "english")
#' }
#'
#' @export
themescope <- function(data,
                       model                = NULL,
                       text_col             = "text",
                       doc_id_col           = "doc_id",
                       concreteness_lexicon = brysbaert,
                       vocab_size           = 1500,
                       pos_filter           = c("NOUN", "ADJ", "PROPN"),
                       threshold_percentile = 0.98,
                       community_algorithm  = "walktrap",
                       walktrap_steps       = 4,
                       min_community_size   = 10,
                       seed                 = NULL,
                       verbose              = TRUE) {

  call <- match.call()

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame (tokens or a raw collection).")
  }

  # ---- 0. Annotate raw texts if needed ----
  if (.is_tokens_df(data)) {
    tokens_df <- data
  } else if (text_col %in% names(data)) {
    if (is.null(model)) {
      cli::cli_abort(c(
        "x" = "{.arg data} looks like a raw collection but no {.arg model} was supplied.",
        "i" = "Pass a language (e.g. {.code model = \"english\"}) or a .udpipe path."
      ))
    }
    themescope_progress("Step 0: Annotating raw texts ...", verbose)
    tokens_df <- preprocess_texts(data, model = model, text_col = text_col,
                                  doc_id_col = doc_id_col, verbose = verbose)
  } else {
    cli::cli_abort(c(
      "x" = "{.arg data} is neither a tokens data frame nor a raw collection.",
      "i" = "Tokens need {.field doc_id}, {.field sentence_id}, {.field upos} + {.field token}/{.field lemma}; a collection needs a {.field {text_col}} column."
    ))
  }

  validate_tokens_df(tokens_df)

  # ---- 1. Vocabulary ----
  themescope_progress("Step 1/7: Building vocabulary ...", verbose)
  vocab <- build_vocab(tokens_df, vocab_size = vocab_size, pos_filter = pos_filter)
  themescope_progress(paste0("  Vocabulary size: ", length(vocab), " terms."), verbose)

  # ---- 2. Co-occurrence matrix + presence ----
  themescope_progress("Step 2/7: Building co-occurrence matrix ...", verbose)
  cooc <- build_cooccurrence_matrix(tokens_df, vocab = vocab, pos_filter = pos_filter,
                                    window = "sentence")

  # ---- 3. Association Strength ----
  themescope_progress("Step 3/7: Computing Association Strength ...", verbose)
  as_matrix <- compute_association_strength(cooc$cooc_matrix, cooc$presence)

  # ---- 4. Network ----
  themescope_progress("Step 4/7: Building network ...", verbose)
  graph <- build_cooccurrence_network(as_matrix, threshold_percentile = threshold_percentile,
                                      verbose = verbose)

  # ---- 5. Communities ----
  themescope_progress("Step 5/7: Detecting communities ...", verbose)
  comm <- detect_communities(graph, algorithm = community_algorithm,
                             steps = walktrap_steps, min_size = min_community_size,
                             seed = seed, verbose = verbose)
  communities <- comm$communities
  membership  <- comm$membership

  if (length(communities) == 0) {
    cli::cli_abort(c(
      "x" = "No communities with >= {min_community_size} members were found.",
      "i" = "Try lowering {.arg min_community_size} or {.arg threshold_percentile}."
    ))
  }

  # ---- 6. PSI ----
  themescope_progress("Step 6/7: Computing PSI ...", verbose)
  psi <- compute_psi(graph, communities, cooc$presence)

  # ---- 7. CS ----
  if (!is.null(concreteness_lexicon)) {
    themescope_progress("Step 7/7: Computing CS ...", verbose)
    cs <- compute_cs(graph, communities, concreteness_lexicon)
  } else {
    themescope_progress("Step 7/7: No concreteness lexicon -- CS set to NA.", verbose)
    cs <- stats::setNames(rep(NA_real_, length(communities)), names(communities))
  }

  network_stats <- compute_network_stats(graph, communities)

  params <- list(
    vocab_size           = vocab_size,
    pos_filter           = pos_filter,
    threshold_percentile = threshold_percentile,
    community_algorithm  = community_algorithm,
    walktrap_steps       = walktrap_steps,
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
  n_comm  <- length(x$communities)
  cli::cli_h1("ThemeScope Analysis")
  cli::cli_bullets(c(
    "*" = "Network: {igraph::vcount(x$graph)} nodes, {igraph::ecount(x$graph)} edges",
    "*" = "Communities: {n_comm}",
    "*" = "Vocabulary size: {length(x$vocab)} terms"
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
    "*" = "Vocabulary size: {x$params$vocab_size}",
    "*" = "POS filter: {paste(x$params$pos_filter, collapse = ', ')}",
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
#'   `"network"`).
#' @param type Character. `"map"` (default) or `"network"`.
#' @export
plot.themescope <- function(x, type = c("map", "network"), ...) {
  type <- match.arg(type)
  if (type == "map") {
    plot_themescope(psi = x$psi, cs = x$cs,
                    community_sizes = vapply(x$communities, length, integer(1)), ...)
  } else {
    plot_network(graph = x$graph, membership = x$membership, ...)
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
