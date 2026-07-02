#' Compute the Prototypical Salience Index (PSI) per community
#'
#' The PSI operationalises *anchoring* in Social Representation Theory: the
#' degree to which a thematic community is structurally embedded in the wider
#' discourse. Following the ThemeScope papers it is defined as
#' \deqn{\Psi(g_i) = \frac{\sum_{t \in g_i} a_t \cdot s_t}{\max_j \delta(g_j)},}
#' where \eqn{a_t} is the *presence* of term \eqn{t} (the number of sentences
#' containing it), \eqn{s_t = \frac{\sum_{t' \in N_i(t)} AS_{t,t'}}{|N_i(t)|}} is
#' the *mean association strength* of \eqn{t}'s neighbours within the community
#' subgraph, and \eqn{\delta(g_i) = \frac{2 W_i}{|V_i|(|V_i|-1)}} is the weighted
#' internal density of community \eqn{i} (\eqn{W_i} = sum of internal edge
#' weights).
#'
#' @details
#' This implementation follows the equations in the ThemeScope manuscript
#' (Eqs. 2--4) and case study (Eq. 2): the per-term contribution uses the
#' **mean neighbour association strength** \eqn{s_t} (not the raw degree) and the
#' normalisation uses the **maximum weighted community density** (not the maximum
#' numerator). Because the denominator is a single global constant, the
#' subsequent z-scoring used for the ThemeScope map is unaffected by it.
#'
#' @param graph An undirected weighted \code{igraph} object (the full network).
#'   Edge weights are the Association Strength values.
#' @param communities Named list of character vectors of community members, as
#'   returned by [detect_communities()].
#' @param presence Named numeric vector of term presence \eqn{a_t} (sentence
#'   counts), as returned in the `presence` element of
#'   [build_cooccurrence_matrix()]. Names must match vertex names in `graph`.
#'
#' @return A named numeric vector of PSI values, one per community.
#'
#' @examples
#' \dontrun{
#' psi <- compute_psi(net, comm$communities, cooc$presence)
#' }
#'
#' @export
compute_psi <- function(graph, communities, presence) {
  if (!igraph::is_igraph(graph)) {
    cli::cli_abort("{.arg graph} must be an {.cls igraph} object.")
  }
  if (!is.list(communities) || length(communities) == 0) {
    cli::cli_abort("{.arg communities} must be a non-empty named list.")
  }
  if (!is.numeric(presence)) {
    cli::cli_abort("{.arg presence} must be a numeric vector.")
  }

  per_comm <- lapply(communities, function(members) {
    sub_g <- igraph::induced_subgraph(graph, vids = members)
    w     <- igraph::E(sub_g)$weight
    if (is.null(w)) w <- rep(1, igraph::ecount(sub_g))

    deg      <- igraph::degree(sub_g)
    strength <- igraph::strength(sub_g, weights = w)
    s_t      <- ifelse(deg > 0, strength / deg, 0)   # mean neighbour AS

    a_t <- presence[names(s_t)]
    a_t[is.na(a_t)] <- 0

    # Numerator D(g_i) = sum_t a_t * s_t
    D_i <- sum(a_t * s_t, na.rm = TRUE)

    # Weighted internal density delta(g_i) = 2 W_i / (|V_i| (|V_i| - 1))
    V_i <- igraph::vcount(sub_g)
    W_i <- sum(w)
    delta_i <- if (V_i > 1) 2 * W_i / (V_i * (V_i - 1)) else NA_real_

    c(D = D_i, delta = delta_i)
  })

  D_values     <- vapply(per_comm, function(x) x[["D"]], numeric(1))
  delta_values <- vapply(per_comm, function(x) x[["delta"]], numeric(1))

  max_delta <- suppressWarnings(max(delta_values, na.rm = TRUE))
  if (!is.finite(max_delta) || max_delta == 0) {
    cli::cli_warn("Maximum community density is zero or undefined; PSI values set to 0.")
    return(stats::setNames(rep(0, length(communities)), names(communities)))
  }

  D_values / max_delta
}


#' Compute the Concreteness Score (CS) per community
#'
#' The CS operationalises *objectification*: the degree to which a community is
#' grounded in concrete, perceptually accessible content. It is the
#' association-strength-weighted mean concreteness of edge endpoints
#' \deqn{C_w(g_i) = \frac{\sum_{(t,t') \in E_i} w_{t,t'} \cdot \frac{c(t)+c(t')}{2}}{\sum_{(t,t') \in E_i} w_{t,t'}},}
#' where \eqn{w_{t,t'}} is the AS edge weight and \eqn{c(t)} the concreteness
#' rating. Only edges where **both** endpoints have a rating contribute.
#'
#' @param graph An undirected weighted \code{igraph} object with a `weight` edge
#'   attribute.
#' @param communities Named list of character vectors of community members.
#' @param concreteness_lexicon Data frame with columns `"word"` and `"conc.m"`
#'   (1--5 scale). Defaults to the bundled [brysbaert] norms.
#'
#' @return A named numeric vector of CS values. Communities where no edge has
#'   both endpoints in the lexicon return `NA`.
#'
#' @examples
#' \dontrun{
#' cs <- compute_cs(net, comm$communities)
#' }
#'
#' @export
compute_cs <- function(graph, communities, concreteness_lexicon = brysbaert) {
  if (!igraph::is_igraph(graph)) {
    cli::cli_abort("{.arg graph} must be an {.cls igraph} object.")
  }
  if (!is.list(communities) || length(communities) == 0) {
    cli::cli_abort("{.arg communities} must be a non-empty named list.")
  }
  if (!is.data.frame(concreteness_lexicon)) {
    cli::cli_abort("{.arg concreteness_lexicon} must be a data frame.")
  }
  required_cols <- c("word", "conc.m")
  missing_cols <- setdiff(required_cols, names(concreteness_lexicon))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Concreteness lexicon missing column{?s}: {.field {missing_cols}}.")
  }

  conc_lookup <- stats::setNames(
    concreteness_lexicon$conc.m,
    tolower(concreteness_lexicon$word)
  )

  vapply(communities, function(members) {
    sub_g <- igraph::induced_subgraph(graph, vids = members)
    if (igraph::ecount(sub_g) == 0) return(NA_real_)

    el  <- igraph::as_edgelist(sub_g, names = TRUE)
    wts <- igraph::E(sub_g)$weight
    if (is.null(wts)) wts <- rep(1, nrow(el))

    ci <- conc_lookup[tolower(el[, 1])]
    cj <- conc_lookup[tolower(el[, 2])]

    valid <- !is.na(ci) & !is.na(cj)
    if (!any(valid)) return(NA_real_)

    mean_conc <- (ci[valid] + cj[valid]) / 2
    w_valid   <- wts[valid]
    sum(w_valid * mean_conc) / sum(w_valid)
  }, numeric(1))
}


#' Concreteness lexicon coverage diagnostics
#'
#' Reports how many terms have a rating in a concreteness lexicon — overall and,
#' for a [themescope()] result, per community. A low coverage means the
#' Concreteness Score (CS) is computed on few edges and may be unreliable; this
#' typically happens when the corpus language does not match the lexicon (the
#' bundled [brysbaert] norms cover **English** words only).
#'
#' @param x Either a `themescope` object (coverage is reported per community and
#'   for the whole network) or a character vector of terms.
#' @param lexicon Data frame with columns `"word"` and `"conc.m"`. Defaults to
#'   the bundled [brysbaert] norms.
#'
#' @return A data frame with columns `community` (community id, or `"(all)"` for
#'   the overall row), `n_terms`, `n_matched` and `coverage` (proportion in
#'   \eqn{[0, 1]}).
#'
#' @seealso [compute_cs()], [match_concreteness()].
#'
#' @examples
#' lex <- data.frame(word = c("dog", "cat"), conc.m = c(4.8, 4.7))
#' lexicon_coverage(c("dog", "cat", "freedom"), lex)
#'
#' @export
lexicon_coverage <- function(x, lexicon = brysbaert) {
  coverage_row <- function(label, terms) {
    matched <- sum(!is.na(match_concreteness(terms, lexicon)))
    data.frame(
      community = label,
      n_terms   = length(terms),
      n_matched = matched,
      coverage  = if (length(terms) > 0) matched / length(terms) else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  if (inherits(x, "themescope")) {
    rows <- lapply(names(x$communities), function(cid) {
      coverage_row(cid, x$communities[[cid]])
    })
    out <- rbind(do.call(rbind, rows), coverage_row("(all)", igraph::V(x$graph)$name))
  } else if (is.character(x)) {
    out <- coverage_row("(all)", x)
  } else {
    cli::cli_abort("{.arg x} must be a {.cls themescope} object or a character vector of terms.")
  }
  rownames(out) <- NULL
  out
}


#' Compute term relevance within communities
#'
#' Implements the ThemeScope case-study term-relevance measure (Eq. 4), which
#' combines lexical salience with the balance between a term's internal and
#' external connectivity:
#' \deqn{R_t(g_i) = \log(1 + a_t) \cdot \frac{s_t^{in}(g_i)}{s_t^{in}(g_i) + s_t^{out}(g_i)},}
#' where \eqn{a_t} is term presence, \eqn{s_t^{in}} the total association
#' strength linking \eqn{t} to terms within its own community, and
#' \eqn{s_t^{out}} the total linking it to terms outside. Higher values identify
#' terms that are both frequent and predominantly embedded within their
#' community, downweighting generic terms shared across clusters.
#'
#' @param graph An undirected weighted \code{igraph} object.
#' @param membership Named integer vector of community assignments (`NA` for
#'   unassigned vertices), as returned by [detect_communities()].
#' @param presence Named numeric vector of term presence \eqn{a_t}.
#'
#' @return A data frame with columns `term`, `community` (e.g. `"C1"`),
#'   `relevance`, `degree`, `presence`, ordered by community then decreasing
#'   relevance. Unassigned vertices are dropped.
#'
#' @examples
#' \dontrun{
#' rel <- term_relevance(net, comm$membership, cooc$presence)
#' }
#'
#' @export
term_relevance <- function(graph, membership, presence) {
  if (!igraph::is_igraph(graph)) {
    cli::cli_abort("{.arg graph} must be an {.cls igraph} object.")
  }
  if (!is.numeric(membership)) {
    cli::cli_abort("{.arg membership} must be a numeric/integer vector.")
  }

  vnames   <- igraph::V(graph)$name
  comm_ids <- membership[vnames]
  deg      <- igraph::degree(graph)

  el <- igraph::as_edgelist(graph, names = TRUE)
  w  <- igraph::E(graph)$weight
  if (is.null(w)) w <- rep(1, nrow(el))

  # Accumulate in/out association strength per node, vectorised over the edge
  # list: stack both endpoints of every edge and sum the weights with rowsum().
  ca <- comm_ids[el[, 1]]
  cb <- comm_ids[el[, 2]]
  same <- !is.na(ca) & !is.na(cb) & ca == cb

  ends2 <- c(el[, 1], el[, 2])
  w2    <- c(w, w)
  same2 <- c(same, same)

  s_in  <- stats::setNames(numeric(length(vnames)), vnames)
  s_out <- stats::setNames(numeric(length(vnames)), vnames)
  if (any(same2)) {
    acc <- rowsum(w2[same2], ends2[same2])
    s_in[rownames(acc)] <- acc[, 1]
  }
  if (any(!same2)) {
    acc <- rowsum(w2[!same2], ends2[!same2])
    s_out[rownames(acc)] <- acc[, 1]
  }

  a_t <- presence[vnames]
  a_t[is.na(a_t)] <- 0
  denom <- s_in + s_out
  rel   <- ifelse(denom > 0, log1p(a_t) * (s_in / denom), 0)

  keep <- !is.na(comm_ids)
  out <- data.frame(
    term      = vnames[keep],
    community = paste0("C", comm_ids[keep]),
    relevance = as.numeric(rel[keep]),
    degree    = as.integer(deg[vnames][keep]),
    presence  = as.numeric(a_t[keep]),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$community, -out$relevance), , drop = FALSE]
  rownames(out) <- NULL
  out
}


#' Top terms per community
#'
#' Returns the highest-ranked terms of each community, used to label and
#' interpret the thematic clusters. Terms can be ranked by three criteria:
#' \describe{
#'   \item{`"relevance"` (default)}{the ThemeScope case-study relevance measure
#'     \eqn{R_t(g_i)} ([term_relevance()]), which combines lexical salience with
#'     a term's internal-versus-external connectivity. This is the labelling
#'     method described in the ThemeScope case study: it downweights generic,
#'     high-frequency terms that co-occur across several clusters and favours
#'     terms that are both frequent and structurally embedded in their own
#'     community.}
#'   \item{`"frequency"`}{the term presence \eqn{a_t} (the number of sentences,
#'     or documents, in which the term occurs) — the salience component of
#'     \eqn{R_t} on its own, i.e. plain frequency-based labelling.}
#'   \item{`"degree"`}{the network degree of the term (number of co-occurrence
#'     links).}
#' }
#'
#' @param x A `themescope` object (from [themescope()]).
#' @param n Integer. Number of terms to return per community (default `10`).
#' @param by Ranking criterion: `"relevance"` (default, \eqn{R_t}),
#'   `"frequency"` (term presence \eqn{a_t}) or `"degree"`.
#'
#' @return A data frame with columns `community`, `rank`, `term`, `relevance`,
#'   `frequency` (term presence \eqn{a_t}) and `degree`, ordered by community and
#'   then by the chosen ranking criterion (decreasing).
#'
#' @seealso [term_relevance()] for the relevance measure \eqn{R_t}.
#'
#' @examples
#' \dontrun{
#' top_terms(result, n = 10)                  # by relevance R_t (default)
#' top_terms(result, n = 10, by = "frequency") # by term frequency a_t
#' }
#'
#' @export
top_terms <- function(x, n = 10, by = c("relevance", "frequency", "degree")) {
  by <- match.arg(by)
  if (!inherits(x, "themescope")) {
    cli::cli_abort("{.arg x} must be a {.cls themescope} object.")
  }

  rel <- term_relevance(x$graph, x$membership, x$presence)
  # "frequency" ranks by term presence a_t (the salience term of R_t); the
  # data frame column carrying a_t is `presence`.
  ord_col <- switch(by,
    relevance = "relevance",
    frequency = "presence",
    degree    = "degree"
  )

  comms <- names(x$communities)
  rows <- lapply(comms, function(cid) {
    sub <- rel[rel$community == cid, , drop = FALSE]
    # Primary key = chosen criterion; ties broken by relevance then term name
    # for a deterministic order.
    sub <- sub[order(-sub[[ord_col]], -sub$relevance, sub$term), , drop = FALSE]
    sub <- utils::head(sub, n)
    if (nrow(sub) == 0) return(NULL)
    data.frame(
      community = cid,
      rank      = seq_len(nrow(sub)),
      term      = sub$term,
      relevance = sub$relevance,
      frequency = sub$presence,
      degree    = sub$degree,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


#' Compute network-level and community-level statistics
#'
#' @param graph An \code{igraph} object.
#' @param communities Named list of character vectors of community members.
#'
#' @return A list with `community_stats` (data frame: `community`, `size`,
#'   `density`, `mean_degree`, `n_edges`) and `global_stats` (named list:
#'   `n_nodes`, `n_edges`, `mean_degree`, `modularity`, `n_communities`).
#'
#' @examples
#' \dontrun{
#' compute_network_stats(net, comm$communities)$global_stats
#' }
#'
#' @export
compute_network_stats <- function(graph, communities) {
  if (!igraph::is_igraph(graph)) {
    cli::cli_abort("{.arg graph} must be an {.cls igraph} object.")
  }
  if (!is.list(communities)) {
    cli::cli_abort("{.arg communities} must be a list.")
  }

  n_comms <- length(communities)

  community_stats <- do.call(rbind, lapply(seq_len(n_comms), function(k) {
    members <- communities[[k]]
    sub_g   <- igraph::induced_subgraph(graph, vids = members)
    sz      <- igraph::vcount(sub_g)
    data.frame(
      community   = names(communities)[k],
      size        = sz,
      density     = igraph::edge_density(sub_g, loops = FALSE),
      mean_degree = if (sz > 0) mean(igraph::degree(sub_g)) else 0,
      n_edges     = igraph::ecount(sub_g),
      stringsAsFactors = FALSE
    )
  }))

  all_vertices <- igraph::V(graph)$name
  membership_vec <- stats::setNames(rep(NA_integer_, length(all_vertices)), all_vertices)
  for (k in seq_len(n_comms)) membership_vec[communities[[k]]] <- k

  has_comm <- !is.na(membership_vec)
  modularity_val <- if (sum(has_comm) > 1) {
    subg_all <- igraph::induced_subgraph(graph, vids = all_vertices[has_comm])
    tryCatch(
      igraph::modularity(subg_all, membership = membership_vec[has_comm],
                         weights = igraph::E(subg_all)$weight),
      error = function(e) NA_real_
    )
  } else {
    NA_real_
  }

  global_stats <- list(
    n_nodes       = igraph::vcount(graph),
    n_edges       = igraph::ecount(graph),
    mean_degree   = mean(igraph::degree(graph)),
    modularity    = modularity_val,
    n_communities = n_comms
  )

  list(community_stats = community_stats, global_stats = global_stats)
}


#' Compute edge-density coherence per community
#'
#' Coherence is the ratio of actual to maximum possible edges within a community
#' (its edge density): \eqn{|E_i| / (|G_i|(|G_i|-1)/2)}.
#'
#' @param graph An \code{igraph} object.
#' @param communities Named list of character vectors of community members.
#'
#' @return Named numeric vector of coherence values in \eqn{[0, 1]}. Communities
#'   with fewer than 2 members return `NA`.
#'
#' @examples
#' \dontrun{
#' compute_coherence(net, comm$communities)
#' }
#'
#' @export
compute_coherence <- function(graph, communities) {
  if (!igraph::is_igraph(graph)) {
    cli::cli_abort("{.arg graph} must be an {.cls igraph} object.")
  }
  if (!is.list(communities)) {
    cli::cli_abort("{.arg communities} must be a list.")
  }

  vapply(communities, function(members) {
    if (length(members) < 2) return(NA_real_)
    sub_g <- igraph::induced_subgraph(graph, vids = members)
    igraph::edge_density(sub_g, loops = FALSE)
  }, numeric(1))
}
