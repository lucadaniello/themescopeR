#' Detect communities in a semantic network
#'
#' Runs walktrap or Louvain community detection on a weighted undirected graph
#' and filters out small communities.
#'
#' @param graph An undirected weighted \code{igraph} object, e.g. from
#'   [build_cooccurrence_network()].
#' @param algorithm Character. `"walktrap"` (default) or `"louvain"`.
#' @param steps Integer. Number of steps for the walktrap random walk (default
#'   `4`). Ignored when `algorithm = "louvain"`.
#' @param min_size Integer. Minimum number of members for a community to be
#'   retained; smaller communities are assigned `NA` membership (default `10`).
#' @param seed Optional integer. If supplied, sets the random seed before
#'   community detection for reproducibility (Louvain is stochastic).
#' @param verbose Logical. Print a progress message (default `TRUE`).
#'
#' @return A named list with:
#'   \describe{
#'     \item{`membership`}{Named integer vector mapping each vertex to its
#'       community ID (`NA` for vertices in removed small communities).}
#'     \item{`communities`}{Named list of character vectors of member names, one
#'       per retained community (`"C1"`, `"C2"`, ...).}
#'     \item{`algorithm_result`}{The raw \code{communities} object from igraph.}
#'   }
#'
#' @examples
#' \dontrun{
#' comm <- detect_communities(net, algorithm = "walktrap", min_size = 10, seed = 1)
#' }
#'
#' @export
detect_communities <- function(graph,
                               algorithm = "walktrap",
                               steps     = 4,
                               min_size  = 10,
                               seed      = NULL,
                               verbose   = TRUE) {
  if (!igraph::is_igraph(graph)) {
    cli::cli_abort("{.arg graph} must be an {.cls igraph} object.")
  }
  algorithm <- match.arg(algorithm, c("walktrap", "louvain"))
  if (!is.numeric(steps) || steps < 1) {
    cli::cli_abort("{.arg steps} must be a positive integer.")
  }
  if (!is.numeric(min_size) || min_size < 1) {
    cli::cli_abort("{.arg min_size} must be a positive integer.")
  }

  steps    <- as.integer(steps)
  min_size <- as.integer(min_size)

  if (!is.null(seed)) set.seed(as.integer(seed))

  wt <- if (algorithm == "louvain") {
    igraph::cluster_louvain(graph, weights = igraph::E(graph)$weight)
  } else {
    igraph::cluster_walktrap(graph, steps = steps, weights = igraph::E(graph)$weight)
  }

  raw_membership <- igraph::membership(wt)
  vertex_names   <- igraph::V(graph)$name

  comm_sizes  <- table(raw_membership)
  valid_comms <- as.integer(names(comm_sizes[comm_sizes >= min_size]))
  relabel_map <- stats::setNames(seq_along(valid_comms), valid_comms)

  new_membership <- raw_membership
  new_membership[] <- NA_integer_
  for (old_id in valid_comms) {
    new_membership[which(raw_membership == old_id)] <- relabel_map[as.character(old_id)]
  }
  names(new_membership) <- vertex_names

  n_comms <- length(valid_comms)
  communities <- lapply(seq_len(n_comms), function(k) {
    vertex_names[which(new_membership == k)]
  })
  names(communities) <- paste0("C", seq_len(n_comms))

  n_removed <- sum(comm_sizes < min_size)
  themescope_progress(
    paste0("Community detection: ", n_comms, " retained (min size = ", min_size,
           "); ", n_removed, " small communities removed."),
    verbose = verbose
  )

  list(membership = new_membership, communities = communities, algorithm_result = wt)
}


#' Extract community subgraphs
#'
#' For each retained community, induces the subgraph of its vertices and their
#' interconnecting edges.
#'
#' @param graph An \code{igraph} object (the full network).
#' @param membership Named integer vector of community assignments from
#'   [detect_communities()]. `NA` values are ignored.
#'
#' @return A named list of \code{igraph} objects, one per community
#'   (`"C1"`, `"C2"`, ...).
#'
#' @examples
#' \dontrun{
#' subgraphs <- get_community_subgraphs(net, comm$membership)
#' }
#'
#' @export
get_community_subgraphs <- function(graph, membership) {
  if (!igraph::is_igraph(graph)) {
    cli::cli_abort("{.arg graph} must be an {.cls igraph} object.")
  }
  if (!is.numeric(membership)) {
    cli::cli_abort("{.arg membership} must be a numeric/integer vector.")
  }

  comm_ids <- sort(unique(membership[!is.na(membership)]))
  subgraphs <- lapply(comm_ids, function(cid) {
    members <- names(membership)[!is.na(membership) & membership == cid]
    igraph::induced_subgraph(graph, vids = members)
  })
  names(subgraphs) <- paste0("C", comm_ids)
  subgraphs
}
