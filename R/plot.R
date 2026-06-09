# Internal: a fixed palette of 40 pastel colours (no external package needed).
# Community i is drawn with colour i (recycled beyond 40). Used by BOTH the map
# and the network so that a community has the same colour in both views.
.themescope_palette <- function(n = NULL) {
  pal <- c(
    "#AEC6CF", "#FFB347", "#B39EB5", "#FF6961", "#77DD77", "#FDFD96", "#84B6F4",
    "#FDCAE1", "#CB99C9", "#C1E1C1", "#FFD1DC", "#B19CD9", "#F49AC2", "#FFB7CE",
    "#A2D2FF", "#BDE0FE", "#FFC8A2", "#E2F0CB", "#B5EAD7", "#C7CEEA", "#FFDAC1",
    "#FF9AA2", "#E0BBE4", "#D5AAFF", "#85E3FF", "#B9FBC0", "#FBE7C6", "#A0C4FF",
    "#FFADAD", "#FFD6A5", "#FDFFB6", "#CAFFBF", "#9BF6FF", "#BDB2FF", "#FFC6FF",
    "#D0F4DE", "#E4C1F9", "#F1C0E8", "#CFBAF0", "#98F5E1"
  )
  if (is.null(n)) return(pal)
  rep(pal, length.out = n)
}

#' ThemeScope pastel colour palette
#'
#' Returns the built-in palette of 40 pastel colours used to colour communities
#' consistently across [plot_themescope()] and [plot_network()].
#'
#' @param n Optional number of colours to return (recycled if `n > 40`).
#'
#' @return A character vector of hex colours.
#'
#' @examples
#' themescope_colours(5)
#'
#' @export
themescope_colours <- function(n = NULL) .themescope_palette(n)


#' Create a ThemeScope representational map
#'
#' Produces the two-dimensional strategic diagram locating each community in the
#' space defined by z-scored PSI (x-axis, anchoring) and z-scored CS (y-axis,
#' objectification). Points are **coloured by community** with the shared pastel
#' palette (so colours match [plot_network()]); the four SRT quadrants are
#' annotated in the corners. The only legend is community size (number of terms),
#' placed at the bottom.
#'
#' @param psi Named numeric vector of PSI values (one per community).
#' @param cs Named numeric vector of CS values (one per community); may contain
#'   `NA`.
#' @param community_labels Optional named character vector of point labels (e.g.
#'   top terms per community). If `NULL`, community names from `psi` are used.
#' @param community_sizes Optional named numeric vector of community sizes (the
#'   number of terms in each community) used to scale point area.
#' @param title Character. Plot title (default `"ThemeScope Map"`).
#' @param palette Optional vector of community colours (one per community, in the
#'   order of `psi`). Defaults to [themescope_colours()].
#' @param ... Currently unused.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @examples
#' plot_themescope(
#'   psi = c(C1 = 1.2, C2 = -0.5, C3 = 0.3),
#'   cs  = c(C1 = 0.8, C2 = -1.1, C3 = 0.2)
#' )
#'
#' @export
plot_themescope <- function(psi,
                            cs,
                            community_labels = NULL,
                            community_sizes  = NULL,
                            title            = "ThemeScope Map",
                            palette          = NULL,
                            ...) {
  if (!is.numeric(psi)) cli::cli_abort("{.arg psi} must be numeric.")
  if (!is.numeric(cs))  cli::cli_abort("{.arg cs} must be numeric.")

  comm_names <- names(psi)
  if (is.null(comm_names)) comm_names <- paste0("C", seq_along(psi))

  psi_z <- zscore(psi)
  cs_z  <- if (all(is.na(cs))) rep(0, length(cs)) else suppressWarnings(zscore(cs))

  labels <- if (!is.null(community_labels)) community_labels[comm_names] else comm_names
  labels[is.na(labels)] <- comm_names[is.na(labels)]

  sizes <- if (!is.null(community_sizes)) as.numeric(community_sizes[comm_names]) else rep(4, length(comm_names))
  sizes[is.na(sizes)] <- 4

  if (is.null(palette)) palette <- .themescope_palette(length(comm_names))
  col_map <- stats::setNames(palette[seq_along(comm_names)], comm_names)

  plot_df <- data.frame(
    community = factor(comm_names, levels = comm_names),
    label     = as.character(labels),
    psi_z     = psi_z,
    cs_z      = cs_z,
    size      = sizes,
    stringsAsFactors = FALSE
  )

  # Quadrant corner annotations (text only; no colour legend)
  x_range <- range(c(psi_z, 0), na.rm = TRUE)
  y_range <- range(c(cs_z, 0), na.rm = TRUE)
  xpad <- diff(x_range) * 0.14 + 0.2
  ypad <- diff(y_range) * 0.14 + 0.2
  quad_df <- data.frame(
    x = c(x_range[2] + xpad, x_range[2] + xpad, x_range[1] - xpad, x_range[1] - xpad),
    y = c(y_range[2] + ypad, y_range[1] - ypad, y_range[2] + ypad, y_range[1] - ypad),
    label = c("Stable Core", "Ideological Core", "Emerging Practices", "Latent Representations"),
    hjust = c(1, 1, 0, 0),
    stringsAsFactors = FALSE
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$psi_z, y = .data$cs_z)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.4) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(size = .data$size, fill = .data$community),
                        shape = 21, colour = "grey30", stroke = 0.4, alpha = 0.95) +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = .data$label),
      size = 3.1, colour = "grey15", fontface = "bold",
      max.overlaps = Inf, box.padding = 0.6, point.padding = 0.3,
      min.segment.length = 0, force = 3, force_pull = 0.5,
      segment.colour = "grey70", segment.size = 0.3, seed = 42
    ) +
    ggplot2::annotate("text", x = quad_df$x, y = quad_df$y, label = quad_df$label,
                      hjust = quad_df$hjust, size = 3, colour = "grey45", fontface = "italic") +
    ggplot2::scale_fill_manual(values = col_map, guide = "none") +
    ggplot2::scale_size_continuous(name = "Community size (terms)", range = c(3, 13)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.18)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.18)) +
    ggplot2::labs(
      title = title,
      x = expression(italic("PSI")["z"] ~ "(Prototypical Salience - anchoring)"),
      y = expression(italic("CS")["z"] ~ "(Concreteness - objectification)")
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", hjust = 0.5),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom"
    )
}


#' Plot a community-coloured semantic network (igraph)
#'
#' Draws the co-occurrence network with \pkg{igraph}: nodes are coloured by
#' community using the shared pastel palette (matching [plot_themescope()]),
#' edges are light grey, node size scales with degree, and the highest-degree
#' terms of each community are labelled.
#'
#' @param graph An \code{igraph} object.
#' @param membership Named integer vector of community assignments from
#'   [detect_communities()] (`NA` for unassigned vertices).
#' @param palette Optional vector of community colours (community `i` uses
#'   `palette[i]`). Defaults to [themescope_colours()].
#' @param layout Character layout: `"fr"` (default), `"kk"`, `"dh"`, `"lgl"`.
#' @param top_n_labels Integer. Highest-degree nodes per community to label
#'   (default `8`).
#' @param edge_color Colour of the edges (default light grey).
#' @param seed Optional integer seed for a reproducible layout.
#' @param ... Passed to [igraph::plot.igraph()].
#'
#' @return Invisibly `NULL`; called for the plot it draws.
#'
#' @examples
#' \dontrun{
#' plot_network(net, comm$membership, top_n_labels = 5)
#' }
#'
#' @export
plot_network <- function(graph,
                         membership,
                         palette      = NULL,
                         layout       = "fr",
                         top_n_labels = 8,
                         edge_color   = "grey85",
                         seed         = NULL,
                         ...) {
  if (!igraph::is_igraph(graph)) {
    cli::cli_abort("{.arg graph} must be an {.cls igraph} object.")
  }

  vertex_names <- igraph::V(graph)$name
  comm_ids     <- membership[vertex_names]
  deg          <- igraph::degree(graph)

  if (is.null(palette)) {
    n_comms <- suppressWarnings(max(comm_ids, na.rm = TRUE))
    palette <- .themescope_palette(if (is.finite(n_comms)) n_comms else 1)
  }
  node_col <- ifelse(is.na(comm_ids), "grey85", palette[comm_ids])

  # Label the top-degree nodes of each community
  label_nodes <- character(0)
  for (cid in sort(unique(comm_ids[!is.na(comm_ids)]))) {
    cn  <- names(comm_ids)[!is.na(comm_ids) & comm_ids == cid]
    top <- names(sort(deg[cn], decreasing = TRUE))[seq_len(min(top_n_labels, length(cn)))]
    label_nodes <- c(label_nodes, top)
  }
  node_labels <- ifelse(vertex_names %in% label_nodes, vertex_names, "")

  layout_fn <- switch(layout,
    fr = igraph::layout_with_fr, kk = igraph::layout_with_kk,
    dh = igraph::layout_with_dh, lgl = igraph::layout_with_lgl,
    igraph::layout_with_fr
  )
  if (!is.null(seed)) set.seed(as.integer(seed))
  lyt <- layout_fn(graph)

  igraph::plot.igraph(
    graph,
    layout             = lyt,
    vertex.color       = node_col,
    vertex.frame.color = "grey50",
    vertex.size        = 4 + log1p(deg[vertex_names]) * 2.5,
    vertex.label       = node_labels,
    vertex.label.cex   = 0.7,
    vertex.label.color = "grey15",
    vertex.label.family = "sans",
    edge.color         = edge_color,
    edge.width         = 0.6,
    main               = "Semantic Co-occurrence Network",
    ...
  )
  invisible(NULL)
}
