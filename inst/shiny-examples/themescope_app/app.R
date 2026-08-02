# ==============================================================================
# themescopeR: Shiny GUI
#
# This app is a *thin* graphical front-end: all computation is delegated to the
# exported package functions
#   read_collection() -> preprocess_texts() -> themescope() -> top_terms()
#   save_themescope() / read_themescope() for the .themescope archive
# The helpers below only turn the returned `themescope` object into interactive
# widgets (plotly / visNetwork / DT); they do not re-implement any analysis
# logic. Communities use themescope_colours() so the map, the network and the
# top-terms bars share the exact colours of the console plots.
# Launch it with themescopeR::themescope_app().
#
# Interaction model (step-by-step, by design):
#   1. Import      pick the data type, browse, and Import. The demo corpus and
#                  a saved .themescope archive arrive already annotated.
#   2. Preprocess  udpipe tokenize + POS (unlocks the rest of the pipeline).
#   3. Run         build the network, detect communities and compute metrics.
#   4. Refine      change the look of the network without recomputing anything.
# ==============================================================================

library(themescopeR)
library(shiny)
library(bslib)
library(DT)
library(plotly)
library(visNetwork)
library(shinycssloaders)

# ---- Upload size limit -------------------------------------------------------
# Shiny's default upload cap is 5 MB, far too small for ThemeScope collections
# (tens of MB to several GB). We raise a comfortable baseline here so typical
# large files load without friction; the "Expected file size" input in the
# import panel lets users raise it further before browsing.
# themescope_app(max_upload_size_mb = ...) can override the baseline from R.
# Shiny re-reads this option on every upload, so bumping it at runtime (before
# the file is picked) takes effect immediately.
.baseline_upload_mb <- 50L
if (is.null(getOption("shiny.maxRequestSize")) ||
    getOption("shiny.maxRequestSize") < .baseline_upload_mb * 1024^2) {
  options(shiny.maxRequestSize = .baseline_upload_mb * 1024^2)
}

# Client-side guard: a file that exceeds the current limit would fail to upload
# with a cryptic error. Instead we intercept the file *before* Shiny uploads it
# (capture-phase change listener + stopImmediatePropagation), ask the server to
# open a modal offering to raise the limit, and, once the user confirms, replay
# the selection so the file imports automatically. Uses single quotes only, so
# the raw string below stays valid.
.oversize_js <- r"(
(function(){
  var bypassNext = false;
  var lastId = 'file_upload';
  var watched = ['file_upload', 'archive_upload'];

  function limitMB(){
    var el = document.getElementById('expected_size_mb');
    var v = el ? parseFloat(el.value) : NaN;
    return isNaN(v) ? 50 : v;
  }

  // Capture-phase guard: runs before Shiny's own change handler. If the picked
  // file(s) exceed the limit we stop the upload and ask the server to prompt.
  document.addEventListener('change', function(e){
    var t = e.target;
    if (!t || watched.indexOf(t.id) === -1) return;
    if (bypassNext){ bypassNext = false; return; }
    if (!t.files || t.files.length === 0) return;
    var bytes = 0;
    for (var i = 0; i < t.files.length; i++) bytes += t.files[i].size;
    var mb = bytes / (1024 * 1024);
    var lim = limitMB();
    if (mb > lim){
      e.stopImmediatePropagation();
      e.preventDefault();
      lastId = t.id;
      if (window.Shiny) Shiny.setInputValue('oversize_detected',
        {size_mb: Math.round(mb * 10) / 10, limit_mb: lim, recommend: Math.ceil(mb) + 10, nonce: Date.now()},
        {priority: 'event'});
    }
  }, true);

  // After the server raised the limit, replay the upload. The browser's native
  // selection is still in input.files (we only stopped the event, not the
  // selection), so we just re-dispatch change and let it through once.
  function registerResume(){
    if (!window.Shiny || !Shiny.addCustomMessageHandler) return;
    Shiny.addCustomMessageHandler('resume_upload', function(msg){
      var input = document.getElementById(lastId);
      if (!input || !input.files || input.files.length === 0){
        Shiny.setInputValue('resume_failed', Date.now(), {priority: 'event'});
        return;
      }
      bypassNext = true;
      input.dispatchEvent(new Event('change', {bubbles: true}));
    });
  }
  if (window.Shiny && Shiny.addCustomMessageHandler) registerResume();
  else document.addEventListener('shiny:connected', registerResume);
})();
)"

# App styling: the analysis-unit radios in the navbar, the card toolbars (whose
# select and button must share one height and baseline) and the author portraits.
.navbar_css <- r"(
.ts-unit-toggle .btn { color: #fff; border-color: rgba(255,255,255,0.55);
  padding: 0.12rem 0.7rem; font-weight: 500; margin-bottom: 0; }
.ts-unit-toggle .btn-check:checked + .btn { background: #fff; color: #3A3A38;
  border-color: #fff; font-weight: 600; }
.navbar .container-fluid { flex-wrap: wrap; gap: 0.5rem; }
.ts-navlinks a, .ts-navlinks .action-button { text-decoration: none; }
.ts-navlinks a:hover, .ts-navlinks .action-button:hover { opacity: 0.75; }

/* Toolbars above the tables: every control the same height, aligned on one row */
.ts-toolbar { display: flex; align-items: center; gap: 0.5rem; flex-wrap: wrap; }
.ts-toolbar .shiny-input-container,
.ts-toolbar .form-group { margin-bottom: 0 !important; }
.ts-toolbar .form-select,
.ts-toolbar .form-control,
.ts-toolbar .btn { height: 2.35rem; padding-top: 0; padding-bottom: 0;
  line-height: 1.2; display: inline-flex; align-items: center; }
.ts-toolbar .btn { justify-content: center; }

/* Author portraits in the Authors window */
.ts-avatar { width: 60px; height: 60px; border-radius: 50%; object-fit: cover;
  object-position: 50% 18%; border: 2px solid #ECE6DE; }
)"

# ---- Citation ----------------------------------------------------------------
# Using themescopeR / ThemeScope requires citing the paper below.
.cite_doi_url   <- "https://doi.org/10.1177/01655515261454276"
.readme_url     <- "https://github.com/lucadaniello/themescopeR#readme"
.repo_url       <- "https://github.com/lucadaniello/themescopeR"
.paper_sage_url <- "https://journals.sagepub.com/doi/10.1177/01655515261454276"
.authors <- list(
  list(name = "Luca D'Aniello",
       role = "Author and maintainer, University of Naples Federico II",
       url  = "https://lucadaniello.github.io",
       photo = "author_daniello.jpg"),
  list(name = "Michelangelo Misuraca",
       role = "Author, University of Salerno",
       url  = "https://mmisuraca.it",
       photo = "author_misuraca.jpg"),
  list(name = "Maria Spano",
       role = "Author, University of Naples Federico II",
       url  = "https://mariaspano.github.io/homepage/",
       photo = "author_spano.jpg")
)
.cite_apa <- paste0(
  "Misuraca, M., Spano, M., & D'Aniello, L. (2026). ThemeScope: A ",
  "quantitative thematic analysis for depicting social representations in ",
  "digital arenas. Journal of Information Science. ", .cite_doi_url)
.cite_bibtex <- paste(
  "@article{misuraca2026themescope,",
  "  author  = {Misuraca, Michelangelo and Spano, Maria and D'Aniello, Luca},",
  "  title   = {ThemeScope: A quantitative thematic analysis for depicting social representations in digital arenas},",
  "  journal = {Journal of Information Science},",
  "  year    = {2026},",
  "  pages   = {01655515261454276},",
  "  doi     = {10.1177/01655515261454276},",
  "  url     = {https://doi.org/10.1177/01655515261454276}",
  "}",
  sep = "\n")

# ---- Demo dataset (bundled climate sample, shipped already annotated) --------
# The demo arrives tokenised, lemmatised and POS tagged, so the button loads the
# raw texts *and* their udpipe annotation: the demo skips straight to Run.
# The annotation is read on demand (see .demo_words()) to keep start-up quick.
.demo_path       <- system.file("extdata", "sample_collection.csv", package = "themescopeR")
.demo_words_path <- system.file("extdata", "demo_annotated.rds", package = "themescopeR")
.demo_coll  <- if (nzchar(.demo_path)) read_collection(.demo_path, sequential_ids = TRUE) else NULL
.demo_ndocs <- if (!is.null(.demo_coll)) nrow(.demo_coll) else 0L
.demo_cache <- NULL
.demo_words <- function() {
  if (!nzchar(.demo_words_path)) return(NULL)
  if (is.null(.demo_cache)) .demo_cache <<- readRDS(.demo_words_path)
  .demo_cache
}

# ---- Languages offered for annotation ----------------------------------------
.languages <- c("english", "italian", "french", "spanish", "german",
                "portuguese", "dutch")

# ---- Universal POS tags, organised for the filter -----------------------------
# Content words carry the thematic meaning ThemeScope maps; the remaining UPOS
# tags are offered as optional extras for special analyses.
.pos_content <- c("NOUN", "PROPN", "ADJ", "VERB", "ADV")
.pos_other   <- c("ADP", "AUX", "CCONJ", "DET", "INTJ", "NUM",
                  "PART", "PRON", "PUNCT", "SCONJ", "SYM", "X")

# ---- SRT quadrant colours (hover/table accents only; points use the palette) --
.quad_colors <- c("Stable Core"            = "#1D9E75",
                  "Ideological Core"       = "#EF9F27",
                  "Emerging Practices"     = "#7F77DD",
                  "Latent Representations" = "#D85A30",
                  "N/A"                    = "#aaaaaa")

.spinner <- function(out) withSpinner(out, type = 6, color = "#1D9E75")

# ---- Formula symbols ---------------------------------------------------------
# Rendered as real subscripts wherever they appear (labels, table headers,
# plot axes and hovers), so R_t and a_t never show up as plain underscores.
# Both Shiny and plotly understand this markup.
.html_relevance <- "Relevance <i>R</i><sub><i>t</i></sub>"
.html_frequency <- "Frequency <i>a</i><sub><i>t</i></sub>"

# DT escapes whatever is passed as `colnames`, so a header carrying markup (a
# subscript, for instance) has to be supplied as a container sketch instead.
# The filter row DT builds is unaffected.
.dt_header <- function(labels) {
  htmltools::withTags(table(
    class = "display",
    thead(tr(lapply(labels, function(l) th(HTML(l)))))))
}

# ---- Small UI helpers --------------------------------------------------------
# A muted "locked" note shown in place of pipeline controls until preprocessing
# is done (keeps the downstream menus visibly greyed/blocked).
.locked_note <- function(msg = "Complete preprocessing to unlock.") {
  div(class = "text-muted small py-2",
      icon("lock"), " ", msg)
}

# Case-insensitive text-column guess used to pre-select the column picker; falls
# back to the first label (as requested) when nothing obvious is found.
.detect_text_col <- function(nms) {
  cand <- c("text", "body", "content", "comment", "message")
  hit  <- nms[tolower(nms) %in% cand]
  if (length(hit)) hit[1] else nms[1]
}

# Peek at the column names of a tabular file without reading the whole thing, so
# we can offer a text-column selector after import (csv / tsv / xlsx / xls only).
.peek_colnames <- function(path) {
  ext <- tolower(tools::file_ext(path))
  tryCatch(
    switch(ext,
      csv  = names(readr::read_csv(path, n_max = 0, show_col_types = FALSE, progress = FALSE)),
      tsv  = names(readr::read_tsv(path, n_max = 0, show_col_types = FALSE, progress = FALSE)),
      xlsx = names(readxl::read_excel(path, n_max = 0)),
      xls  = names(readxl::read_excel(path, n_max = 0)),
      NULL),
    error = function(e) NULL)
}

# Fetch the updated-model registry (language + treebank + description). Needs the
# network on first call; cached afterwards. Returns NULL on failure (offline),
# in which case only the "Default" model is offered.
.safe_model_registry <- function() {
  tryCatch(themescopeR::ts_list_models(), error = function(e) NULL)
}

# ---- Helper: shared community palette (same colours as plot.themescope) ------
.community_palette <- function(result) {
  comm_names <- names(result$communities)
  stats::setNames(themescope_colours(length(comm_names)), comm_names)
}

# ---- Helper: ThemeScope map as plotly ----------------------------------------
build_map_plotly <- function(result, top3, label_terms = FALSE) {
  comm_names <- names(result$psi)
  comm_sizes <- vapply(result$communities, length, integer(1))
  pal        <- .community_palette(result)

  psi_z     <- zscore(result$psi)
  cs_all_na <- all(is.na(result$cs))
  cs_z      <- if (cs_all_na) rep(0, length(result$cs)) else suppressWarnings(zscore(result$cs))

  term_str <- vapply(comm_names, function(cid) {
    paste(top3$term[top3$community == cid], collapse = ", ")
  }, character(1))
  labels <- if (label_terms) term_str else comm_names

  quadrant <- as.character(assign_quadrant(psi_z, cs_z))
  quadrant[is.na(quadrant)] <- "N/A"
  node_size <- 14 + (comm_sizes / max(comm_sizes)) * 28

  hover <- paste0(
    "<b>", comm_names, "</b>: ", term_str, "<br>",
    "Size: ", comm_sizes, " terms<br>",
    "PSI (z): ", round(psi_z, 3), "<br>",
    if (cs_all_na) "" else paste0("CS (z): ", round(cs_z, 3), "<br>"),
    "Quadrant: <b style='color:", .quad_colors[quadrant], "'>", quadrant, "</b>"
  )

  xr <- range(c(psi_z, 0), na.rm = TRUE); yr <- range(c(cs_z, 0), na.rm = TRUE)
  xp <- diff(xr) * 0.25 + 0.1; yp <- diff(yr) * 0.25 + 0.1

  annot <- list(
    list(x = xr[2] + xp * .3, y = yr[2] + yp * .3, text = "<i>Stable Core</i>", showarrow = FALSE,
         font = list(color = .quad_colors[["Stable Core"]], size = 11)),
    list(x = xr[2] + xp * .3, y = yr[1] - yp * .3, text = "<i>Ideological Core</i>", showarrow = FALSE,
         font = list(color = .quad_colors[["Ideological Core"]], size = 11)),
    list(x = xr[1] - xp * .3, y = yr[2] + yp * .3, text = "<i>Emerging Practices</i>", showarrow = FALSE,
         font = list(color = .quad_colors[["Emerging Practices"]], size = 11)),
    list(x = xr[1] - xp * .3, y = yr[1] - yp * .3, text = "<i>Latent Representations</i>", showarrow = FALSE,
         font = list(color = .quad_colors[["Latent Representations"]], size = 11))
  )

  plot_ly(x = psi_z, y = cs_z, type = "scatter", mode = "markers+text",
          text = labels, textposition = "top center",
          hovertext = hover, hoverinfo = "text",
          marker = list(color = unname(pal[comm_names]), size = node_size,
                        line = list(color = "#5b6770", width = 1.2), opacity = 0.95),
          textfont = list(size = 11, color = "#2c3e50")) |>
    layout(
      xaxis = list(title = "<i>PSI<sub>z</sub></i>  (Prototypical Salience, anchoring)",
                   zeroline = TRUE, zerolinecolor = "#cccccc", showgrid = FALSE),
      yaxis = list(title = if (cs_all_na) "CS not available" else
                     "<i>CS<sub>z</sub></i>  (Concreteness, objectification)",
                   zeroline = TRUE, zerolinecolor = "#cccccc", showgrid = FALSE),
      shapes = list(
        list(type = "line", x0 = 0, x1 = 0, y0 = yr[1] - yp, y1 = yr[2] + yp,
             line = list(color = "#bbbbbb", width = 1, dash = "dash")),
        list(type = "line", y0 = 0, y1 = 0, x0 = xr[1] - xp, x1 = xr[2] + xp,
             line = list(color = "#bbbbbb", width = 1, dash = "dash"))),
      annotations = annot, showlegend = FALSE,
      plot_bgcolor = "white", paper_bgcolor = "white",
      margin = list(t = 30, b = 60, l = 70, r = 40)) |>
    config(displayModeBar = TRUE, displaylogo = FALSE)
}

# ---- Helper: top terms as plotly bars (community-coloured) --------------------
# `metric` selects which column drives the bar length / ordering:
#   "relevance" -> R_t (default), "frequency" -> term presence a_t.
build_topterms_plotly <- function(tt, pal, communities = NULL, metric = "relevance") {
  value_col   <- if (identical(metric, "frequency")) "frequency" else "relevance"
  value_title <- if (identical(metric, "frequency")) .html_frequency else .html_relevance
  hover_fmt   <- if (identical(metric, "frequency")) "%{x:.0f}" else "%{x:.3f}"

  tt <- tt[!is.na(tt$term), , drop = FALSE]
  if (!is.null(communities) && length(communities) > 0) {
    tt <- tt[tt$community %in% communities, , drop = FALSE]
  }
  comms <- intersect(names(pal), unique(tt$community))
  validate(need(length(comms) > 0, "No community selected."))

  # One chart per row (full width) so bars stretch and labels stay readable;
  # the caller paginates to at most three communities per page.
  ncols <- 1L
  nrows <- length(comms)

  plots <- lapply(comms, function(cid) {
    sub <- tt[tt$community == cid, , drop = FALSE]
    sub$value <- sub[[value_col]]
    sub <- sub[order(sub$value), , drop = FALSE]
    plot_ly(sub, x = ~value, y = ~reorder(term, value),
            type = "bar", orientation = "h",
            marker = list(color = pal[[cid]], opacity = 0.95,
                          line = list(color = "#5b6770", width = 0.5)),
            hovertemplate = paste0("<b>%{y}</b><br>", value_title, ": ", hover_fmt,
                                   "<br>Community: ", cid, "<extra></extra>"),
            showlegend = FALSE) |>
      layout(
        annotations = list(list(text = paste0("<b>", cid, "</b>"),
                                x = 0, y = 1.12, xref = "paper", yref = "paper",
                                xanchor = "left", yanchor = "bottom",
                                showarrow = FALSE,
                                font = list(size = 13, color = pal[[cid]]))),
        xaxis = list(title = value_title, showgrid = TRUE, gridcolor = "#eeeeee"),
        yaxis = list(title = "", automargin = TRUE))
  })

  # Generous vertical margin so each community title sits in the gap above its
  # chart instead of overlapping the bars / the axis title of the plot above.
  subplot(plots, nrows = nrows, shareX = FALSE, shareY = FALSE,
          titleX = TRUE, titleY = TRUE, margin = 0.09) |>
    layout(plot_bgcolor = "white", paper_bgcolor = "white",
           margin = list(t = 46)) |>
    config(displayModeBar = FALSE)
}

# ---- Helper: network as visNetwork --------------------------------------------
# Communities are laid out in R so they visibly cluster and separate, because
# vis.js force layouts have no notion of community and collapse everything into
# one blob. A Fruchterman-Reingold base layout is (1) pushed apart by community,
# each centroid moving away from the global centre in proportion to `repulsion`,
# and (2) rescaled so that two neighbouring nodes of the same community sit
# `spacing` node diameters apart. Because the scale is expressed in node
# diameters, the picture stays equally readable whatever the graph size, the
# node-size setting or the repulsion, and `repulsion` now genuinely controls how
# far apart the communities are (a fixed +/-600 box did the opposite: pushing one
# community out shrank all the others).
#
# The coordinates go to vis.js with **physics off**, so grouping and separation
# are preserved exactly. Every argument here is cosmetic: nothing is recomputed
# upstream, which is why the sidebar can redraw without re-running the analysis.
.network_defaults <- list(
  repulsion         = 0.3,
  spacing           = 1.2,   # gap between neighbours, in node diameters
  hide_unclassified = TRUE,  # unclassified terms are ~half the nodes and hide the rest
  label_size        = 16,
  node_scale        = 1,
  edge_opacity      = 0.65,
  show_labels       = TRUE,
  seed              = 1
)

# Internal: median nearest-neighbour distance among nodes of the same community,
# estimated on a sample so a big graph does not build a huge distance matrix.
.median_within_gap <- function(lay, comm_ids, max_n = 1200L) {
  idx <- which(!is.na(comm_ids))
  if (length(idx) > max_n) idx <- sample(idx, max_n)
  if (length(idx) < 3) return(NA_real_)
  d <- as.matrix(stats::dist(lay[idx, , drop = FALSE]))
  diag(d) <- Inf
  same <- outer(comm_ids[idx], comm_ids[idx], "==")
  diag(same) <- FALSE
  if (!any(same)) return(NA_real_)
  d[!same] <- Inf
  nn <- apply(d, 1, min)
  nn <- nn[is.finite(nn)]
  if (length(nn) == 0) return(NA_real_)
  stats::median(nn)
}

build_network_visnetwork <- function(result, opts = list()) {
  o <- utils::modifyList(.network_defaults, opts[!vapply(opts, is.null, logical(1))])
  clamp <- function(x, lo, hi, default) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) != 1 || is.na(x)) return(default)
    max(lo, min(hi, x))
  }
  repulsion  <- clamp(o$repulsion, 0, 1, 0.3)
  spacing    <- clamp(o$spacing, 0.4, 3, 1.2)
  label_size <- clamp(o$label_size, 6, 40, 16)
  node_scale <- clamp(o$node_scale, 0.4, 3, 1)
  edge_alpha <- clamp(o$edge_opacity, 0.05, 1, 0.65)
  seed       <- clamp(o$seed, 0, 1e6, 1)

  g    <- result$graph
  memb <- result$membership
  vnames   <- igraph::V(g)$name
  comm_ids <- memb[vnames]

  if (isTRUE(o$hide_unclassified)) {
    keep <- vnames[!is.na(comm_ids)]
    if (length(keep) > 0) {
      g <- igraph::induced_subgraph(g, vids = keep)
      vnames   <- igraph::V(g)$name
      comm_ids <- memb[vnames]
    }
  }
  deg <- igraph::degree(g)

  # Node radius as vis.js will draw it: `value` is mapped linearly onto
  # [scaling$min, scaling$max]. Unclassified terms stay at the minimum size.
  size_min <- 8 * node_scale
  size_max <- 40 * node_scale
  val  <- 10 + log1p(deg) * 5
  val[is.na(comm_ids)] <- min(val)
  rng  <- diff(range(val)); if (!is.finite(rng) || rng == 0) rng <- 1
  radius <- size_min + (val - min(val)) / rng * (size_max - size_min)

  # ---- community-aware layout ----
  set.seed(as.integer(seed))
  lay  <- igraph::layout_with_fr(g)
  cids <- unique(comm_ids[!is.na(comm_ids)])
  if (length(cids) > 0) {
    global_c <- colMeans(lay)
    sep      <- repulsion * 3 # push centroids outward (0 = none .. 3 = strong)
    for (cid in cids) {
      idx   <- which(!is.na(comm_ids) & comm_ids == cid)
      cen   <- colMeans(lay[idx, , drop = FALSE])
      shift <- (cen - global_c) * sep
      lay[idx, 1] <- lay[idx, 1] + shift[1]
      lay[idx, 2] <- lay[idx, 2] + shift[2]
    }
  }
  lay <- sweep(lay, 2, colMeans(lay))

  # Rescale so neighbours of the same community sit `spacing` diameters apart.
  gap <- .median_within_gap(lay, comm_ids)
  if (is.na(gap) || gap <= 0) {
    d <- as.matrix(stats::dist(lay)); diag(d) <- Inf
    gap <- suppressWarnings(stats::median(apply(d, 1, min)))
  }
  if (is.finite(gap) && gap > 0) {
    lay <- lay * (spacing * stats::median(2 * radius) / gap)
  }

  # Community i uses palette colour i, identical to the map and console plots.
  pal <- themescope_colours(max(c(comm_ids, 1L), na.rm = TRUE))

  nodes <- data.frame(
    id = vnames, label = if (isTRUE(o$show_labels)) vnames else "",
    title = paste0("<b>", vnames, "</b><br>Community: ",
                   ifelse(is.na(comm_ids), "none", paste0("C", comm_ids)),
                   "<br>Degree: ", deg),
    group = ifelse(is.na(comm_ids), "Unclassified", paste0("C", comm_ids)),
    value = as.numeric(val),
    # Unclassified terms fade into the background instead of masking the rest.
    color = ifelse(is.na(comm_ids), "rgba(190,190,190,0.45)", pal[comm_ids]),
    x = lay[, 1], y = lay[, 2],
    stringsAsFactors = FALSE)

  # Association-strength weights are extremely skewed (the median sits at a few
  # per cent of the maximum), so a min-max scale drew every edge as a 0.4 px
  # ghost. Ranking spreads them over the whole width and opacity range instead.
  el <- igraph::as_edgelist(g); w <- igraph::E(g)$weight
  wr <- if (length(w) > 0) rank(w, ties.method = "average") / length(w) else numeric(0)
  edges <- data.frame(from = el[, 1], to = el[, 2],
                      value = 0.8 + wr * 2.5,
                      color = sprintf("rgba(110,110,110,%.2f)",
                                      pmin(1, edge_alpha * (0.6 + 0.4 * wr))),
                      title = paste0("Weight: ", signif(w, 4)), stringsAsFactors = FALSE)

  visNetwork(nodes, edges, width = "100%", height = "100%") |>
    visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
               selectedBy = list(variable = "group", main = "Filter by community"),
               nodesIdSelection = list(enabled = TRUE, main = "Find a term")) |>
    visPhysics(enabled = FALSE) |>
    visEdges(smooth = FALSE, scaling = list(min = 0.8, max = 3.3)) |>
    visNodes(shape = "dot",
             scaling = list(min = size_min, max = size_max),
             font = list(size = label_size), fixed = list(x = FALSE, y = FALSE)) |>
    visInteraction(navigationButtons = TRUE, tooltipDelay = 100, dragNodes = TRUE)
}

# ---- Helper: top terms as one collapsible list per community ------------------
# The Table view opens compact: every cluster is a closed panel, and opening one
# shows its terms in rank order. Plain Bootstrap tables are used on purpose, as
# a DT inside a collapsed container measures its columns against zero width.
build_topterms_accordion <- function(tt, pal, metric = "relevance") {
  comms <- intersect(names(pal), unique(tt$community))
  if (length(comms) == 0) return(.empty_state("No community selected."))

  hdr <- function(label, active) {
    tags$th(HTML(label), class = if (active) "text-end fw-bold" else "text-end")
  }
  by_relevance <- !identical(metric, "frequency")

  panels <- lapply(comms, function(cid) {
    sub <- tt[tt$community == cid, , drop = FALSE]
    sub <- sub[order(sub$rank), , drop = FALSE]
    rows <- lapply(seq_len(nrow(sub)), function(i) {
      tags$tr(
        tags$td(sub$rank[i], class = "text-muted"),
        tags$td(tags$b(sub$term[i])),
        tags$td(sprintf("%.3f", sub$relevance[i]), class = "text-end"),
        tags$td(format(sub$frequency[i], big.mark = ","), class = "text-end"),
        tags$td(sub$degree[i], class = "text-end"))
    })
    accordion_panel(
      value = cid,
      title = tagList(
        tags$span(style = paste0(
          "display:inline-block; width:0.85rem; height:0.85rem; border-radius:50%;",
          " margin-right:0.55rem; vertical-align:middle; background:", pal[[cid]], ";")),
        tags$b(cid),
        tags$span(class = "text-muted small ms-2",
                  sprintf("%d terms", nrow(sub)))),
      div(class = "table-responsive",
        tags$table(
          class = "table table-sm table-hover align-middle mb-0",
          tags$thead(tags$tr(
            tags$th("Rank"), tags$th("Term"),
            hdr(.html_relevance, by_relevance),
            hdr(.html_frequency, !by_relevance),
            tags$th("Degree", class = "text-end"))),
          tags$tbody(rows))))
  })
  do.call(accordion, c(panels, list(open = FALSE, multiple = TRUE)))
}

# ---- Helper: empty-state placeholder -----------------------------------------
.empty_state <- function(msg = "Run the analysis to populate this tab.") {
  div(class = "text-muted text-center py-5",
      icon("circle-play", style = "font-size:2rem; color:#1D9E75;"),
      tags$p(msg, class = "mt-2"))
}

# ==============================================================================
# UI
# ==============================================================================
ui <- page_sidebar(
  window_title = "themescopeR",
  title = tagList(
    # Left: brand
    tags$span(
      style = "display:flex; align-items:center; gap:0.7rem;",
      tags$img(src = "themescope_logo.svg", height = "40px", alt = "themescopeR logo"),
      tags$span("themescopeR", style = "font-weight:600; letter-spacing:0.04em;"),
      tags$span("mapping social representations in digital discourse",
                class = "d-none d-lg-inline", style = "font-size:0.8rem; opacity:0.75;")),
    # Right: analysis-unit switch (only after preprocessing) + quick links
    div(
      class = "d-flex align-items-center gap-3",
      conditionalPanel(
        condition = "output.has_preprocessed",
        # Segmented toggle (a .shiny-input-radiogroup, so input$unit is lemma/token)
        tags$div(
          id = "unit", role = "group", `aria-label` = "Analysis unit",
          class = "shiny-input-radiogroup btn-group btn-group-sm ts-unit-toggle",
          tags$input(type = "radio", class = "btn-check", name = "unit",
                     id = "unit_lemma", value = "lemma", checked = "checked",
                     autocomplete = "off"),
          tags$label(class = "btn", `for` = "unit_lemma", "Lemma"),
          tags$input(type = "radio", class = "btn-check", name = "unit",
                     id = "unit_token", value = "token", autocomplete = "off"),
          tags$label(class = "btn", `for` = "unit_token", "Token"))),
      div(class = "ts-navlinks d-flex align-items-center gap-3 fs-5",
          actionLink("authors_info", icon("users"), title = "Authors",
                     class = "text-reset"),
          tags$a(href = .repo_url, target = "_blank", rel = "noopener",
                 title = "GitHub repository", class = "text-reset", icon("github")),
          tags$a(href = .paper_sage_url, target = "_blank", rel = "noopener",
                 title = "Read the paper (SAGE Journals)", class = "text-reset",
                 icon("file-lines")))
    )
  ),
  theme = bs_theme(
    version    = 5,
    bootswatch = "flatly",
    # ---- Logo-harmonized light palette (warm, on-brand, high contrast) ----
    primary    = "#1D9E75",  # logo green
    secondary  = "#D85A30",  # logo terracotta
    success    = "#1D9E75",
    warning    = "#EF9F27",  # logo amber (accents)
    info       = "#7F77DD",  # logo purple
    bg         = "#FBF8F4",  # warm off-white page
    fg         = "#3A3A38",  # warm dark grey text
    "body-color"    = "#4A4A48",
    "border-color"  = "#ECE6DE",
    "card-bg"       = "#FFFFFF",
    "link-color"    = "#0F6E56"
  ),
  fillable = FALSE,

  tags$head(
    tags$script(HTML(.oversize_js)),
    tags$style(HTML(.navbar_css))
  ),

  # ---- Sidebar: import first; the pipeline reveals once data is loaded ------
  sidebar = sidebar(
    width = 340,

    # ---- Import data (always visible) ----
    div(
      class = "d-flex align-items-center justify-content-between mb-1",
      tags$h6("Import data", class = "text-uppercase small fw-bold mb-0",
              style = "letter-spacing:0.06em; color:#3A3A38;"),
      actionLink("import_info", icon("circle-info"),
                 title = "How importing works", class = "text-secondary")
    ),

    # Before import: choose the data type, then browse + import (or load demo)
    conditionalPanel(
      condition = "!output.has_data",
      selectInput("import_type", "Data type",
                  choices = c("Raw text"                        = "raw",
                              "themescope file (.themescope)"   = "themescope",
                              "Demo corpus: climate change"     = "demo"),
                  selected = "raw"),

      # -- Raw text: size hint + browse + import --
      conditionalPanel(
        condition = "input.import_type == 'raw'",
        numericInput("expected_size_mb", "Expected file size (MB)",
                     value = .baseline_upload_mb, min = 1, step = 10),
        helpText(class = "small text-muted mb-2",
                 "Set this ", tags$em("before browsing"),
                 " if your file is larger than the default. See the ",
                 icon("circle-info"), " for details."),
        fileInput("file_upload", label = NULL,
                  accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls",
                             ".RData", ".rda", ".zip"),
                  placeholder = "Raw documents or annotated words",
                  buttonLabel = "Browse"),
        helpText(class = "small",
                 "Raw documents (", tags$code("doc_id"), ", ", tags$code("text"),
                 " in .csv/.tsv/.xlsx/.RData/.txt/.zip) ", tags$em("or"),
                 " an annotated words file (", tags$code("doc_id"), ", ",
                 tags$code("sentence_id"), ", ", tags$code("lemma"), "/",
                 tags$code("token"), ", ", tags$code("upos"), ")."),
        actionButton("do_import", tagList(icon("file-import"), " Import"),
                     class = "btn-primary w-100")
      ),

      # -- themescope file: a saved analysis, reopened as it was --
      conditionalPanel(
        condition = "input.import_type == 'themescope'",
        helpText(class = "small text-muted mb-2",
                 "A ", tags$code(".themescope"), " file written by ",
                 tags$code("save_themescope()"), " or by the Export button of ",
                 "this app. It restores the map, the communities and the top ",
                 "terms without re-annotating or re-running anything."),
        fileInput("archive_upload", label = NULL, accept = ".themescope",
                  placeholder = "Saved analysis", buttonLabel = "Browse"),
        actionButton("do_import_archive",
                     tagList(icon("box-open"), " Open archive"),
                     class = "btn-primary w-100")
      ),

      # -- Demo corpus: one click, no file, annotation included --
      conditionalPanel(
        condition = "input.import_type == 'demo'",
        helpText(class = "small text-muted mb-2",
                 "A bundled Reddit climate-change sample of 1,000 documents, ",
                 "shipped already tokenised and POS tagged: it goes straight ",
                 "to the analysis, with no waiting for udpipe."),
        actionButton("load_demo",
                     tagList(icon("seedling"), " Load demo corpus"),
                     class = "btn-outline-primary w-100")
      )
    ),

    # After import: badge + optional text-column picker + reset
    conditionalPanel(
      condition = "output.has_data",
      uiOutput("data_badge"),
      uiOutput("text_col_picker"),
      actionButton("reset_app", tagList(icon("rotate-left"), " Reset"),
                   class = "btn-outline-danger btn-sm w-100 mt-2")
    ),

    tags$hr(class = "my-2"),

    # ---- Pipeline: only after data is imported --------------------------------
    conditionalPanel(
      condition = "output.has_data",
      accordion(
        multiple = FALSE, open = "Preprocessing",

        # -- Step 1: preprocessing (tokenize + POS) --
        accordion_panel(
          "Preprocessing", icon = icon("language"),
          helpText(class = "small text-muted mb-2",
                   "After preprocessing, pick the analysis unit (Lemma or Token) ",
                   "with the switch in the top bar. It drives the network and the metrics."),
          conditionalPanel(
            condition = "output.words_ready",
            div(class = "small text-success py-1",
                icon("check"), " ", textOutput("words_ready_note", inline = TRUE))
          ),
          conditionalPanel(
            condition = "!output.words_ready",
            selectInput("language", "Annotation language (udpipe)",
                        choices = .languages, selected = "english"),
            selectInput("annot_treebank", "Model (treebank)", choices = NULL),
            uiOutput("model_desc"),
            actionButton("do_preprocess",
                         tagList(icon("wand-magic-sparkles"),
                                 " Preprocess (tokenize + POS)"),
                         class = "btn-primary w-100 mt-1"),
            uiOutput("preprocess_status")
          )
        ),

        # -- Step 2 params (locked until preprocessing is done) --
        accordion_panel(
          "Vocabulary & network", icon = icon("diagram-project"),
          conditionalPanel("!output.has_preprocessed", .locked_note()),
          conditionalPanel(
            "output.has_preprocessed",
            numericInput("vocab_size", "Vocabulary size (blank = all terms)",
                         value = 1500, min = 50, step = 50),
            checkboxGroupInput("pos_filter", "POS filter: content words",
                               choices = .pos_content,
                               selected = c("NOUN", "ADJ", "PROPN"), inline = TRUE),
            selectizeInput("pos_extra", "Additional UPOS tags (optional)",
                           choices = .pos_other, multiple = TRUE,
                           options = list(placeholder = "None")),
            selectInput("window", "Co-occurrence window",
                        choices = c("Sentence" = "sentence", "Document" = "document")),
            selectInput("normalization", "Similarity measure",
                        choices = c("Association strength" = "association",
                                    "Equivalence" = "equivalence",
                                    "Jaccard" = "jaccard",
                                    "Salton (cosine)" = "salton",
                                    "Inclusion" = "inclusion",
                                    "Raw frequency" = "frequency")),
            sliderInput("threshold_percentile", "Edge threshold percentile",
                        min = 0.80, max = 0.99, value = 0.98, step = 0.01)
          )
        ),
        accordion_panel(
          "Communities", icon = icon("layer-group"),
          conditionalPanel("!output.has_preprocessed", .locked_note()),
          conditionalPanel(
            "output.has_preprocessed",
            selectInput("community_algorithm", "Detection algorithm",
                        choices = c("Walktrap" = "walktrap",
                                    "Louvain" = "louvain",
                                    "Leiden" = "leiden")),
            conditionalPanel(
              "input.community_algorithm === 'walktrap'",
              numericInput("walktrap_steps", "Walktrap steps", value = 4, min = 2, max = 10)),
            conditionalPanel(
              "input.community_algorithm !== 'walktrap'",
              numericInput("resolution", "Resolution", value = 1, min = 0.1, max = 4, step = 0.1)),
            numericInput("min_community_size", "Min community size", value = 10, min = 2, max = 50),
            numericInput("seed", "Random seed", value = 1, min = 0, step = 1),
            radioButtons(
              "label_by", "Label clusters by",
              choiceNames = list(
                HTML("Relevance <i>R<sub>t</sub></i>"),
                HTML("Frequency <i>a<sub>t</sub></i>")
              ),
              choiceValues = c("relevance", "frequency"),
              selected = "relevance", inline = TRUE),
            helpText(
              HTML("Relevance <i>R<sub>t</sub></i> (case-study default) favours terms that are frequent <em>and</em> internally embedded; frequency uses term presence <i>a<sub>t</sub></i> alone."),
              class = "small text-muted")
          )
        ),
        accordion_panel(
          "Concreteness", icon = icon("cube"),
          conditionalPanel("!output.has_preprocessed", .locked_note()),
          conditionalPanel(
            "output.has_preprocessed",
            tags$p(tags$strong("Lexicon: "), uiOutput("lexicon_badge", inline = TRUE),
                   class = "mb-2"),
            fileInput("lexicon_upload", "Override (optional CSV: word, conc.m)",
                      accept = ".csv", placeholder = "Bundled Brysbaert",
                      buttonLabel = "Browse")
          )
        ),

        # -- Looks only: these never touch the pipeline, so Refine just redraws --
        accordion_panel(
          "Network appearance", icon = icon("palette"),
          conditionalPanel(
            "!output.has_result",
            .locked_note("Run the analysis to enable the network options.")),
          conditionalPanel(
            "output.has_result",
            helpText(class = "small text-muted mb-2",
                     "These settings change how the network is drawn. Click ",
                     tags$b("Refine network"), " to apply them: the analysis is ",
                     "not recomputed."),
            sliderInput("net_repulsion", "Repulsion between communities",
                        min = 0, max = 1, value = 0.3, step = 0.05),
            sliderInput("net_spacing", "Node spacing inside a community",
                        min = 0.4, max = 3, value = 1.2, step = 0.1),
            helpText(class = "small text-muted mt-n2 mb-2",
                     "Distance between neighbouring terms of the same ",
                     "community, counted in node diameters."),
            sliderInput("net_edge_opacity", "Edge opacity",
                        min = 0.05, max = 1, value = 0.65, step = 0.05),
            sliderInput("net_label_size", "Label size (px)",
                        min = 6, max = 32, value = 16, step = 1),
            sliderInput("net_node_scale", "Node size",
                        min = 0.4, max = 2.5, value = 1, step = 0.1),
            numericInput("net_seed", "Layout seed", value = 1, min = 0, step = 1),
            input_switch("net_show_labels", "Show term labels", value = TRUE),
            input_switch("net_hide_unclassified", "Only classified terms",
                         value = TRUE),
            helpText(class = "small text-muted mt-n2 mb-2",
                     "Terms that ended up in no community are usually about half ",
                     "the network. Switch this off to bring them back as faded ",
                     "grey dots."),
            actionButton("refine_network",
                         tagList(icon("wand-magic-sparkles"), " Refine network"),
                         class = "btn-outline-primary w-100 mt-2")
          )
        )
      ),

      # -- Step 2 trigger: only after preprocessing --
      conditionalPanel(
        condition = "output.has_preprocessed",
        actionButton("run_analysis", tagList(icon("play"), " Run analysis"),
                     class = "btn-primary w-100 mt-1"),
        uiOutput("analysis_status_badge")
      ),

      # -- Save the finished study (data + parameters + scores) --
      conditionalPanel(
        condition = "output.has_result",
        tags$hr(class = "my-2"),
        downloadButton("dl_themescope",
                       tagList(icon("floppy-disk"), " Export .themescope"),
                       class = "btn-outline-secondary w-100"),
        helpText(class = "small text-muted mt-1 mb-0",
                 "Saves the annotated corpus, the parameters and the term ",
                 "scores in one file you can reopen here or in the console.")
      ),
      conditionalPanel(
        condition = "!output.has_preprocessed",
        div(class = "text-muted small text-center py-2 mt-1",
            icon("lock"), " Preprocess first to run the analysis.")
      )
    )  # end conditionalPanel (pipeline)
  ),

  # ---- Headline value boxes (appear after the first run) --------------------
  uiOutput("value_boxes"),

  # ---- Main tabs -------------------------------------------------------------
  navset_card_tab(
    id = "main_tabs", selected = "info",

    nav_panel(
      value = "info", title = tagList(icon("circle-info"), " Info"),
      div(class = "container-fluid py-3 px-3", style = "max-width: 960px;",

        # -- Masthead --
        div(class = "d-flex align-items-center gap-3 mb-3",
            tags$img(src = "themescope_logo.svg", height = "96px",
                     alt = "themescopeR logo"),
            div(
              tags$h3("themescopeR", class = "mb-1", style = "color:#3A3A38;"),
              tags$p(class = "text-muted mb-0", style = "max-width:620px;",
                     "From raw digital discourse to a map of social ",
                     "representations. ThemeScope builds a semantic ",
                     "co-occurrence network and places each theme on a ",
                     "strategic diagram along two axes: ",
                     tags$b("PSI"), " (anchoring) and ", tags$b("CS"),
                     " (objectification)."))),

        # -- Four SRT regions (the visual signature) --
        tags$h6("The four SRT regions", class = "text-uppercase small fw-bold mt-2 mb-2",
                style = "letter-spacing:0.06em; color:#3A3A38;"),
        layout_columns(
          col_widths = c(6, 6),
          card(class = "border-0", style = paste0("background:", .quad_colors[["Stable Core"]], "1a;"),
               card_body(tags$strong("Stable Core", style = paste0("color:", .quad_colors[["Stable Core"]])),
                         tags$small("High PSI, high CS: consolidated, concrete themes at the heart of the discourse."))),
          card(class = "border-0", style = paste0("background:", .quad_colors[["Ideological Core"]], "1a;"),
               card_body(tags$strong("Ideological Core", style = paste0("color:", .quad_colors[["Ideological Core"]])),
                         tags$small("High PSI, low CS: central but abstract, value-laden themes."))),
          card(class = "border-0", style = paste0("background:", .quad_colors[["Emerging Practices"]], "1a;"),
               card_body(tags$strong("Emerging Practices", style = paste0("color:", .quad_colors[["Emerging Practices"]])),
                         tags$small("Low PSI, high CS: concrete themes not yet anchored in the core."))),
          card(class = "border-0", style = paste0("background:", .quad_colors[["Latent Representations"]], "1a;"),
               card_body(tags$strong("Latent Representations", style = paste0("color:", .quad_colors[["Latent Representations"]])),
                         tags$small("Low PSI, low CS: peripheral, abstract themes.")))),

        # -- Seminal paper + mandatory citation --
        div(class = "card border-0 mt-4", style = "background:#1D9E7514;",
          div(class = "card-body",
            tags$h5(tagList(icon("book"), " The seminal paper"), class = "mb-1",
                    style = "color:#3A3A38;"),
            tags$p(class = "mb-2",
                   "Misuraca, Spano and D'Aniello (2026), ",
                   tags$em("ThemeScope: A quantitative thematic analysis for ",
                           "depicting social representations in digital arenas"),
                   ", ", tags$em("Journal of Information Science"), "."),
            tags$p(class = "mb-2",
                   tags$b("Using themescopeR requires citing it."),
                   " Publishing results without attribution violates the terms of use."),
            radioButtons("cite_format", NULL,
                         choices = c("APA" = "apa", "BibTeX" = "bibtex"),
                         selected = "apa", inline = TRUE),
            uiOutput("citation_box_ui"),
            div(class = "d-flex flex-wrap gap-2 mt-2",
              tags$button(
                id = "copy_citation", type = "button", class = "btn btn-sm btn-primary",
                onclick = paste0(
                  "navigator.clipboard.writeText(",
                  "document.getElementById('citation_text').innerText).then(function(){",
                  "var b=document.getElementById('copy_citation');",
                  "var t=b.innerHTML;b.innerHTML='Copied!';",
                  "setTimeout(function(){b.innerHTML=t;},1500);});"),
                tagList(icon("copy"), " Copy citation")),
              tags$a(href = .cite_doi_url, target = "_blank",
                     class = "btn btn-sm btn-outline-secondary",
                     tagList(icon("up-right-from-square"), " Read the paper")),
              tags$a(href = .readme_url, target = "_blank",
                     class = "btn btn-sm btn-outline-secondary",
                     tagList(icon("book-open"), " More info (README)"))
            )
          )
        ),

        tags$p(class = "small text-muted mt-3 mb-0",
               "Everything you see here is produced by the exported functions ",
               tags$code("read_collection()"), ", ", tags$code("preprocess_texts()"),
               ", ", tags$code("themescope()"), " and ", tags$code("top_terms()"),
               ". See the ", tags$b("Reproduce"), " tab for the console equivalent. ",
               "Concreteness norms: Brysbaert et al. (2014); updated udpipe models: ",
               "tall.language.models (M. Aria).")
      )
    ),

    nav_panel(
      value = "data", title = tagList(icon("table"), " Data"),
      navset_card_tab(
        id = "data_subtabs",
        nav_panel(
          value = "imported",
          title = tagList(icon("file-lines"), " Imported data"),
          div(class = "ts-toolbar mb-2",
              tags$span(class = "small text-muted me-auto",
                        "Raw documents as imported (long text truncated for display)."),
              selectInput("exp_imported_fmt", NULL,
                          choices = c("CSV" = "csv", "Excel" = "xlsx"),
                          width = "110px"),
              downloadButton("dl_imported", "Export",
                             class = "btn-outline-secondary")),
          uiOutput("imported_preview_ui")
        ),
        nav_panel(
          value = "preprocessed",
          title = tagList(icon("diagram-project"), " Pre-processed data"),
          div(class = "ts-toolbar mb-2",
              tags$span(class = "small text-muted me-auto",
                        "udpipe tokens: doc, sentence, token, lemma, POS."),
              selectInput("exp_pre_fmt", NULL,
                          choices = c("CSV" = "csv", "Excel" = "xlsx"),
                          width = "110px"),
              downloadButton("dl_pre", "Export",
                             class = "btn-outline-secondary")),
          uiOutput("preprocessed_preview_ui")
        )
      )
    ),

    nav_panel(
      value = "themescope", title = tagList(icon("map"), " ThemeScope"),
      navset_card_tab(
        id = "ts_subtabs", full_screen = TRUE,
        nav_panel(
          value = "network", title = tagList(icon("circle-nodes"), " Network"),
          conditionalPanel(
            condition = "output.has_result",
            div(class = "ts-toolbar justify-content-end mb-2",
                tags$span(class = "small text-muted",
                          "Appearance is set in the sidebar, under ",
                          tags$b("Network appearance"), "."))),
          uiOutput("network_ui")
        ),
        nav_panel(
          value = "map", title = tagList(icon("map"), " Map"),
          conditionalPanel(
            condition = "output.has_result",
            div(class = "ts-toolbar justify-content-end mb-2",
                input_switch("map_label_terms", "Label with top terms",
                             value = FALSE))),
          uiOutput("map_ui")
        )
      )
    ),

    nav_panel(
      value = "communities", title = tagList(icon("layer-group"), " Communities"),
      card(card_header(class = "ts-toolbar justify-content-between",
                       span("Community metrics and structure"),
                       downloadButton("dl_communities", "CSV",
                                      class = "btn-outline-secondary")),
           card_body(
             tags$p(class = "small text-muted mb-2",
                    "Use the boxes under the headers to filter any column."),
             uiOutput("communities_ui")))
    ),

    nav_panel(
      value = "terms", title = tagList(icon("list-ol"), " Top Terms"),
      card(full_screen = TRUE,
        card_header(
          div(class = "ts-toolbar justify-content-between",
              span("Top terms by community, ranked by ",
                   uiOutput("tt_metric_label", inline = TRUE)),
              div(class = "ts-toolbar",
                  selectizeInput("tt_comms", label = NULL, choices = NULL, multiple = TRUE,
                                 width = "320px",
                                 options = list(placeholder = "All communities")),
                  numericInput("top_n_terms", label = NULL, value = 10, min = 3, max = 30,
                               width = "90px"),
                  downloadButton("dl_terms", "CSV",
                                 class = "btn-outline-secondary")))),
        card_body(
          navset_tab(
            nav_panel(tagList(icon("chart-column"), " Plot"),
                      uiOutput("topterms_ui")),
            nav_panel(tagList(icon("table"), " Table"),
                      uiOutput("topterms_table_ui"))
          )
        ))
    ),

    nav_panel(
      value = "repro", title = tagList(icon("code"), " Reproduce"),
      card(card_header("Reproduce this analysis in the R console"),
           card_body(uiOutput("repro_ui")))
    )
  )
)

# ==============================================================================
# Server
# ==============================================================================
server <- function(input, output, session) {

  collection  <- reactiveVal(NULL)   # raw documents (doc_id, text, ...)
  words_data  <- reactiveVal(NULL)   # annotated words (udpipe output)
  annot_key   <- reactiveVal(NULL)   # signature of the annotation in words_data
  result_obj  <- reactiveVal(NULL)
  data_source <- reactiveVal(NULL)
  import_path <- reactiveVal(NULL)   # staged file path (for text-column re-reads)
  raw_names   <- reactiveVal(NULL)   # original column names of a tabular import
  archive_obj <- reactiveVal(NULL)   # the .themescope archive currently open

  # Analysis unit from the navbar switch (off = lemma, on = token).
  analysis_unit <- reactive({
    u <- input$unit
    if (is.null(u) || !nzchar(u)) "lemma" else u
  })

  # ---- Reveal flags: drive the sidebar / gating --------------------------------
  output$has_data <- reactive({
    !is.null(collection()) || !is.null(words_data())
  })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)

  # Preprocessing is "done" once we have an annotated words table: from a
  # Preprocess run, from an uploaded annotated file, from the demo, or from an
  # archive.
  output$has_preprocessed <- reactive({ !is.null(words_data()) })
  outputOptions(output, "has_preprocessed", suspendWhenHidden = FALSE)

  # The words table arrived ready-made, so the udpipe step can be skipped.
  output$words_ready <- reactive({
    isTRUE(annot_key() %in% c("uploaded-words", "demo-annotated", "archive"))
  })
  outputOptions(output, "words_ready", suspendWhenHidden = FALSE)

  output$words_ready_note <- renderText({
    switch(if (is.null(annot_key())) "" else annot_key(),
      "demo-annotated" = "Demo corpus already annotated. Run the analysis when ready.",
      "archive"        = "Annotated corpus restored from the archive.",
      "Annotated words already provided. You can run the analysis.")
  })

  # A finished analysis exists: unlocks the appearance controls and the export.
  output$has_result <- reactive({ !is.null(result_obj()) })
  outputOptions(output, "has_result", suspendWhenHidden = FALSE)

  # ---- Updated-model registry (language -> treebanks) ----
  # The first entry a language offers is the best general model for it, so it is
  # preselected: no "default" placeholder, the actual treebank is always named.
  model_registry <- reactive({ .safe_model_registry() })

  observeEvent(input$language, {
    reg <- model_registry()
    if (is.null(reg) || !all(c("language_name", "treebank") %in% names(reg))) {
      updateSelectInput(session, "annot_treebank",
                        choices = c("Best available model" = ""), selected = "")
      return()
    }
    rows <- reg[tolower(reg$language_name) == tolower(input$language), , drop = FALSE]
    if (nrow(rows) == 0) {
      updateSelectInput(session, "annot_treebank",
                        choices = c("Best available model" = ""), selected = "")
      return()
    }
    ch <- stats::setNames(rows$treebank, rows$treebank)
    updateSelectInput(session, "annot_treebank", choices = ch, selected = ch[[1]])
  }, ignoreInit = FALSE)

  output$model_desc <- renderUI({
    reg <- model_registry()
    tb  <- input$annot_treebank
    if (is.null(tb) || !nzchar(tb)) {
      return(helpText(class = "small text-muted",
                      "The model catalogue is not reachable right now, so the ",
                      "best model for the language will be used."))
    }
    d <- ""
    if (!is.null(reg) && "description" %in% names(reg)) {
      row <- reg[tolower(reg$language_name) == tolower(input$language) &
                   reg$treebank == tb, , drop = FALSE]
      if (nrow(row) > 0) d <- as.character(row$description[1])
    }
    helpText(class = "small text-muted",
             if (nzchar(d) && !is.na(d)) d else paste0("Treebank: ", tb),
             tags$br(), "Downloaded once, then cached.")
  })

  # ---- Expected file size sets Shiny's upload cap ----
  # Shiny reads this option at upload time, so it must be set before the user
  # browses (hence the numeric input sits above the Browse button).
  observeEvent(input$expected_size_mb, {
    mb <- suppressWarnings(as.numeric(input$expected_size_mb))
    if (is.na(mb) || mb < 1) mb <- .baseline_upload_mb
    options(shiny.maxRequestSize = mb * 1024^2)
  }, ignoreInit = FALSE)

  # ---- Oversized file: the JS guard caught a file bigger than the limit ----
  # Offer to raise the limit (editable, pre-filled recommendation); on OK we
  # bump shiny.maxRequestSize and replay the upload so it imports automatically.
  observeEvent(input$oversize_detected, {
    info    <- input$oversize_detected
    size_mb <- suppressWarnings(as.numeric(info$size_mb))
    lim_mb  <- suppressWarnings(as.numeric(info$limit_mb))
    rec     <- suppressWarnings(as.numeric(info$recommend))
    if (is.na(rec)) rec <- ceiling(size_mb) + 10
    showModal(modalDialog(
      title = tagList(icon("triangle-exclamation"), " File larger than the upload limit"),
      tags$p(HTML(sprintf(
        "The selected file is about <b>%s MB</b>, but the current import limit is <b>%s MB</b>, so it can't be uploaded.",
        format(size_mb), format(lim_mb)))),
      tags$p("Raise the limit to at least the file size, then confirm. The file ",
             "will be imported automatically."),
      numericInput("oversize_new_mb", "New maximum upload size (MB)",
                   value = rec, min = max(1, ceiling(size_mb)), step = 10),
      helpText(class = "small text-muted",
               sprintf(paste0("Recommended: %s MB (file size + headroom). Bigger ",
                              "files need more free RAM, so keep roughly 2 to 5 times the ",
                              "file size available."), rec)),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("oversize_confirm", tagList(icon("check"), " OK, raise & import"),
                     class = "btn-primary")
      )
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$oversize_confirm, {
    mb      <- suppressWarnings(as.numeric(input$oversize_new_mb))
    size_mb <- suppressWarnings(as.numeric(input$oversize_detected$size_mb))
    if (is.na(mb) || mb < 1) mb <- .baseline_upload_mb
    if (!is.na(size_mb) && mb < size_mb) {
      showNotification(sprintf("The limit must be at least the file size (%s MB).",
                               ceiling(size_mb)), type = "warning")
      return()
    }
    options(shiny.maxRequestSize = mb * 1024^2)
    updateNumericInput(session, "expected_size_mb", value = mb)
    removeModal()
    session$sendCustomMessage("resume_upload", list(ts = as.numeric(Sys.time())))
  }, ignoreInit = TRUE)

  observeEvent(input$resume_failed, {
    showNotification(paste("Upload limit raised. Please click Browse and pick",
                           "the file again."), type = "message", duration = 6)
  }, ignoreInit = TRUE)

  # ---- Info modal: how the import process works ----
  observeEvent(input$import_info, {
    showModal(modalDialog(
      title = tagList(icon("circle-info"), " How importing works"),
      easyClose = TRUE,
      tags$ol(
        tags$li(tags$b("Pick the data type"), ": ", tags$em("Raw text"),
                " (documents to analyse), a saved ", tags$code(".themescope"),
                " object (coming soon), or the bundled ", tags$em("Demo"), "."),
        tags$li(tags$b("Set the expected file size"), " (MB) if your raw file is ",
                "larger than ", .baseline_upload_mb, " MB. This raises the upload ",
                "limit before the file is transferred."),
        tags$li(tags$b("Browse"), " and pick one file: raw documents ",
                "(.csv/.tsv/.xlsx/.RData/.txt, or a .zip of many) with ",
                tags$code("doc_id"), " + ", tags$code("text"), ", or an already ",
                "annotated words file (", tags$code("doc_id"), ", ",
                tags$code("sentence_id"), ", ", tags$code("lemma"), "/",
                tags$code("token"), ", ", tags$code("upos"), ")."),
        tags$li(tags$b("Click Import"), ". For tabular files you can then pick ",
                "which column holds the ", tags$b("text"), " to analyse."),
        tags$li(tags$b("Preprocess"), " (tokenize + POS) to unlock the rest of ",
                "the pipeline, then ", tags$b("Run analysis"), ".")
      ),
      tags$p(class = "mb-1", tags$b("What to expect for large files:")),
      tags$ul(
        tags$li("The whole file is loaded into RAM; keep roughly ",
                tags$b("2 to 5 times the file size"), " free, or R may slow down or crash."),
        tags$li("Very large uploads take time to transfer through the browser."),
        tags$li("udpipe annotation of big collections can take minutes to hours.")
      ),
      tags$p(class = "small text-muted mb-0",
             "Use ", tags$b("Reset"), " (top of the sidebar, after import) to ",
             "clear everything and start over."),
      footer = modalButton("Got it")
    ))
  })

  # ---- Authors modal (navbar) ----
  observeEvent(input$authors_info, {
    showModal(modalDialog(
      title = tagList(icon("users"), " Authors"),
      easyClose = TRUE,
      tags$p(class = "text-muted small",
             "themescopeR and the ThemeScope method are developed by:"),
      tags$ul(
        class = "list-unstyled mb-0",
        lapply(.authors, function(a) {
          tags$li(
            class = "d-flex align-items-center gap-3 mb-3",
            tags$img(src = a$photo, class = "ts-avatar", alt = a$name),
            div(
              tags$a(href = a$url, target = "_blank", rel = "noopener",
                     class = "text-decoration-none",
                     tagList(tags$b(a$name), " ",
                             icon("up-right-from-square", class = "small ms-1"))),
              tags$div(class = "small text-muted", a$role)))
        })
      ),
      footer = modalButton("Close")
    ))
  })

  # ---- Demo: raw texts plus their annotation, so udpipe can be skipped ----
  observeEvent(input$load_demo, {
    if (is.null(.demo_coll)) {
      showNotification("Demo data not found in the package.", type = "error"); return()
    }
    words <- tryCatch(.demo_words(), error = function(e) NULL)
    collection(.demo_coll); result_obj(NULL); data_source("demo")
    import_path(NULL); raw_names(NULL); archive_obj(NULL)
    if (is.null(words)) {
      words_data(NULL); annot_key(NULL)
      showNotification(sprintf(
        "Demo loaded: %s raw documents. The bundled annotation is missing, so preprocess them first.",
        format(.demo_ndocs, big.mark = ",")), type = "warning", duration = 8)
      nav_select("main_tabs", "data")
      nav_select("data_subtabs", "imported")
      return()
    }
    words_data(words); annot_key("demo-annotated")
    showNotification(sprintf("Demo loaded: %s documents, %s annotated tokens. Ready to run.",
                             format(.demo_ndocs, big.mark = ","),
                             format(nrow(words), big.mark = ",")), type = "message")
    nav_select("main_tabs", "data")
    nav_select("data_subtabs", "preprocessed")
  })

  # ---- Import (raw documents OR annotated words), triggered by the button ----
  observeEvent(input$do_import, {
    if (is.null(input$file_upload)) {
      showNotification("Choose a file with Browse first, then click Import.",
                       type = "warning"); return()
    }
    tryCatch({
      path <- input$file_upload$datapath
      coll <- tryCatch(read_collection(path, sequential_ids = TRUE),
                       error = function(e) NULL)
      if (!is.null(coll) && "text" %in% names(coll)) {
        # Raw documents: stage the path + original column names so the user can
        # switch the text column afterwards (tabular files only).
        import_path(path)
        raw_names(.peek_colnames(path))
        collection(coll); words_data(NULL); annot_key(NULL)
        showNotification(sprintf("Loaded %d raw documents.", nrow(coll)), type = "message")
      } else {
        sep <- if (tolower(tools::file_ext(input$file_upload$name)) == "tsv") "\t" else ","
        df <- utils::read.csv(path, sep = sep, stringsAsFactors = FALSE, check.names = FALSE)
        if (!"lemma" %in% names(df) && "token" %in% names(df)) df$lemma <- df$token
        validate_words_df(df)
        for (c0 in intersect(c("doc_id", "sentence_id", "token", "lemma", "upos"), names(df)))
          df[[c0]] <- as.character(df[[c0]])
        import_path(NULL); raw_names(NULL)
        collection(NULL); words_data(df); annot_key("uploaded-words")
        showNotification(sprintf("Loaded annotated words: %d rows.", nrow(df)), type = "message")
      }
      result_obj(NULL); data_source("upload"); archive_obj(NULL)
      nav_select("main_tabs", "data")
    }, error = function(e) {
      showNotification(paste("Error reading file:", conditionMessage(e)),
                       type = "error", duration = 8)
    })
  })

  # ---- Import a saved .themescope archive ----
  # Everything comes back at once: raw texts, annotation, parameters and the
  # term scores behind the maps. A summary window says what the file holds.
  observeEvent(input$do_import_archive, {
    if (is.null(input$archive_upload)) {
      showNotification("Choose a .themescope file with Browse first.",
                       type = "warning"); return()
    }
    tryCatch({
      arc <- read_themescope(input$archive_upload$datapath)
      res <- arc$result

      collection(arc$collection)
      words_data(arc$words)
      annot_key(if (is.null(arc$words)) NULL else "archive")
      result_obj(res)
      data_source("archive")
      import_path(NULL); raw_names(NULL)
      archive_obj(arc)

      # Put the sidebar back in the state that produced these results, so a new
      # run starts from the archived parameters instead of the app defaults.
      p <- res$params
      updateSelectInput(session, "window", selected = p$window)
      updateSelectInput(session, "normalization", selected = p$normalization)
      updateSelectInput(session, "community_algorithm", selected = p$community_algorithm)
      updateSliderInput(session, "threshold_percentile", value = p$threshold_percentile)
      updateNumericInput(session, "vocab_size", value = p$vocab_size)
      updateNumericInput(session, "walktrap_steps", value = p$walktrap_steps)
      updateNumericInput(session, "resolution", value = p$resolution)
      updateNumericInput(session, "min_community_size", value = p$min_community_size)
      if (!is.null(p$seed)) updateNumericInput(session, "seed", value = p$seed)
      updateCheckboxGroupInput(session, "pos_filter",
                               selected = intersect(p$pos_filter, .pos_content))
      updateSelectizeInput(session, "pos_extra",
                           selected = intersect(p$pos_filter, .pos_other))
      if (!is.null(arc$meta$language)) {
        updateSelectInput(session, "language", selected = arc$meta$language)
      }

      showModal(modalDialog(
        title = tagList(icon("box-open"), " ", basename(input$archive_upload$name)),
        size = "l", easyClose = TRUE,
        tags$p(class = "text-muted",
               "This archive was reopened as it was saved. The map, the ",
               "communities and the top terms are already available; nothing ",
               "was recomputed."),
        uiOutput("archive_summary_ui"),
        tags$p(class = "small text-muted mt-3 mb-0",
               "Change any parameter in the sidebar and click ",
               tags$b("Run analysis"), " to analyse the same corpus differently."),
        footer = modalButton("Continue")
      ))
      nav_select("main_tabs", "themescope")
      nav_select("ts_subtabs", "map")
    }, error = function(e) {
      showNotification(paste("Could not open the archive:", conditionMessage(e)),
                       type = "error", duration = 10)
    })
  })

  # The description shown in the import window (and reusable elsewhere): built
  # by the package, so the GUI stays a thin front-end.
  output$archive_summary_ui <- renderUI({
    arc <- archive_obj(); req(arc)
    info <- summary(arc)
    div(class = "table-responsive",
      tags$table(
        class = "table table-sm align-middle mb-0",
        tags$tbody(lapply(seq_len(nrow(info)), function(i) {
          tags$tr(tags$td(class = "text-muted", style = "width:45%;", info$field[i]),
                  tags$td(tags$b(info$value[i])))
        }))))
  })

  # ---- Text-column picker (tabular raw imports only, before preprocessing) ----
  output$text_col_picker <- renderUI({
    nms <- raw_names()
    # Only meaningful for a raw tabular import that has not been annotated yet.
    if (is.null(nms) || length(nms) < 2 || is.null(collection()) ||
        !is.null(words_data())) {
      return(NULL)
    }
    selectInput("text_column", "Text column to analyse",
                choices = nms, selected = .detect_text_col(nms), width = "100%")
  })

  # Re-read the staged file when the user changes the text column.
  observeEvent(input$text_column, {
    path <- import_path()
    if (is.null(path) || is.null(input$text_column)) return()
    tryCatch({
      coll <- read_collection(path, text_col = input$text_column)
      collection(coll); words_data(NULL); annot_key(NULL); result_obj(NULL)
      showNotification(sprintf("Text column set to '%s'.", input$text_column),
                       type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Could not use that column:", conditionMessage(e)),
                       type = "error", duration = 6)
    })
  }, ignoreInit = TRUE)

  # ---- Reset the whole app (with confirmation) ----
  observeEvent(input$reset_app, {
    showModal(modalDialog(
      title = tagList(icon("triangle-exclamation"), " Reset the app?"),
      "This clears the imported data and all results, and starts over from scratch.",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset", "Reset", class = "btn-danger")
      )
    ))
  })
  observeEvent(input$confirm_reset, {
    removeModal()
    session$reload()
  })

  # ---- Citation (APA default, switchable to BibTeX; copied by the button) ----
  output$citation_box_ui <- renderUI({
    txt <- if (identical(input$cite_format, "bibtex")) .cite_bibtex else .cite_apa
    tags$pre(id = "citation_text", class = "p-3 rounded mb-0",
             style = paste0("background:#FFFFFF; border:1px solid #ECE6DE; ",
                            "white-space:pre-wrap; font-size:0.85rem;"),
             txt)
  })

  output$data_badge <- renderUI({
    if (!is.null(collection())) {
      tags$span(class = "badge bg-success mt-2", icon("check"),
                sprintf(" %s raw documents loaded", format(nrow(collection()), big.mark = ",")))
    } else if (!is.null(words_data())) {
      tags$span(class = "badge bg-success mt-2", icon("check"),
                sprintf(" %s annotated rows loaded", format(nrow(words_data()), big.mark = ",")))
    }
  })

  # ---- Concreteness lexicon (bundled default, optional override) ----
  lexicon_data <- reactive({
    if (is.null(input$lexicon_upload)) return(themescopeR::brysbaert)
    tryCatch({
      df <- utils::read.csv(input$lexicon_upload$datapath, stringsAsFactors = FALSE)
      if (!all(c("word", "conc.m") %in% names(df))) {
        showNotification("Lexicon needs columns 'word' and 'conc.m'. Using bundled Brysbaert.",
                         type = "warning")
        return(themescopeR::brysbaert)
      }
      df
    }, error = function(e) themescopeR::brysbaert)
  })
  output$lexicon_badge <- renderUI({
    if (is.null(input$lexicon_upload)) {
      tags$span(class = "badge bg-success", "Brysbaert (bundled)")
    } else {
      tags$span(class = "badge bg-info", input$lexicon_upload$name)
    }
  })

  # ---- Step 1: preprocess (udpipe tokenize + POS) ----
  observeEvent(input$do_preprocess, {
    if (is.null(collection())) {
      showNotification("Import raw documents first.", type = "warning"); return()
    }
    tb <- input$annot_treebank
    key <- paste0(data_source(), "-", nrow(collection()), "-", input$language,
                  "-", if (is.null(tb)) "" else tb)
    withProgress(message = "Preprocessing with udpipe ...", value = 0, {
      tryCatch({
        setProgress(0.1, detail = paste0("Resolving model (", input$language, ") ..."))
        mdl <- if (!is.null(tb) && nzchar(tb)) {
          pth <- tryCatch(ts_download_model(input$language, treebank = tb),
                          error = function(e) NULL)
          if (is.null(pth)) input$language else pth
        } else input$language
        setProgress(0.3, detail = "Annotating (tokenize + POS) ...")
        words <- preprocess_texts(collection(), model = mdl, verbose = FALSE)
        words_data(words); annot_key(key); result_obj(NULL)
        setProgress(1, detail = "Done.")
        showNotification(sprintf("Preprocessing complete: %s tokens. Now tune and Run.",
                                 format(nrow(words), big.mark = ",")), type = "message")
        nav_select("main_tabs", "data")
        nav_select("data_subtabs", "preprocessed")
      }, error = function(e) {
        showNotification(paste("Preprocessing failed:", conditionMessage(e)),
                         type = "error", duration = 10)
      })
    })
  })

  output$preprocess_status <- renderUI({
    if (is.null(words_data())) {
      tags$span(class = "badge bg-secondary mt-2 d-block text-center",
                "Not preprocessed yet")
    } else {
      tags$span(class = "badge bg-success mt-2 d-block text-center", icon("check"),
                sprintf(" %s tokens", format(nrow(words_data()), big.mark = ",")))
    }
  })

  # ---- Step 2: run analysis (network + communities + metrics) ----
  observeEvent(input$run_analysis, {
    if (is.null(words_data())) {
      showNotification("Preprocess the data first.", type = "warning"); return()
    }
    pos_filter <- union(input$pos_filter, input$pos_extra)
    if (length(pos_filter) == 0) {
      showNotification("Select at least one POS tag.", type = "warning"); return()
    }

    withProgress(message = "Running themescopeR ...", value = 0, {
      tryCatch({
        vocab_size <- if (is.null(input$vocab_size) || is.na(input$vocab_size)) {
          NULL
        } else {
          as.integer(input$vocab_size)
        }

        setProgress(0.4, detail = "Building network & metrics ...")
        res <- themescope(
          words_data(),
          unit                 = analysis_unit(),
          concreteness_lexicon = lexicon_data(),
          vocab_size           = vocab_size,
          pos_filter           = pos_filter,
          window               = input$window,
          normalization        = input$normalization,
          threshold_percentile = input$threshold_percentile,
          community_algorithm  = input$community_algorithm,
          walktrap_steps       = input$walktrap_steps,
          resolution           = input$resolution,
          min_community_size   = input$min_community_size,
          seed                 = input$seed,
          verbose              = FALSE
        )
        result_obj(res)
        setProgress(1, detail = "Done.")
        showNotification(sprintf("Analysis complete: %d communities (%d nodes, %d edges).",
                                 length(res$communities), igraph::vcount(res$graph),
                                 igraph::ecount(res$graph)), type = "message")
        nav_select("main_tabs", "themescope")
        nav_select("ts_subtabs", "map")
      }, error = function(e) {
        showNotification(paste("Analysis failed:", conditionMessage(e)),
                         type = "error", duration = 10)
      })
    })
  })

  output$analysis_status_badge <- renderUI({
    res <- result_obj()
    if (is.null(res)) {
      tags$span(class = "badge bg-secondary mt-2 d-block text-center", "No analysis yet")
    } else {
      tags$span(class = "badge bg-success mt-2 d-block text-center", icon("check"),
                sprintf(" %d communities", length(res$communities)))
    }
  })

  # ---- Export the finished study as a .themescope archive ----
  # Whatever is not already inside the themescope object: where the corpus came
  # from, which model annotated it, which lexicon scored it.
  archive_meta <- reactive({
    res <- result_obj()
    arc <- archive_obj()
    from_archive <- identical(annot_key(), "archive") && !is.null(arc)
    demo         <- identical(annot_key(), "demo-annotated")

    # The bundled demo annotation was produced with the English GUM treebank.
    lang <- if (demo) "english" else if (from_archive) arc$meta$language else input$language
    tb   <- if (demo) "GUM" else if (from_archive) arc$meta$treebank else input$annot_treebank
    if (!is.null(tb) && (length(tb) != 1 || !nzchar(tb))) tb <- NULL

    cov <- if (is.null(res)) NULL else tryCatch(
      mean(!is.na(match_concreteness(igraph::V(res$graph)$name, lexicon_data()))),
      error = function(e) NULL)

    list(
      language = lang,
      treebank = tb,
      lexicon  = if (is.null(input$lexicon_upload)) "Brysbaert (bundled)"
                 else input$lexicon_upload$name,
      lexicon_coverage = cov,
      source   = switch(if (is.null(data_source())) "" else data_source(),
                        demo    = "Bundled demo corpus (climate change)",
                        archive = "Reopened .themescope archive",
                        upload  = "File imported in the app",
                        "Unknown"),
      label_by = label_by()
    )
  })

  output$dl_themescope <- downloadHandler(
    filename = function() {
      paste0("themescope_", format(Sys.Date(), "%Y%m%d"), ".themescope")
    },
    content = function(file) {
      res <- result_obj(); req(res)
      # Write to a temp path first: save_themescope() enforces the extension,
      # while Shiny hands us a path it will serve under the name above.
      tmp <- tempfile(fileext = ".themescope")
      on.exit(unlink(tmp), add = TRUE)
      save_themescope(res, tmp, words = words_data(), collection = collection(),
                      meta = archive_meta())
      file.copy(tmp, file, overwrite = TRUE)
    }
  )

  # ---- Network appearance: applied on demand, never re-running the pipeline ----
  net_opts <- reactiveVal(.network_defaults)
  # NULL entries are dropped downstream, so a control that has not reported yet
  # falls back to its default rather than to FALSE.
  .collect_net_opts <- function() {
    list(repulsion         = input$net_repulsion,
         spacing           = input$net_spacing,
         hide_unclassified = input$net_hide_unclassified,
         label_size        = input$net_label_size,
         node_scale        = input$net_node_scale,
         edge_opacity      = input$net_edge_opacity,
         show_labels       = input$net_show_labels,
         seed              = input$net_seed)
  }
  # A fresh analysis draws with whatever the controls currently say ...
  observeEvent(result_obj(), { net_opts(.collect_net_opts()) })
  # ... and afterwards only Refine changes the drawing.
  observeEvent(input$refine_network, {
    net_opts(.collect_net_opts())
    showNotification("Network redrawn.", type = "message", duration = 2)
    nav_select("main_tabs", "themescope")
    nav_select("ts_subtabs", "network")
  })

  # ---- Derived data ----
  # Cluster-labelling method (relevance R_t by default; frequency a_t optional).
  label_by <- reactive({
    lb <- input$label_by
    if (is.null(lb) || !nzchar(lb)) "relevance" else lb
  })
  top3 <- reactive({ req(result_obj()); top_terms(result_obj(), n = 3, by = label_by()) })

  communities_df <- reactive({
    res <- result_obj(); req(res)
    df <- as.data.frame(res)
    cs <- res$network_stats$community_stats
    df <- merge(df, cs[, c("community", "density", "mean_degree", "n_edges")],
                by.x = "community_id", by.y = "community", all.x = TRUE, sort = FALSE)
    t3 <- top3()
    df$top_terms <- vapply(df$community_id, function(cid) {
      paste(t3$term[t3$community == cid], collapse = ", ")
    }, character(1))
    df$label <- NULL
    df <- df[order(as.integer(sub("^C", "", df$community_id))), , drop = FALSE]
    df[, c("community_id", "size", "top_terms", "psi", "cs", "psi_z", "cs_z",
           "quadrant", "density", "mean_degree", "n_edges")]
  })

  # Refresh the community filter of the Top Terms tab after each run
  observeEvent(result_obj(), {
    res <- result_obj(); req(res)
    updateSelectizeInput(session, "tt_comms",
                         choices = names(res$communities), selected = character(0))
  })

  # ---- Value boxes ----
  output$value_boxes <- renderUI({
    res <- result_obj()
    if (is.null(res)) return(NULL)
    gs <- res$network_stats$global_stats
    layout_columns(
      col_widths = c(3, 3, 3, 3), fill = FALSE, class = "mb-3",
      value_box(title = "Terms (nodes)", value = format(gs$n_nodes, big.mark = ","),
                showcase = icon("font"), theme = "primary"),
      value_box(title = "Edges", value = format(gs$n_edges, big.mark = ","),
                showcase = icon("share-nodes"), theme = "secondary"),
      value_box(title = "Communities", value = gs$n_communities,
                showcase = icon("layer-group"), theme = "success"),
      value_box(title = "Modularity",
                value = ifelse(is.na(gs$modularity), "n/a", round(gs$modularity, 3)),
                showcase = icon("circle-half-stroke"), theme = "info")
    )
  })

  # ---- Data tab: Imported data ----
  output$imported_preview_ui <- renderUI({
    if (is.null(collection())) {
      msg <- if (!is.null(words_data()))
        "This session started from an annotated words file. See Pre-processed data."
      else
        "Import a file or load the demo to preview the raw documents."
      return(.empty_state(msg))
    }
    tagList(
      tags$p(class = "small text-muted",
             sprintf("Raw collection: %s documents x %d columns.",
                     format(nrow(collection()), big.mark = ","), ncol(collection()))),
      .spinner(DTOutput("imported_table"))
    )
  })
  output$imported_table <- renderDT({
    df <- collection(); req(df)
    # Truncate long text for display only (the export keeps the full text).
    if ("text" %in% names(df)) {
      df$text <- ifelse(nchar(df$text) > 100L,
                        paste0(substr(df$text, 1L, 100L), "..."),
                        df$text)
    }
    datatable(df, rownames = FALSE, selection = "none",
              options = list(dom = "tip", pageLength = 10, scrollX = TRUE))
  })

  # ---- Data tab: Pre-processed data ----
  output$preprocessed_preview_ui <- renderUI({
    if (is.null(words_data())) {
      return(.empty_state("Run preprocessing (tokenize + POS) to see the annotated tokens."))
    }
    tagList(
      tags$p(class = "small text-muted",
             sprintf("Annotated tokens: %s rows.", format(nrow(words_data()), big.mark = ","))),
      .spinner(DTOutput("preprocessed_table"))
    )
  })
  output$preprocessed_table <- renderDT({
    df <- words_data(); req(df)
    # sentence shown next to sentence_id; token_id hidden.
    keep <- intersect(c("doc_id", "sentence_id", "sentence", "token", "lemma", "upos"),
                      names(df))
    show <- if (length(keep) > 0) df[, keep, drop = FALSE] else df
    datatable(show, rownames = FALSE, selection = "none",
              options = list(dom = "tip", pageLength = 10, scrollX = TRUE))
  })

  # ---- Data tab: exports (CSV or Excel per sub-panel) ----
  .export_ext <- function(fmt) {
    if (identical(fmt, "xlsx") && requireNamespace("writexl", quietly = TRUE)) "xlsx" else "csv"
  }
  .write_df <- function(df, file) {
    if (grepl("\\.xlsx$", file)) writexl::write_xlsx(as.data.frame(df), file)
    else utils::write.csv(df, file, row.names = FALSE)
  }
  output$dl_imported <- downloadHandler(
    filename = function() paste0("themescope_imported.", .export_ext(input$exp_imported_fmt)),
    content  = function(file) { df <- collection(); req(df); .write_df(df, file) }
  )
  output$dl_pre <- downloadHandler(
    filename = function() paste0("themescope_preprocessed.", .export_ext(input$exp_pre_fmt)),
    content  = function(file) { df <- words_data(); req(df); .write_df(df, file) }
  )

  # ---- Map ----
  output$map_ui <- renderUI({
    if (is.null(result_obj())) return(.empty_state())
    .spinner(plotlyOutput("map_plot", height = "600px"))
  })
  output$map_plot <- renderPlotly({
    req(result_obj())
    build_map_plotly(result_obj(), top3 = top3(),
                     label_terms = isTRUE(input$map_label_terms))
  })

  # ---- Communities table ----
  output$communities_ui <- renderUI({
    if (is.null(result_obj())) return(.empty_state())
    .spinner(DTOutput("communities_table"))
  })
  output$communities_table <- renderDT({
    df <- communities_df()
    pal <- .community_palette(result_obj())
    # filter = "top" puts a search box (or a range slider, for the numeric
    # columns) under every header, so each column can be filtered on its own.
    datatable(df, rownames = FALSE, selection = "none", filter = "top",
              container = .dt_header(c(
                "ID", "Size", "Top terms", "PSI", "CS",
                "PSI<sub>z</sub>", "CS<sub>z</sub>",
                "Quadrant", "Density", "Mean degree", "Edges")),
              options = list(dom = "tip", pageLength = 20, scrollX = TRUE)) |>
      formatRound(c("psi", "mean_degree"), 2) |>
      formatRound(c("cs", "psi_z", "cs_z", "density"), 3) |>
      formatStyle("community_id", fontWeight = "bold",
                  backgroundColor = styleEqual(names(pal), unname(pal))) |>
      formatStyle("quadrant", fontWeight = "bold",
                  color = styleEqual(names(.quad_colors), unname(.quad_colors)))
  })
  output$dl_communities <- downloadHandler(
    filename = function() "themescope_communities.csv",
    content = function(file) utils::write.csv(communities_df(), file, row.names = FALSE)
  )

  # ---- Top terms ----
  output$tt_metric_label <- renderUI({
    HTML(if (identical(label_by(), "frequency")) .html_frequency else .html_relevance)
  })

  # ---- Paging: at most 3 community charts per page ----
  tt_page <- reactiveVal(1L)
  tt_comms_shown <- reactive({
    res <- result_obj(); req(res)
    all <- names(res$communities)
    sel <- input$tt_comms
    if (!is.null(sel) && length(sel) > 0) intersect(all, sel) else all
  })
  tt_n_pages <- reactive({ max(1L, ceiling(length(tt_comms_shown()) / 3L)) })
  observeEvent(list(result_obj(), input$tt_comms), { tt_page(1L) }, ignoreInit = TRUE)
  observeEvent(input$tt_prev, { tt_page(max(1L, tt_page() - 1L)) })
  observeEvent(input$tt_next, { tt_page(min(tt_n_pages(), tt_page() + 1L)) })
  tt_page_comms <- reactive({
    comms <- tt_comms_shown()
    if (length(comms) == 0) return(character(0))
    p   <- min(tt_page(), tt_n_pages())
    idx <- seq((p - 1L) * 3L + 1L, min(length(comms), p * 3L))
    comms[idx]
  })

  output$topterms_ui <- renderUI({
    if (is.null(result_obj())) return(.empty_state())
    n <- length(tt_page_comms()); if (n < 1L) n <- 1L
    tagList(
      .spinner(plotlyOutput("topterms_plot", height = paste0(n * 260L, "px"))),
      uiOutput("tt_pager")
    )
  })
  output$tt_pager <- renderUI({
    np <- tt_n_pages()
    if (np <= 1L) return(NULL)
    div(class = "d-flex justify-content-center align-items-center gap-3 mt-2",
        actionButton("tt_prev", icon("chevron-left"),
                     class = "btn-sm btn-outline-secondary"),
        tags$span(class = "small text-muted",
                  sprintf("Page %d of %d", min(tt_page(), np), np)),
        actionButton("tt_next", icon("chevron-right"),
                     class = "btn-sm btn-outline-secondary"))
  })
  output$topterms_plot <- renderPlotly({
    res <- result_obj(); req(res)
    comms <- tt_page_comms(); req(length(comms) > 0)
    n  <- input$top_n_terms
    if (is.null(n) || is.na(n)) n <- 10
    tt <- top_terms(res, n = max(3, min(30, n)), by = label_by())
    build_topterms_plotly(tt, pal = .community_palette(res),
                          communities = comms, metric = label_by())
  })
  # ---- Top terms as collapsible lists (one closed panel per cluster) ----
  output$topterms_table_ui <- renderUI({
    res <- result_obj()
    if (is.null(res)) return(.empty_state())
    n <- input$top_n_terms; if (is.null(n) || is.na(n)) n <- 10
    tt <- top_terms(res, n = max(3, min(30, n)), by = label_by())
    sel <- input$tt_comms
    if (!is.null(sel) && length(sel) > 0) {
      tt <- tt[tt$community %in% sel, , drop = FALSE]
    }
    tagList(
      tags$p(class = "small text-muted mb-2",
             "One panel per cluster, closed by default. Open a cluster to see ",
             "its terms in rank order."),
      build_topterms_accordion(tt, pal = .community_palette(res),
                               metric = label_by())
    )
  })

  output$dl_terms <- downloadHandler(
    filename = function() "themescope_top_terms.csv",
    content = function(file) {
      res <- result_obj(); req(res)
      n <- input$top_n_terms
      if (is.null(n) || is.na(n)) n <- 10
      utils::write.csv(top_terms(res, n = max(3, min(30, n)), by = label_by()),
                       file, row.names = FALSE)
    }
  )

  # ---- Network ----
  output$network_ui <- renderUI({
    if (is.null(result_obj())) return(.empty_state())
    .spinner(visNetworkOutput("network_plot", height = "640px"))
  })
  # Depends on net_opts(), not on the individual controls, so moving a slider
  # does not redraw anything until Refine is clicked.
  output$network_plot <- renderVisNetwork({
    req(result_obj())
    build_network_visnetwork(result_obj(), opts = net_opts())
  })

  # ---- Reproduce: the exact console equivalent of the current run ----
  output$repro_ui <- renderUI({
    res <- result_obj()
    if (is.null(res)) return(.empty_state("Run the analysis to get the equivalent console code."))
    tagList(
      tags$p(class = "small text-muted",
             "This code reproduces the current GUI results from the R console."),
      tags$pre(class = "p-3 rounded", style = "background:#f6f8fa; font-size:0.85rem;",
               textOutput("repro_code", container = tags$code))
    )
  })
  output$repro_code <- renderText({
    res <- result_obj(); req(res)
    p <- res$params
    src <- if (is.null(data_source())) "" else data_source()
    tb  <- input$annot_treebank

    # The demo and an archive arrive annotated, so their code skips udpipe.
    if (identical(src, "demo")) {
      data_line <- paste0(
        'words <- readRDS(system.file("extdata", "demo_annotated.rds",\n',
        '                             package = "themescopeR"))')
      annot_line <- ""
    } else if (identical(src, "archive")) {
      data_line  <- paste0('archive <- read_themescope("your_analysis.themescope")\n',
                           'words   <- archive$words')
      annot_line <- ""
    } else if (!is.null(collection())) {
      data_line <- 'coll  <- read_collection("your_file.csv", sequential_ids = TRUE)'
      annot_line <- if (!is.null(tb) && nzchar(tb)) {
        paste0('model <- ts_download_model("', input$language, '", treebank = "', tb, '")\n',
               'words <- preprocess_texts(coll, model = model)')
      } else {
        paste0('words <- preprocess_texts(coll, model = "', input$language, '")')
      }
    } else {
      data_line  <- 'words <- read.csv("your_words_file.csv")  # the annotated file you uploaded'
      annot_line <- ""
    }
    paste0(
      "library(themescopeR)\n\n",
      data_line, "\n",
      if (nzchar(annot_line)) paste0(annot_line, "\n") else "",
      "\nresult <- themescope(\n",
      "  words,\n",
      '  unit                 = "', p$unit, '",\n',
      "  vocab_size           = ", if (is.null(p$vocab_size)) "NULL" else p$vocab_size, ",\n",
      '  pos_filter           = c(', paste0('"', p$pos_filter, '"', collapse = ", "), "),\n",
      '  window               = "', p$window, '",\n',
      '  normalization        = "', p$normalization, '",\n',
      "  threshold_percentile = ", p$threshold_percentile, ",\n",
      '  community_algorithm  = "', p$community_algorithm, '",\n',
      if (identical(p$community_algorithm, "walktrap")) {
        paste0("  walktrap_steps       = ", p$walktrap_steps, ",\n")
      } else {
        paste0("  resolution           = ", p$resolution, ",\n")
      },
      "  min_community_size   = ", p$min_community_size, ",\n",
      "  seed                 = ", if (is.null(p$seed)) "NULL" else p$seed, "\n",
      ")\n\n",
      if (identical(label_by(), "frequency")) {
        paste0(
          'plot(result, type = "map", label = "terms", label_by = "frequency")  # strategic diagram\n',
          'plot(result, type = "network")                                       # community-coloured network\n',
          'top_terms(result, n = 10, by = "frequency")                          # representative terms\n',
          "as.data.frame(result)                                                # community metrics table"
        )
      } else {
        paste0(
          'plot(result, type = "map", label = "terms")  # strategic diagram\n',
          'plot(result, type = "network")               # community-coloured network\n',
          "top_terms(result, n = 10)                    # representative terms (relevance R_t)\n",
          "as.data.frame(result)                        # community metrics table"
        )
      },
      "\n\n",
      '# Save everything (corpus, parameters, term scores) in one file\n',
      'save_themescope(result, "my_analysis.themescope", words = words)'
    )
  })
}

shinyApp(ui = ui, server = server)
