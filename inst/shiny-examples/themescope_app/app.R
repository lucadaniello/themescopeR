# ==============================================================================
# themescopeR — Shiny GUI
#
# This app is a *thin* graphical front-end: all computation is delegated to the
# exported package functions
#   read_collection() -> preprocess_texts() -> themescope() -> top_terms()
# The helpers below only turn the returned `themescope` object into interactive
# widgets (plotly / visNetwork / DT); they do not re-implement any analysis
# logic. Communities use themescope_colours() so the map, the network and the
# top-terms bars share the exact colours of the console plots.
# Launch it with themescopeR::run_themescope().
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
# large files load without friction; the "large-file mode" toggle in the Data
# panel lets users raise it further, after an explicit warning about the memory
# implications. run_themescope(max_upload_size_mb = ...) can override the
# baseline from R. Shiny re-reads this option on every upload, so bumping it at
# runtime (before the file is picked) takes effect immediately.
.baseline_upload_mb <- 50L
if (is.null(getOption("shiny.maxRequestSize")) ||
    getOption("shiny.maxRequestSize") < .baseline_upload_mb * 1024^2) {
  options(shiny.maxRequestSize = .baseline_upload_mb * 1024^2)
}

# ---- Citation ----------------------------------------------------------------
# Using themescopeR / ThemeScope requires citing the paper below.
.cite_doi_url <- "https://doi.org/10.1177/01655515261454276"
.cite_apa <- paste0(
  "Misuraca, M., Spano, M., & D’Aniello, L. (2026). ThemeScope: A ",
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

# ---- Demo dataset (bundled raw climate sample) -------------------------------
.demo_path  <- system.file("extdata", "sample_collection.csv", package = "themescopeR")
.demo_coll  <- if (nzchar(.demo_path)) read_collection(.demo_path) else NULL
.demo_ndocs <- if (!is.null(.demo_coll)) nrow(.demo_coll) else 0L

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
    "<b>", comm_names, "</b> — ", term_str, "<br>",
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
      xaxis = list(title = "<i>PSI<sub>z</sub></i>  (Prototypical Salience — anchoring)",
                   zeroline = TRUE, zerolinecolor = "#cccccc", showgrid = FALSE),
      yaxis = list(title = if (cs_all_na) "CS not available" else
                     "<i>CS<sub>z</sub></i>  (Concreteness — objectification)",
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
  value_title <- if (identical(metric, "frequency")) "Frequency (a_t)" else "Relevance (R_t)"
  hover_fmt   <- if (identical(metric, "frequency")) "%{x:.0f}" else "%{x:.3f}"

  tt <- tt[!is.na(tt$term), , drop = FALSE]
  if (!is.null(communities) && length(communities) > 0) {
    tt <- tt[tt$community %in% communities, , drop = FALSE]
  }
  comms <- intersect(names(pal), unique(tt$community))
  validate(need(length(comms) > 0, "No community selected."))

  ncols <- min(2L, length(comms))
  nrows <- ceiling(length(comms) / ncols)

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
                                x = 0.5, y = 1.04, xref = "paper", yref = "paper",
                                showarrow = FALSE, font = list(size = 12))),
        xaxis = list(title = value_title, showgrid = TRUE, gridcolor = "#eeeeee"),
        yaxis = list(title = "", automargin = TRUE))
  })

  subplot(plots, nrows = nrows, shareX = FALSE, shareY = FALSE,
          titleX = TRUE, titleY = TRUE, margin = 0.05) |>
    layout(plot_bgcolor = "white", paper_bgcolor = "white",
           margin = list(t = 40)) |>
    config(displayModeBar = FALSE)
}

# ---- Helper: network as visNetwork --------------------------------------------
# `repulsion` (0–1) spreads communities apart: it scales the force-atlas
# gravitational constant (more negative = stronger repulsion) and the spring
# length, so higher values push clusters further from each other.
build_network_visnetwork <- function(result, hide_unclassified = FALSE,
                                      repulsion = 0.3) {
  repulsion <- max(0, min(1, if (is.null(repulsion) || is.na(repulsion)) 0.3 else repulsion))
  grav_const  <- -50 - repulsion * 550   # -50 (rep=0) .. -600 (rep=1)
  spring_len  <- 100 + repulsion * 300   # 100 .. 400
  g    <- result$graph
  memb <- result$membership
  deg  <- igraph::degree(g)
  vnames   <- igraph::V(g)$name
  comm_ids <- memb[vnames]

  if (hide_unclassified) {
    keep <- !is.na(comm_ids)
    vnames <- vnames[keep]; comm_ids <- comm_ids[keep]; deg <- deg[keep]
    g <- igraph::induced_subgraph(g, vids = vnames)
  }

  # Community i uses palette colour i — identical to the map and console plots.
  pal <- themescope_colours(max(c(comm_ids, 1L), na.rm = TRUE))

  nodes <- data.frame(
    id = vnames, label = vnames,
    title = paste0("<b>", vnames, "</b><br>Community: ",
                   ifelse(is.na(comm_ids), "—", paste0("C", comm_ids)),
                   "<br>Degree: ", deg),
    group = ifelse(is.na(comm_ids), "Unclassified", paste0("C", comm_ids)),
    value = as.numeric(10 + log1p(deg) * 5),
    color = ifelse(is.na(comm_ids), "#d9d9d9", pal[comm_ids]),
    stringsAsFactors = FALSE)

  el <- igraph::as_edgelist(g); w <- igraph::E(g)$weight
  wn <- (w - min(w)) / (max(w) - min(w) + 1e-9)
  edges <- data.frame(from = el[, 1], to = el[, 2], value = wn * 3 + 0.3,
                      color = sprintf("rgba(150,150,150,%.2f)", 0.15 + wn * 0.5),
                      title = paste0("Weight: ", round(w, 5)), stringsAsFactors = FALSE)

  visNetwork(nodes, edges, width = "100%", height = "100%") |>
    visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
               selectedBy = list(variable = "group", main = "Filter by community"),
               nodesIdSelection = list(enabled = TRUE, main = "Find a term")) |>
    visPhysics(solver = "forceAtlas2Based",
               forceAtlas2Based = list(gravitationalConstant = grav_const,
                                       springLength = spring_len),
               stabilization = list(enabled = TRUE, iterations = 300, fit = TRUE)) |>
    visNodes(shape = "dot", scaling = list(min = 8, max = 40),
             font = list(size = 16)) |>
    visInteraction(navigationButtons = TRUE, tooltipDelay = 100)
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
  title = tags$span(
    style = "display:flex; align-items:center; gap:0.7rem;",
    tags$img(src = "themescope_logo.svg", height = "40px", alt = "themescopeR logo"),
    tags$span("themescopeR", style = "font-weight:600; letter-spacing:0.04em;"),
    tags$span("mapping social representations in digital discourse",
              class = "d-none d-lg-inline", style = "font-size:0.8rem; opacity:0.75;")),
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

    # Before import: size hint + browse + import + demo
    conditionalPanel(
      condition = "!output.has_data",
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
                   class = "btn-primary w-100"),
      tags$hr(class = "my-2"),
      actionButton("load_demo",
                   tagList(icon("seedling"), " Load demo — Reddit climate"),
                   class = "btn-outline-primary btn-sm w-100")
    ),

    # After import: badge + reset (starts the app over)
    conditionalPanel(
      condition = "output.has_data",
      uiOutput("data_badge"),
      actionButton("reset_app", tagList(icon("rotate-left"), " Reset"),
                   class = "btn-outline-danger btn-sm w-100 mt-2")
    ),

    tags$hr(class = "my-2"),

    # ---- Pipeline parameters: only after data is imported ----
    conditionalPanel(
      condition = "output.has_data",
    accordion(
      multiple = FALSE, open = "Preprocessing",
      accordion_panel(
        "Preprocessing", icon = icon("language"),
        selectInput("language", "Annotation language (udpipe)",
                    choices = .languages, selected = "english"),
        radioButtons("unit", "Word unit",
                     choices = c("Lemma" = "lemma", "Token" = "token"),
                     selected = "lemma", inline = TRUE),
        helpText("Models are downloaded once and cached locally.",
                 class = "small text-muted")
      ),
      accordion_panel(
        "Vocabulary & network", icon = icon("diagram-project"),
        numericInput("vocab_size", "Vocabulary size (blank = all terms)",
                     value = 1500, min = 50, step = 50),
        checkboxGroupInput("pos_filter", "POS filter — content words",
                           choices = .pos_content,
                           selected = c("NOUN", "ADJ", "PROPN"), inline = TRUE),
        selectizeInput("pos_extra", "Additional UPOS tags (optional)",
                       choices = .pos_other, multiple = TRUE,
                       options = list(placeholder = "None")),
        selectInput("window", "Co-occurrence window",
                    choices = c("Sentence" = "sentence", "Document" = "document")),
        selectInput("normalization", "Similarity measure",
                    choices = c("Association strength (ThemeScope)" = "association",
                                "Equivalence" = "equivalence",
                                "Jaccard" = "jaccard",
                                "Salton (cosine)" = "salton",
                                "Inclusion" = "inclusion",
                                "Raw frequency" = "frequency")),
        sliderInput("threshold_percentile", "Edge threshold percentile",
                    min = 0.80, max = 0.99, value = 0.98, step = 0.01)
      ),
      accordion_panel(
        "Communities", icon = icon("layer-group"),
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
      ),
      accordion_panel(
        "Concreteness", icon = icon("cube"),
        tags$p(tags$strong("Lexicon: "), uiOutput("lexicon_badge", inline = TRUE),
               class = "mb-2"),
        fileInput("lexicon_upload", "Override (optional CSV: word, conc.m)",
                  accept = ".csv", placeholder = "Bundled Brysbaert",
                  buttonLabel = "Browse")
      )
    ),
    actionButton("run_analysis", tagList(icon("play"), " Run analysis"),
                 class = "btn-primary w-100 mt-1"),
    uiOutput("analysis_status_badge")
    )  # end conditionalPanel (pipeline params)
  ),

  # ---- Headline value boxes (appear after the first run) --------------------
  uiOutput("value_boxes"),

  # ---- Main tabs -------------------------------------------------------------
  navset_card_tab(
    id = "main_tabs", selected = "info",

    nav_panel(
      value = "info", title = tagList(icon("circle-info"), " Info"),
      div(class = "container-fluid py-3 px-3", style = "max-width: 1000px;",
        div(class = "d-flex align-items-center gap-3 mb-3",
            tags$img(src = "themescope_logo.svg", height = "110px",
                     alt = "themescopeR logo"),
            div(tags$h3("themescopeR", class = "mb-0", style = "color:#3A3A38;"),
                tags$p(class = "text-muted mb-0",
                       "Map social representations in digital discourse from raw text."))),
        tags$ol(
          tags$li(tags$strong("Load data"), " — upload raw documents (or an annotated words file), or load the demo."),
          tags$li(tags$strong("Choose the preprocessing"), " — udpipe language (cached after first use) and word unit (lemma/token)."),
          tags$li(tags$strong("Tune the pipeline"), " — vocabulary, co-occurrence window, similarity measure, edge threshold and community detection."),
          tags$li(tags$strong("Run"), " and explore the Map, Communities, Top Terms and Network tabs.")),
        tags$h5("The four SRT regions", class = "mt-4"),
        layout_columns(
          col_widths = c(6, 6),
          card(class = "border-0", style = paste0("background:", .quad_colors[["Stable Core"]], "1a;"),
               card_body(tags$strong("Stable Core", style = paste0("color:", .quad_colors[["Stable Core"]])),
                         tags$small("High PSI, high CS — consolidated, concrete themes at the heart of the discourse."))),
          card(class = "border-0", style = paste0("background:", .quad_colors[["Ideological Core"]], "1a;"),
               card_body(tags$strong("Ideological Core", style = paste0("color:", .quad_colors[["Ideological Core"]])),
                         tags$small("High PSI, low CS — central but abstract, value-laden themes."))),
          card(class = "border-0", style = paste0("background:", .quad_colors[["Emerging Practices"]], "1a;"),
               card_body(tags$strong("Emerging Practices", style = paste0("color:", .quad_colors[["Emerging Practices"]])),
                         tags$small("Low PSI, high CS — concrete themes not (yet) anchored in the core."))),
          card(class = "border-0", style = paste0("background:", .quad_colors[["Latent Representations"]], "1a;"),
               card_body(tags$strong("Latent Representations", style = paste0("color:", .quad_colors[["Latent Representations"]])),
                         tags$small("Low PSI, low CS — peripheral, abstract themes.")))),
        tags$hr(),
        tags$p(class = "small text-muted",
               "All computation uses the exported package functions read_collection(), ",
               "preprocess_texts(), themescope() and top_terms(); the GUI reproduces the ",
               "console results exactly (see the Reproduce tab). ",
               "Concreteness norms: Brysbaert et al. (2014). ",
               "Updated udpipe models: tall.language.models (M. Aria).")
        ,
        tags$hr(),
        div(class = "card border-0", style = "background:#1D9E7514;",
          div(class = "card-body",
            tags$h5(tagList(icon("quote-right"), " How to cite"), class = "mb-2",
                    style = "color:#3A3A38;"),
            tags$p(class = "mb-2",
                   "Using themescopeR / ThemeScope in your work ", tags$b("requires"),
                   " citing the paper below. Please do not use it without attribution."),
            radioButtons("cite_format", NULL,
                         choices = c("APA" = "apa", "BibTeX" = "bibtex"),
                         selected = "apa", inline = TRUE),
            uiOutput("citation_box_ui"),
            div(class = "d-flex gap-2 mt-2",
              tags$button(
                id = "copy_citation", type = "button", class = "btn btn-sm btn-primary",
                onclick = paste0(
                  "navigator.clipboard.writeText(",
                  "document.getElementById('citation_text').innerText).then(function(){",
                  "var b=document.getElementById('copy_citation');",
                  "var t=b.innerHTML;b.innerHTML='Copied!';",
                  "setTimeout(function(){b.innerHTML=t;},1500);});"),
                tagList(icon("copy"), " Copy")),
              tags$a(href = .cite_doi_url, target = "_blank",
                     class = "btn btn-sm btn-outline-secondary",
                     tagList(icon("up-right-from-square"), " Read the paper"))
            )
          )
        )
      )
    ),

    nav_panel(
      value = "data", title = tagList(icon("table"), " Data"),
      card(card_header("Loaded data preview"),
           card_body(uiOutput("data_preview_ui")))
    ),

    nav_panel(
      value = "map", title = tagList(icon("map"), " Map"),
      card(full_screen = TRUE,
        card_header(class = "d-flex justify-content-between align-items-center",
          span("ThemeScope representational map"),
          div(style = "min-width:220px;",
              input_switch("map_label_terms", "Label with top terms", value = FALSE))),
        card_body(uiOutput("map_ui")))
    ),

    nav_panel(
      value = "communities", title = tagList(icon("layer-group"), " Communities"),
      card(card_header(class = "d-flex justify-content-between align-items-center",
                       span("Community metrics & structure"),
                       downloadButton("dl_communities", "CSV", class = "btn-sm btn-outline-secondary")),
           card_body(uiOutput("communities_ui")))
    ),

    nav_panel(
      value = "terms", title = tagList(icon("list-ol"), " Top Terms"),
      card(full_screen = TRUE,
        card_header(
          div(class = "d-flex flex-wrap justify-content-between align-items-center gap-2",
              span("Top terms by community — ", textOutput("tt_metric_label", inline = TRUE)),
              div(class = "d-flex align-items-center gap-2",
                  selectizeInput("tt_comms", label = NULL, choices = NULL, multiple = TRUE,
                                 width = "320px",
                                 options = list(placeholder = "All communities")),
                  numericInput("top_n_terms", label = NULL, value = 10, min = 3, max = 30,
                               width = "90px"),
                  downloadButton("dl_terms", "CSV", class = "btn-sm btn-outline-secondary")))),
        card_body(uiOutput("topterms_ui")))
    ),

    nav_panel(
      value = "network", title = tagList(icon("circle-nodes"), " Network"),
      card(full_screen = TRUE,
        card_header(class = "d-flex flex-wrap justify-content-between align-items-center gap-3",
          span("Semantic co-occurrence network"),
          div(class = "d-flex align-items-center gap-3",
              div(style = "min-width:200px;",
                  sliderInput("net_repulsion", "Repulsion", min = 0, max = 1,
                              value = 0.3, step = 0.05, width = "200px")),
              div(style = "min-width:150px;",
                  input_switch("net_hide_unclassified", "Only classified", value = FALSE)))),
        card_body(uiOutput("network_ui")))
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

  # ---- Reveal flag: drives the sidebar (Import vs Reset, pipeline controls) ----
  output$has_data <- reactive({
    !is.null(collection()) || !is.null(words_data())
  })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)

  # ---- Expected file size sets Shiny's upload cap ----
  # Shiny reads this option at upload time, so it must be set before the user
  # browses (hence the numeric input sits above the Browse button).
  observeEvent(input$expected_size_mb, {
    mb <- suppressWarnings(as.numeric(input$expected_size_mb))
    if (is.na(mb) || mb < 1) mb <- .baseline_upload_mb
    options(shiny.maxRequestSize = mb * 1024^2)
  }, ignoreInit = FALSE)

  # ---- Info modal: how the import process works ----
  observeEvent(input$import_info, {
    showModal(modalDialog(
      title = tagList(icon("circle-info"), " How importing works"),
      easyClose = TRUE,
      tags$ol(
        tags$li(tags$b("Set the expected file size"), " (MB) if your file is ",
                "larger than ", .baseline_upload_mb, " MB — this raises the upload ",
                "limit before the file is transferred."),
        tags$li(tags$b("Browse"), " and pick one file: raw documents ",
                "(.csv/.tsv/.xlsx/.RData/.txt, or a .zip of many) with ",
                tags$code("doc_id"), " + ", tags$code("text"), ", or an already ",
                "annotated words file (", tags$code("doc_id"), ", ",
                tags$code("sentence_id"), ", ", tags$code("lemma"), "/",
                tags$code("token"), ", ", tags$code("upos"), ")."),
        tags$li(tags$b("Click Import"), " to read the file into memory. Only ",
                "then do the preprocessing and analysis controls appear.")
      ),
      tags$p(class = "mb-1", tags$b("What to expect for large files:")),
      tags$ul(
        tags$li("The whole file is loaded into RAM; keep roughly ",
                tags$b("2–5× the file size"), " free, or R may slow down / crash."),
        tags$li("Very large uploads take time to transfer through the browser."),
        tags$li("udpipe annotation of big collections can take minutes to hours.")
      ),
      tags$p(class = "small text-muted mb-0",
             "Use ", tags$b("Reset"), " (top of the sidebar, after import) to ",
             "clear everything and start over."),
      footer = modalButton("Got it")
    ))
  })

  # ---- Demo ----
  observeEvent(input$load_demo, {
    if (is.null(.demo_coll)) {
      showNotification("Demo data not found in the package.", type = "error"); return()
    }
    collection(.demo_coll); words_data(NULL); annot_key(NULL)
    result_obj(NULL); data_source("demo")
    showNotification(sprintf("Demo loaded: %s raw documents.",
                             format(.demo_ndocs, big.mark = ",")), type = "message")
    nav_select("main_tabs", "data")
  })

  # ---- Import (raw documents OR annotated words), triggered by the button ----
  observeEvent(input$do_import, {
    if (is.null(input$file_upload)) {
      showNotification("Choose a file with Browse first, then click Import.",
                       type = "warning"); return()
    }
    tryCatch({
      path <- input$file_upload$datapath
      coll <- tryCatch(read_collection(path), error = function(e) NULL)
      if (!is.null(coll) && "text" %in% names(coll)) {
        collection(coll); words_data(NULL); annot_key(NULL)
        showNotification(sprintf("Loaded %d raw documents.", nrow(coll)), type = "message")
      } else {
        sep <- if (tolower(tools::file_ext(input$file_upload$name)) == "tsv") "\t" else ","
        df <- utils::read.csv(path, sep = sep, stringsAsFactors = FALSE, check.names = FALSE)
        if (!"lemma" %in% names(df) && "token" %in% names(df)) df$lemma <- df$token
        validate_words_df(df)
        for (c0 in intersect(c("doc_id", "sentence_id", "token", "lemma", "upos"), names(df)))
          df[[c0]] <- as.character(df[[c0]])
        collection(NULL); words_data(df); annot_key("uploaded-words")
        showNotification(sprintf("Loaded annotated words: %d rows.", nrow(df)), type = "message")
      }
      result_obj(NULL); data_source("upload")
      nav_select("main_tabs", "data")
    }, error = function(e) {
      showNotification(paste("Error reading file:", conditionMessage(e)),
                       type = "error", duration = 8)
    })
  })

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

  # ---- Run analysis (delegates everything to the exported API) ----
  observeEvent(input$run_analysis, {
    if (is.null(collection()) && is.null(words_data())) {
      showNotification("No data loaded. Upload a file or load the demo.", type = "warning")
      return()
    }
    pos_filter <- union(input$pos_filter, input$pos_extra)
    if (length(pos_filter) == 0) {
      showNotification("Select at least one POS tag.", type = "warning"); return()
    }

    withProgress(message = "Running themescopeR ...", value = 0, {
      tryCatch({
        # --- annotate raw text if needed (memoised per collection + language) ---
        if (!is.null(collection())) {
          key <- paste0(data_source(), "-", nrow(collection()), "-", input$language)
          if (is.null(words_data()) || !identical(annot_key(), key)) {
            setProgress(0.1, detail = paste0("Annotating with udpipe (", input$language, ") ..."))
            words <- preprocess_texts(collection(), model = input$language, verbose = FALSE)
            words_data(words); annot_key(key)
          }
        }
        req(words_data())

        vocab_size <- if (is.null(input$vocab_size) || is.na(input$vocab_size)) {
          NULL
        } else {
          as.integer(input$vocab_size)
        }

        setProgress(0.7, detail = "Building network & metrics ...")
        res <- themescope(
          words_data(),
          unit                 = input$unit,
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
        nav_select("main_tabs", "map")
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
                value = ifelse(is.na(gs$modularity), "—", round(gs$modularity, 3)),
                showcase = icon("circle-half-stroke"), theme = "info")
    )
  })

  # ---- Data preview ----
  output$data_preview_ui <- renderUI({
    if (is.null(collection()) && is.null(words_data())) {
      return(.empty_state("Upload a file or load the demo to preview the data."))
    }
    tagList(
      if (!is.null(collection()))
        tags$p(class = "small text-muted",
               sprintf("Raw collection: %s documents x %d columns (first 200 shown). It will be annotated with udpipe on the first run.",
                       format(nrow(collection()), big.mark = ","), ncol(collection())))
      else
        tags$p(class = "small text-muted",
               sprintf("Annotated words: %s rows (first 200 shown).",
                       format(nrow(words_data()), big.mark = ","))),
      .spinner(DTOutput("data_preview"))
    )
  })
  output$data_preview <- renderDT({
    df <- if (!is.null(collection())) collection() else words_data()
    req(df)
    datatable(utils::head(df, 200), rownames = FALSE, selection = "none",
              options = list(dom = "tp", pageLength = 10, scrollX = TRUE))
  })

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
    datatable(df, rownames = FALSE, selection = "none",
              colnames = c("ID", "Size", "Top terms", "PSI", "CS", "PSI (z)", "CS (z)",
                           "Quadrant", "Density", "Mean degree", "Edges"),
              options = list(dom = "tp", pageLength = 20, scrollX = TRUE)) |>
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
  output$tt_metric_label <- renderText({
    if (identical(label_by(), "frequency")) "frequency (a_t)" else "relevance (R_t)"
  })
  output$topterms_ui <- renderUI({
    if (is.null(result_obj())) return(.empty_state())
    .spinner(plotlyOutput("topterms_plot", height = "620px"))
  })
  output$topterms_plot <- renderPlotly({
    res <- result_obj(); req(res)
    n  <- input$top_n_terms
    if (is.null(n) || is.na(n)) n <- 10
    tt <- top_terms(res, n = max(3, min(30, n)), by = label_by())
    build_topterms_plotly(tt, pal = .community_palette(res),
                          communities = input$tt_comms, metric = label_by())
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
  output$network_plot <- renderVisNetwork({
    req(result_obj())
    rep_val <- if (is.null(input$net_repulsion)) 0.3 else input$net_repulsion
    build_network_visnetwork(result_obj(),
                             hide_unclassified = isTRUE(input$net_hide_unclassified),
                             repulsion = rep_val)
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
    data_line <- if (identical(data_source(), "demo")) {
      paste0('coll  <- read_collection(system.file("extdata", "sample_collection.csv",',
             ' package = "themescopeR"))')
    } else if (!is.null(collection())) {
      'coll  <- read_collection("your_file.csv")  # the file you uploaded'
    } else {
      'words <- read.csv("your_words_file.csv")   # the annotated file you uploaded'
    }
    annot_line <- if (!is.null(collection())) {
      paste0('words <- preprocess_texts(coll, model = "', input$language, '")')
    } else ""
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
      }
    )
  })
}

shinyApp(ui = ui, server = server)
