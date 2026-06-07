# ==============================================================================
# themescopeR — Shiny GUI
#
# This app is a *thin* graphical front-end: all computation is delegated to the
# exported package functions
#   read_collection()  ->  preprocess_texts()  ->  themescope()  ->  top_terms()
# The helpers below only turn the returned `themescope` object into interactive
# widgets; they do not re-implement any of the analysis logic.
# Launch it with themescopeR::run_themescope().
# ==============================================================================

library(themescopeR)
library(shiny)
library(bslib)
library(DT)
library(plotly)
library(visNetwork)
library(shinycssloaders)

# ---- Demo dataset (bundled raw climate sample) -------------------------------
.demo_path  <- system.file("extdata", "sample_collection.csv", package = "themescopeR")
.demo_coll  <- if (nzchar(.demo_path)) read_collection(.demo_path) else NULL
.demo_ndocs <- if (!is.null(.demo_coll)) nrow(.demo_coll) else 0L

# ---- Languages offered for annotation ----------------------------------------
.languages <- c("english", "italian", "french", "spanish", "german",
                "portuguese", "dutch")

# ---- Helper: is this an already-annotated tokens file? -----------------------
.looks_like_tokens <- function(df) {
  all(c("doc_id", "sentence_id", "upos") %in% names(df)) &&
    any(c("lemma", "token") %in% names(df))
}

# ---- Helper: top terms (uses the exported top_terms()) -----------------------
build_topterms_plotly <- function(tt, custom_labels = NULL) {
  tt <- tt[!is.na(tt$term), ]
  tt$community_label <- if (!is.null(custom_labels)) {
    ifelse(tt$community %in% names(custom_labels), custom_labels[tt$community], tt$community)
  } else tt$community

  comms   <- unique(tt$community_label)
  ncols   <- min(2L, length(comms))
  nrows   <- ceiling(length(comms) / ncols)
  pal     <- setNames(grDevices::hcl.colors(length(comms), palette = "Set2"), comms)

  plots <- lapply(comms, function(clab) {
    sub <- tt[tt$community_label == clab, ]
    sub <- sub[order(sub$relevance), ]
    plot_ly(sub, x = ~relevance, y = ~reorder(term, relevance),
            type = "bar", orientation = "h",
            marker = list(color = pal[clab], opacity = 0.85),
            hovertemplate = "<b>%{y}</b><br>Relevance: %{x:.3f}<extra></extra>",
            showlegend = FALSE) |>
      layout(
        annotations = list(list(text = paste0("<b>", clab, "</b>"),
                                x = 0.5, y = 1.05, xref = "paper", yref = "paper",
                                showarrow = FALSE, font = list(size = 11))),
        xaxis = list(title = "Relevance", showgrid = TRUE, gridcolor = "#eeeeee"),
        yaxis = list(title = "", automargin = TRUE))
  })

  subplot(plots, nrows = nrows, shareX = FALSE, shareY = FALSE,
          titleX = TRUE, titleY = TRUE, margin = 0.06) |>
    layout(title = list(text = "<b>Top Terms per Community (Relevance)</b>",
                        font = list(size = 14)),
           plot_bgcolor = "white", paper_bgcolor = "white", margin = list(t = 60)) |>
    config(displayModeBar = FALSE)
}

# ---- Helper: ThemeScope map as plotly ----------------------------------------
build_map_plotly <- function(result, custom_labels = NULL) {
  comm_sizes <- vapply(result$communities, length, integer(1))
  psi_z <- themescopeR::zscore(result$psi)
  cs_all_na <- all(is.na(result$cs))
  cs_z  <- if (cs_all_na) rep(0, length(result$cs)) else suppressWarnings(themescopeR::zscore(result$cs))

  comm_names <- names(result$psi)
  labels <- if (!is.null(custom_labels)) {
    ifelse(comm_names %in% names(custom_labels), custom_labels[comm_names], comm_names)
  } else comm_names

  quad_colors <- c("Stable Core" = "#1D9E75", "Ideological Core" = "#EF9F27",
                   "Emerging Practices" = "#7F77DD", "Latent Representations" = "#D85A30",
                   "N/A" = "#aaaaaa")
  quadrant <- as.character(themescopeR::assign_quadrant(psi_z, cs_z))
  quadrant[is.na(quadrant)] <- "N/A"
  node_color <- quad_colors[quadrant]
  node_size  <- 10 + (comm_sizes / max(comm_sizes)) * 25

  hover <- paste0("<b>", labels, "</b><br>Size: ", comm_sizes, " terms<br>",
                  "PSI (z): ", round(psi_z, 3), "<br>",
                  if (cs_all_na) "" else paste0("CS (z): ", round(cs_z, 3), "<br>"),
                  "Quadrant: ", quadrant)
  xr <- range(c(psi_z, 0), na.rm = TRUE); yr <- range(c(cs_z, 0), na.rm = TRUE)
  xp <- diff(xr) * 0.25; yp <- diff(yr) * 0.25

  annot <- list(
    list(x = xr[2]+xp*.3, y = yr[2]+yp*.3, text = "<i>Stable Core</i>", showarrow = FALSE,
         font = list(color = quad_colors[["Stable Core"]], size = 11)),
    list(x = xr[2]+xp*.3, y = yr[1]-yp*.3, text = "<i>Ideological Core</i>", showarrow = FALSE,
         font = list(color = quad_colors[["Ideological Core"]], size = 11)),
    list(x = xr[1]-xp*.3, y = yr[2]+yp*.3, text = "<i>Emerging Practices</i>", showarrow = FALSE,
         font = list(color = quad_colors[["Emerging Practices"]], size = 11)),
    list(x = xr[1]-xp*.3, y = yr[1]-yp*.3, text = "<i>Latent Representations</i>", showarrow = FALSE,
         font = list(color = quad_colors[["Latent Representations"]], size = 11)))

  plot_ly(x = psi_z, y = cs_z, type = "scatter", mode = "markers+text",
          text = labels, textposition = "top center", hovertext = hover, hoverinfo = "text",
          marker = list(color = node_color, size = node_size,
                        line = list(color = "white", width = 1.5), opacity = 0.88),
          textfont = list(size = 10)) |>
    layout(title = list(text = "<b>ThemeScope Representational Map</b>", font = list(size = 15)),
           xaxis = list(title = "<i>PSI<sub>z</sub></i>  (Prototypical Salience — anchoring)",
                        zeroline = TRUE, zerolinecolor = "#cccccc", showgrid = FALSE),
           yaxis = list(title = if (cs_all_na) "CS not available" else
                          "<i>CS<sub>z</sub></i>  (Concreteness — objectification)",
                        zeroline = TRUE, zerolinecolor = "#cccccc", showgrid = FALSE),
           shapes = list(
             list(type = "line", x0 = 0, x1 = 0, y0 = yr[1]-yp, y1 = yr[2]+yp,
                  line = list(color = "#bbbbbb", width = 1, dash = "dash")),
             list(type = "line", y0 = 0, y1 = 0, x0 = xr[1]-xp, x1 = xr[2]+xp,
                  line = list(color = "#bbbbbb", width = 1, dash = "dash"))),
           annotations = annot, showlegend = FALSE,
           plot_bgcolor = "white", paper_bgcolor = "white",
           margin = list(t = 60, b = 60, l = 70, r = 40)) |>
    config(displayModeBar = TRUE)
}

# ---- Helper: network as visNetwork --------------------------------------------
build_network_visnetwork <- function(result, hide_unclassified = FALSE) {
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

  unique_comms <- sort(unique(stats::na.omit(comm_ids)))
  pal <- setNames(grDevices::hcl.colors(length(unique_comms), palette = "Set2"),
                  paste0("C", unique_comms))

  nodes <- data.frame(
    id = vnames, label = vnames,
    title = paste0("<b>", vnames, "</b><br>Community: ",
                   ifelse(is.na(comm_ids), "—", paste0("C", comm_ids)),
                   "<br>Degree: ", deg),
    group = ifelse(is.na(comm_ids), "Unclassified", paste0("C", comm_ids)),
    value = as.numeric(10 + log1p(deg) * 5),
    color = ifelse(is.na(comm_ids), "#cccccc", pal[paste0("C", comm_ids)]),
    stringsAsFactors = FALSE)

  el <- igraph::as_edgelist(g); w <- igraph::E(g)$weight
  wn <- (w - min(w)) / (max(w) - min(w) + 1e-9)
  edges <- data.frame(from = el[, 1], to = el[, 2], value = wn * 3 + 0.3,
                      color = sprintf("rgba(150,150,150,%.2f)", 0.15 + wn * 0.5),
                      title = paste0("AS: ", round(w, 5)), stringsAsFactors = FALSE)

  visNetwork(nodes, edges, width = "100%", height = "100%") |>
    visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
               selectedBy = list(variable = "group", main = "Filter by community")) |>
    visPhysics(solver = "forceAtlas2Based",
               forceAtlas2Based = list(gravitationalConstant = -300, springLength = 200),
               stabilization = list(enabled = TRUE, iterations = 300, fit = TRUE)) |>
    visNodes(shape = "dot", scaling = list(min = 8, max = 40)) |>
    visInteraction(navigationButtons = TRUE, tooltipDelay = 100)
}

# ==============================================================================
# UI
# ==============================================================================
ui <- page_sidebar(
  window_title = "themescopeR",
  title = tags$span(
    style = "display:flex; align-items:center; gap:0.7rem;",
    tags$img(src = "themescope_logo.svg", height = "34px",
             style = "border-radius:50%; background:#fff;"),
    tags$span("themescopeR", style = "font-weight:600; letter-spacing:0.04em;")),
  theme = bs_theme(bootswatch = "flatly", primary = "#1D9E75", secondary = "#243b55"),

  sidebar = sidebar(
    width = 320,
    card(
      card_header("Data Input"),
      navset_tab(
        id = "input_tab",
        nav_panel("Upload",
          fileInput("file_upload", label = NULL,
                    accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".RData", ".rda", ".zip"),
                    placeholder = "Raw documents or a tokens file", buttonLabel = "Browse"),
          helpText("Raw documents (", tags$code("doc_id"), ", ", tags$code("text"),
                   " in .csv/.tsv/.xlsx/.RData/.txt/.zip) ", tags$em("or"),
                   " a pre-annotated tokens file (", tags$code("doc_id"), ", ",
                   tags$code("sentence_id"), ", ", tags$code("lemma"), ", ", tags$code("upos"), ").")
        ),
        nav_panel("Demo Data",
          p(icon("database"), " Reddit — Climate Change 2025", class = "fw-semibold mt-1"),
          tags$ul(class = "small text-muted ps-3",
                  tags$li(paste0(format(.demo_ndocs, big.mark = ","), " documents")),
                  tags$li("Raw text (annotated on the fly)"),
                  tags$li("Source: Kaggle (public)")),
          actionButton("load_demo", tagList(icon("database"), " Load demo dataset"),
                       class = "btn-outline-primary w-100"),
          uiOutput("demo_loaded_badge"))
      )
    ),
    card(
      card_header("Preprocessing"),
      selectInput("language", "Annotation language (udpipe)",
                  choices = .languages, selected = "english"),
      helpText("Models are downloaded once and cached locally.", class = "small text-muted")
    ),
    card(
      card_header("Analysis Parameters"),
      numericInput("vocab_size", "Vocabulary size", value = 1500, min = 50, max = 5000, step = 50),
      checkboxGroupInput("pos_filter", "POS filter",
                         choices = c("NOUN", "ADJ", "PROPN", "VERB"),
                         selected = c("NOUN", "ADJ", "PROPN"), inline = TRUE),
      sliderInput("threshold_percentile", "AS threshold percentile",
                  min = 0.80, max = 0.99, value = 0.98, step = 0.01),
      selectInput("community_algorithm", "Community detection",
                  choices = c("Walktrap" = "walktrap", "Louvain" = "louvain"),
                  selected = "walktrap"),
      conditionalPanel("input.community_algorithm === 'walktrap'",
        numericInput("walktrap_steps", "Walktrap steps", value = 4, min = 2, max = 10)),
      numericInput("min_community_size", "Min community size", value = 10, min = 2, max = 50),
      numericInput("seed", "Random seed", value = 1, min = 0, step = 1),
      hr(),
      tags$p(tags$strong("Concreteness lexicon: "),
             tags$span(class = "badge bg-success", "Brysbaert (bundled)"), class = "mb-1"),
      fileInput("lexicon_upload", "Override lexicon (optional)",
                accept = c(".csv"), placeholder = "Use bundled Brysbaert", buttonLabel = "Browse")
    ),
    actionButton("run_analysis", tagList(icon("play"), " Run Analysis"),
                 class = "btn-primary w-100 mt-2"),
    uiOutput("analysis_status_badge")
  ),

  navset_card_tab(
    id = "main_tabs", selected = "info",
    nav_panel(value = "info", title = tagList(icon("circle-info"), " Info"),
      div(class = "container-fluid py-3 px-3", style = "max-width: 950px;",
        tags$h3("themescopeR", style = "color:#243b55;"),
        tags$p(class = "text-muted",
               "Map social representations in digital discourse from raw text."),
        tags$ol(
          tags$li(tags$strong("Load data"), " — upload raw documents (or a tokens file), or click ",
                  tags$strong("Demo Data"), "."),
          tags$li(tags$strong("Choose a language"), " for udpipe annotation (cached after first use)."),
          tags$li(tags$strong("Set parameters"), " and click ", tags$strong("Run Analysis"), "."),
          tags$li(tags$strong("Explore"), " the Map, Network, Communities and Top Terms tabs.")),
        tags$hr(),
        tags$p(class = "small text-muted",
               "All computation uses the package functions read_collection(), ",
               "preprocess_texts(), themescope() and top_terms(); the GUI reproduces ",
               "the console results exactly. Concreteness: Brysbaert et al. (2014).")
      )
    ),
    nav_panel(title = tagList(icon("chart-bar"), " Summary"),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Network Statistics"),
             card_body(withSpinner(tableOutput("network_stats_table"), type = 6, color = "#1D9E75"))),
        card(card_header("Parameters"), card_body(tableOutput("params_table"))))
    ),
    nav_panel(title = tagList(icon("map"), " Map"),
      card(full_screen = TRUE, card_header("ThemeScope Representational Map"),
        card_body(uiOutput("map_placeholder"),
                  withSpinner(plotlyOutput("map_plot", height = "560px"), type = 6, color = "#1D9E75")))
    ),
    nav_panel(title = tagList(icon("layer-group"), " Communities"),
      card(card_header("Community Statistics"),
        card_body(withSpinner(DTOutput("communities_table"), type = 6, color = "#1D9E75")))
    ),
    nav_panel(title = tagList(icon("list-ol"), " Top Terms"),
      card(card_header(class = "d-flex justify-content-between align-items-center",
                       "Top Terms by Community",
                       numericInput("top_n_terms", "Top N", value = 10, min = 3, max = 30, width = "100px")),
        card_body(withSpinner(plotlyOutput("topterms_plot", height = "560px"), type = 6, color = "#1D9E75")))
    ),
    nav_panel(title = tagList(icon("circle-nodes"), " Network"),
      card(full_screen = TRUE,
        card_header(class = "d-flex justify-content-between align-items-center",
          "Co-occurrence Network",
          div(class = "form-check form-switch mb-0",
              tags$input(class = "form-check-input", type = "checkbox", id = "net_hide_unclassified", role = "switch"),
              tags$label(class = "form-check-label small", `for` = "net_hide_unclassified", "Only classified"))),
        card_body(withSpinner(visNetworkOutput("network_plot", height = "600px"), type = 6, color = "#1D9E75")))
    )
  )
)

# ==============================================================================
# Server
# ==============================================================================
server <- function(input, output, session) {

  collection  <- reactiveVal(NULL)   # raw documents (doc_id, text, ...)
  tokens_data <- reactiveVal(NULL)   # annotated tokens
  annot_key   <- reactiveVal(NULL)   # signature of the annotation that produced tokens_data
  result_obj  <- reactiveVal(NULL)
  data_source <- reactiveVal(NULL)

  # ---- Demo ----
  observeEvent(input$load_demo, {
    if (is.null(.demo_coll)) {
      showNotification("Demo data not found in the package.", type = "error"); return()
    }
    collection(.demo_coll); tokens_data(NULL); annot_key(NULL)
    result_obj(NULL); data_source("demo")
    showNotification(sprintf("Demo loaded: %s raw documents.",
                             format(.demo_ndocs, big.mark = ",")), type = "message")
  })
  output$demo_loaded_badge <- renderUI({
    if (identical(data_source(), "demo"))
      tags$span(class = "badge bg-success mt-2", icon("check"), " Demo loaded")
  })

  # ---- Upload (raw OR pre-annotated tokens) ----
  observeEvent(input$file_upload, {
    req(input$file_upload)
    tryCatch({
      path <- input$file_upload$datapath
      coll <- tryCatch(read_collection(path), error = function(e) NULL)
      if (!is.null(coll) && "text" %in% names(coll)) {
        collection(coll); tokens_data(NULL); annot_key(NULL)
        showNotification(sprintf("Loaded %d raw documents.", nrow(coll)), type = "message")
      } else {
        sep <- if (tolower(tools::file_ext(input$file_upload$name)) == "tsv") "\t" else ","
        df <- utils::read.csv(path, sep = sep, stringsAsFactors = FALSE, check.names = FALSE)
        if (!"lemma" %in% names(df) && "token" %in% names(df)) df$lemma <- df$token
        validate_tokens_df(df)
        for (c0 in intersect(c("doc_id", "sentence_id", "lemma", "upos"), names(df)))
          df[[c0]] <- as.character(df[[c0]])
        collection(NULL); tokens_data(df); annot_key("uploaded-tokens")
        showNotification(sprintf("Loaded pre-annotated tokens: %d rows.", nrow(df)), type = "message")
      }
      result_obj(NULL); data_source("upload")
    }, error = function(e) {
      showNotification(paste("Error reading file:", conditionMessage(e)), type = "error", duration = 8)
    })
  })

  # ---- Concreteness lexicon (bundled default, optional override) ----
  lexicon_data <- reactive({
    if (is.null(input$lexicon_upload)) return(themescopeR::brysbaert)
    tryCatch({
      df <- utils::read.csv(input$lexicon_upload$datapath, stringsAsFactors = FALSE)
      if (!all(c("word", "conc.m") %in% names(df))) {
        showNotification("Lexicon needs columns 'word' and 'conc.m'. Using bundled Brysbaert.",
                         type = "warning"); return(themescopeR::brysbaert)
      }
      df
    }, error = function(e) themescopeR::brysbaert)
  })

  # ---- Run analysis (calls the exported functions) ----
  observeEvent(input$run_analysis, {
    if (is.null(collection()) && is.null(tokens_data())) {
      showNotification("No data loaded. Upload a file or load the demo.", type = "warning"); return()
    }
    if (length(input$pos_filter) == 0) {
      showNotification("Select at least one POS tag.", type = "warning"); return()
    }

    withProgress(message = "Running themescopeR ...", value = 0, {
      tryCatch({
        # --- annotate raw text if needed (cached per collection + language) ---
        if (!is.null(collection())) {
          key <- paste0(data_source(), "-", nrow(collection()), "-", input$language)
          if (is.null(tokens_data()) || !identical(annot_key(), key)) {
            setProgress(0.1, detail = paste0("Annotating with udpipe (", input$language, ") ..."))
            toks <- preprocess_texts(collection(), model = input$language, verbose = FALSE)
            tokens_data(toks); annot_key(key)
          }
        }
        req(tokens_data())

        setProgress(0.7, detail = "Building network & metrics ...")
        res <- themescope(
          tokens_data(),
          concreteness_lexicon = lexicon_data(),
          vocab_size           = input$vocab_size,
          pos_filter           = input$pos_filter,
          threshold_percentile = input$threshold_percentile,
          community_algorithm  = input$community_algorithm,
          walktrap_steps       = input$walktrap_steps,
          min_community_size   = input$min_community_size,
          seed                 = input$seed,
          verbose              = FALSE
        )
        result_obj(res)
        setProgress(1, detail = "Done.")
        showNotification(sprintf("Analysis complete: %d communities (%d nodes, %d edges).",
                                 length(res$communities), igraph::vcount(res$graph),
                                 igraph::ecount(res$graph)), type = "message")
        nav_select("main_tabs", "Map")
      }, error = function(e) {
        showNotification(paste("Analysis failed:", conditionMessage(e)), type = "error", duration = 10)
      })
    })
  })

  output$analysis_status_badge <- renderUI({
    res <- result_obj()
    if (is.null(res)) tags$span(class = "badge bg-secondary mt-2 d-block text-center", "No analysis yet")
    else tags$span(class = "badge bg-success mt-2 d-block text-center", icon("check"),
                   sprintf(" %d communities", length(res$communities)))
  })

  # ---- Outputs ----
  output$network_stats_table <- renderTable({
    req(result_obj()); gs <- result_obj()$network_stats$global_stats
    data.frame(Statistic = c("Nodes", "Edges", "Mean degree", "Modularity", "Communities"),
               Value = c(gs$n_nodes, gs$n_edges, round(gs$mean_degree, 3),
                         round(gs$modularity, 4), gs$n_communities))
  }, striped = TRUE, bordered = TRUE)

  output$params_table <- renderTable({
    req(result_obj()); p <- result_obj()$params
    data.frame(Parameter = c("Vocabulary size", "POS filter", "AS threshold",
                             "Algorithm", "Min community size", "Seed"),
               Value = c(as.character(p$vocab_size), paste(p$pos_filter, collapse = ", "),
                         as.character(p$threshold_percentile), p$community_algorithm,
                         as.character(p$min_community_size),
                         if (is.null(p$seed)) "—" else as.character(p$seed)))
  }, striped = TRUE, bordered = TRUE)

  output$map_placeholder <- renderUI({
    if (is.null(result_obj()))
      div(class = "text-muted text-center py-5", "Run the analysis to view the map.")
  })
  output$map_plot <- renderPlotly({ req(result_obj()); build_map_plotly(result_obj()) })

  output$communities_table <- renderDT({
    req(result_obj())
    df <- as.data.frame(result_obj())
    df$psi <- round(df$psi, 4); df$cs <- round(df$cs, 3)
    df$psi_z <- round(df$psi_z, 3); df$cs_z <- round(df$cs_z, 3)
    datatable(df, rownames = FALSE, selection = "none",
              colnames = c("ID", "Label", "Size", "PSI", "CS", "PSI (z)", "CS (z)", "Quadrant"),
              options = list(dom = "tp", pageLength = 20, scrollX = TRUE)) |>
      formatStyle("quadrant", backgroundColor = styleEqual(
        c("Stable Core", "Ideological Core", "Emerging Practices", "Latent Representations"),
        c("#d6eaf8", "#fadbd8", "#d5f5e3", "#e8daef")))
  })

  output$topterms_plot <- renderPlotly({
    req(result_obj())
    tt <- top_terms(result_obj(), n = input$top_n_terms, by = "relevance")
    build_topterms_plotly(tt)
  })

  output$network_plot <- renderVisNetwork({
    req(result_obj())
    build_network_visnetwork(result_obj(), hide_unclassified = isTRUE(input$net_hide_unclassified))
  })
}

shinyApp(ui = ui, server = server)
