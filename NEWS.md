# themescopeR 0.1.0

First release.

* The ThemeScope pipeline as a chain of exported functions:
  `read_collection()`, `preprocess_texts()`, `build_vocab()`,
  `build_cooccurrence_matrix()`, `build_cooccurrence_network()`,
  `detect_communities()`, `compute_psi()`, `compute_cs()`, `term_relevance()`,
  `top_terms()`, `plot_themescope()` and `plot_network()`, with `themescope()`
  running them end to end.
* `save_themescope()` and `read_themescope()` write and reopen a `.themescope`
  archive holding the analysis, the annotated corpus, the raw texts, the
  term-level scores and the parameters of the run.
* `read_collection()` imports csv, tsv, txt, xlsx, xls, RData and rda files, or
  a zip of many, and can renumber documents `doc_1`, `doc_2`, ... with
  `sequential_ids = TRUE`.
* Updated Universal Dependencies 2.15 udpipe models are downloaded on demand and
  cached: `ts_list_models()`, `ts_download_model()`, `ts_model_path()`,
  `themescope_cache_dir()`.
* `themescope_app()` launches an optional Shiny interface that calls the same
  exported functions, with a demo corpus that ships already annotated.
* Bundled data: the `brysbaert` concreteness norms, a 1000-document Reddit
  climate sample and its udpipe annotation.
