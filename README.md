<!-- README.md is written by hand; edit here directly. -->

<img src="man/figures/logo.svg" align="right" height="130" alt="themescopeR logo" />

# themescopeR

**Social Representation Analysis via Semantic Network Mapping**

`themescopeR` implements the **ThemeScope** framework for detecting and
visualising *social representations* in large-scale digital text corpora. From
raw documents it builds sentence-level word **co-occurrence networks**, detects
thematic **communities**, and locates them on a two-dimensional **strategic
diagram** using two indicators grounded in Social Representation Theory (SRT):

- **PSI** — *Prototypical Salience Index* (anchoring): how structurally embedded
  a theme is in the wider discourse.
- **CS** — *Concreteness Score* (objectification): how far a theme is articulated
  through concrete, imageable vocabulary.

The whole analysis runs from the **R console**; an optional **Shiny** GUI offers
the same pipeline point-and-click and is backed by the very same exported
functions.

---

## Installation

```r
# install.packages("remotes")
remotes::install_github("lucadaniello/themescopeR")
```

The optional GUI additionally needs: `shiny`, `bslib`, `DT`, `plotly`,
`visNetwork`, `shinycssloaders`.

---

## Quick start (console)

```r
library(themescopeR)

# 1. Read a collection of raw documents (csv / tsv / txt / xlsx / RData / zip)
coll <- read_collection(
  system.file("extdata", "sample_collection.csv", package = "themescopeR")
)

# 2. Annotate with udpipe (the language model is downloaded once and cached)
tokens <- preprocess_texts(coll, model = "english")

# 3. Run the full ThemeScope pipeline
result <- themescope(tokens, vocab_size = 1500, seed = 1)

# 4. Inspect and visualise
print(result)
as.data.frame(result)      # community-level table: PSI, CS, z-scores, quadrant
plot(result, type = "map") # the ThemeScope strategic diagram (ggplot2)
plot(result, type = "network")
top_terms(result, n = 10)  # most representative terms per community
```

`themescope()` can also start **directly from raw texts**, annotating
internally:

```r
result <- themescope(coll, model = "english", vocab_size = 1500, seed = 1)
```

### Language models (downloaded once, cached)

Preprocessing uses the updated Universal Dependencies 2.15 `udpipe` models
maintained for [TALL](https://github.com/massimoaria/tall.language.models).

```r
ts_list_models()              # available languages / treebanks
ts_download_model("italian")  # download + cache (no-op if already cached)
themescope_cache_dir()        # where models are stored
```

The cache lives in `tools::R_user_dir("themescopeR", "cache")`; override it with
`options(themescopeR.model_dir = "/your/path")`.

---

## Shiny GUI

```r
run_themescope()
```

Load a file (or the bundled demo), pick the annotation language, set the
parameters, and click **Run Analysis** to explore the interactive map, network,
community table and top terms. The GUI calls `read_collection()`,
`preprocess_texts()`, `themescope()` and `top_terms()` — its results match the
console exactly.

---

## Main functions

The package mirrors the ThemeScope pipeline as a chain of small, pure,
command-line functions. `themescope()` runs them all in order, but each stage is
exported so you can inspect or customise any step. They are presented below in
the order the pipeline calls them.

### 1. Import — `read_collection()`

```r
read_collection(path, text_col = NULL, id_col = NULL)
```

Reads a document collection into a tidy data frame with a `doc_id` and a `text`
column. It auto-detects the format from the file extension and supports a
**single** `.csv`/`.tsv`, `.txt`, `.xlsx`/`.xls`, `.RData`/`.rds`, **or** a
`.zip` containing many such files (unzipped to a temporary directory, read, and
row-bound). Text and id columns are guessed but can be forced with `text_col` /
`id_col`. Malformed input raises a clear, actionable error.

### 2. Linguistic annotation — `preprocess_texts()`

```r
preprocess_texts(collection, model = "english", text_col = "text",
                 doc_id_col = "doc_id", verbose = TRUE)
```

Annotates the raw texts with [`udpipe`](https://bnosac.github.io/udpipe/):
tokenization, lemmatization, sentence segmentation and part-of-speech (UPOS)
tagging. `model` accepts a language name, a `.udpipe` file path, or a loaded
model object; language models come from the updated Universal Dependencies 2.15
treebanks and are downloaded once and cached (see
`ts_list_models()`, `ts_download_model()`, `ts_model_path()`,
`themescope_cache_dir()`). It returns the **full** udpipe annotation
(one row per token, with `doc_id`, `sentence_id`, `token`, `lemma`, `upos`, …),
which is the input every downstream function expects.

### 3. Vocabulary — `build_vocab()`

```r
build_vocab(words_df, unit = "lemma", vocab_size = 1500,
            pos_filter = c("NOUN", "ADJ", "PROPN"))
```

Filters the annotation to content words (by default nouns, adjectives and proper
nouns), counts them on the chosen `unit` (`"lemma"` or `"token"`), and keeps the
`vocab_size` most frequent terms. Returns a data frame of `term`, `freq` and
`upos` — the node set of the network.

### 4. Co-occurrence & association strength — `build_cooccurrence_matrix()`

```r
build_cooccurrence_matrix(words_df, vocab, unit = "lemma",
                          window = "sentence", normalization = "association")
```

Counts how often each pair of vocabulary terms appears together within the same
`window` (`"sentence"` or `"document"`) and rescales the raw co-occurrence counts
into a similarity matrix via `normalization`. The default `"association"`
(Association Strength, `AS(t,t') = a_tt' / (a_t · a_t')`) is the measure used in
the paper; `"jaccard"`, `"salton"`, `"inclusion"`, `"equivalence"` and
`"frequency"` are also available (`normalize_cooccurrence()`,
`compute_association_strength()` expose them individually). It returns both the
normalised matrix and each term's **presence** `a_t` (sentence/document count),
which PSI needs later.

### 5. Network construction — `build_cooccurrence_network()`

```r
build_cooccurrence_network(as_matrix, threshold_percentile = 0.98)
```

Turns the similarity matrix into a weighted, undirected `igraph` network, keeping
only the strongest edges (those above the `threshold_percentile`) and dropping
isolated nodes. This sparse backbone is what gets partitioned into themes.

### 6. Community detection — `detect_communities()`

```r
detect_communities(graph, algorithm = "walktrap", min_size = 10, seed = NULL)
```

Partitions the network into thematic **communities** (candidate social
representations) with `"walktrap"` (default), `"louvain"` or `"leiden"`.
Communities smaller than `min_size` are dropped; pass a `seed` for reproducible
results. `get_community_subgraphs()` splits the graph into one subgraph per
community.

### 7. The two SRT indicators — `compute_psi()` & `compute_cs()`

```r
compute_psi(graph, communities, presence)
compute_cs(graph, communities, concreteness_lexicon = brysbaert)
```

- **`compute_psi()`** — the **Prototypical Salience Index** (anchoring): how
  structurally salient and frequent a community's terms are within the wider
  discourse, normalised by community density.
- **`compute_cs()`** — the **Concreteness Score** (objectification): the
  association-weighted mean concreteness of a community's terms, using the
  bundled `brysbaert` lexicon (`lexicon_coverage()` / `match_concreteness()`
  report and apply the matching).

Both return one value per community; `themescope()` z-scores them and places each
community in a quadrant (`assign_quadrant()`, `zscore()`).

### 8. Representative terms — `term_relevance()` & `top_terms()`

```r
term_relevance(graph, membership, presence)
top_terms(x, n = 10, by = "relevance")
```

Rank the terms inside each community by `"relevance"` (a salience score) or
`"degree"`, so each theme can be labelled by its most characteristic words.

### 9. Visualisation — `plot_themescope()` & `plot_network()`

```r
plot_themescope(psi, cs, ...)   # the strategic diagram (ggplot2)
plot_network(graph, ...)        # the coloured co-occurrence network
```

`plot_themescope()` draws the z-scored **PSI × CS strategic diagram** with the
four labelled quadrants; `plot_network()` draws the community-coloured network.
Both share `themescope_colours()`, so a community keeps the same colour on the
map and on the network. On a `themescope` object simply call
`plot(result, type = "map" | "network")`; `plot(result, label = "terms")` labels
each community with its top terms.

### 10. Orchestrator & GUI — `themescope()` and `run_themescope()`

`themescope()` runs stages 3–9 (and stage 1–2 if you hand it a raw collection
plus a `model`) and returns a structured `themescope` object with `print()`,
`summary()`, `plot()` and `as.data.frame()` methods. `run_themescope()` launches
the Shiny GUI, which calls these same exported functions.

### Summary table

| Function | Purpose |
|---|---|
| `read_collection()` | Import raw documents (single file or zip of many) → tidy data frame |
| `preprocess_texts()` | udpipe annotation; returns the **full** udpipe data frame |
| `ts_download_model()`, `ts_list_models()`, `ts_model_path()` | Manage cached language models |
| `build_vocab()` | Vocabulary as a data frame (`term`, `freq`, `upos`) |
| `build_cooccurrence_matrix()` | Sentence/document co-occurrence + chosen `normalization` |
| `normalize_cooccurrence()`, `compute_association_strength()` | Similarity measures (association, jaccard, salton, inclusion, equivalence) |
| `build_cooccurrence_network()` | Thresholded weighted network |
| `detect_communities()`, `get_community_subgraphs()` | Walktrap / Louvain / Leiden community detection |
| `compute_psi()`, `compute_cs()` | The two SRT indicators |
| `term_relevance()`, `top_terms()` | Representative terms per community |
| `plot_themescope()`, `plot_network()`, `themescope_colours()` | Map, igraph network, shared palette |
| `themescope()` | End-to-end pipeline (words **or** raw text) → `themescope` object |
| `run_themescope()` | Launch the Shiny GUI |

Key options of `themescope()` / `build_cooccurrence_matrix()`: `unit`
(`"lemma"`/`"token"`), `window` (`"sentence"`/`"document"`), `normalization`
(`"association"`, `"jaccard"`, `"salton"`, `"inclusion"`, `"equivalence"`,
`"frequency"`), and `community_algorithm` (`"walktrap"`, `"louvain"`,
`"leiden"`).

---

## Methodology

For communities in the Association-Strength network
(`AS(t,t') = a_tt' / (a_t * a_t')`, with `a_t` the sentence presence of term
`t`):

| Indicator | Definition | SRT construct |
|---|---|---|
| **PSI** | `Psi(g_i) = ( sum_{t in g_i} a_t * s_t ) / max_j delta(g_j)` | Anchoring |
| **CS** | `Cw(g_i) = ( sum_{(t,t') in E_i} ((c(t)+c(t'))/2) * AS(t,t') ) / ( sum AS(t,t') )` | Objectification |

where `s_t` is the mean Association Strength of `t`'s in-community neighbours,
`delta(g_i) = 2 W_i / (|V_i|(|V_i|-1))` the weighted community density, and
`c(.)` the Brysbaert concreteness rating. Communities are placed on the
z-scored PSI × CS plane:

|  | **High CS** | **Low CS** |
|---|---|---|
| **High PSI** | Stable Core | Ideological Core |
| **Low PSI** | Emerging Practices | Latent Representations |

---

## Data & lexicon

- **Bundled lexicon** `brysbaert`: 39,954 English concreteness norms
  (Brysbaert, Warriner & Kuperman, 2014, *Behavior Research Methods*,
  [doi:10.3758/s13428-013-0403-5](https://doi.org/10.3758/s13428-013-0403-5)).
- **Example data** `inst/extdata/sample_collection.csv`: a 1000-document random
  sample of the publicly available
  [Public Opinion on Climate Change](https://www.kaggle.com/datasets/asaniczka/public-opinion-on-climate-change-updated-daily)
  Reddit dataset on Kaggle.

## Authors

Maria Spano, Michelangelo Misuraca, Luca D'Aniello (maintainer).

## License

MIT © 2026 — see [LICENSE](LICENSE).
