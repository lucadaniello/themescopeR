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

## Core API

| Function | Purpose |
|---|---|
| `read_collection()` | Import raw documents (single file or zip of many) → tidy data frame |
| `preprocess_texts()` | udpipe tokenisation / lemmatisation / POS tagging |
| `ts_download_model()`, `ts_list_models()`, `ts_model_path()` | Manage cached language models |
| `build_cooccurrence_matrix()`, `compute_association_strength()` | Co-occurrence + Association Strength |
| `build_cooccurrence_network()` | Thresholded weighted network |
| `detect_communities()` | Walktrap / Louvain community detection |
| `compute_psi()`, `compute_cs()` | The two SRT indicators |
| `term_relevance()`, `top_terms()` | Representative terms per community |
| `plot_themescope()`, `plot_network()` | Strategic diagram and network plots |
| `themescope()` | End-to-end pipeline (tokens **or** raw text) → `themescope` object |
| `run_themescope()` | Launch the Shiny GUI |

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
