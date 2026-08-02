# Example data: `sample_collection.csv` and `demo_annotated.rds`

## `sample_collection.csv`

A random sample of **1000 documents** used for examples, tests, and the Shiny
demo. They are Reddit comments posted across 2025, spanning subreddits such as
`changemyview`, `conspiracy`, `climatechange`, `energy`, `climate`, `Futurology`,
`worldnews`, `politics`, `climateskeptics` and `science`. Each row is one
document with the columns:

| Column | Type | Description |
|---|---|---|
| `doc_id` | character | Unique document identifier |
| `text` | character | Raw document text |
| `date` | character | Posting date (`YYYY-MM-DD`) |
| `subreddit` | character | Source subreddit |
| `score` | integer | Reddit score |

## `demo_annotated.rds`

The same 1000 documents after `preprocess_texts()`, using the English **GUM**
treebank: the complete udpipe annotation (one row per token, 60,512 rows).
Document ids are renumbered `doc_1` to `doc_1000` following the order of
`sample_collection.csv`, so the two files line up when the collection is read with
`read_collection(..., sequential_ids = TRUE)`.

Shipping the annotation is what lets the Shiny demo start from a corpus that is
already tokenised, lemmatised and POS tagged, with no udpipe model to download.
It is rebuilt by `data-raw/make_demo_annotation.R`.

## Source and licence

The documents are a random subsample of the **"Public Opinion on Climate
Change" Reddit dataset** publicly hosted on Kaggle:

<https://www.kaggle.com/datasets/asaniczka/public-opinion-on-climate-change-updated-daily>

The data are freely available for reuse from Kaggle. They are shipped here only
as a small, self-contained example so that `themescopeR` can be tried end to end
without any download.

> The same 1000 documents are also provided (outside the package, for
> development checks) in `.xlsx` and `.RData` formats under the project's
> `sample_collection/` folder, to verify that `read_collection()` produces an
> identical tidy result across all three formats.
