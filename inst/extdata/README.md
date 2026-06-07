# Example data — `sample_collection.csv`

A random sample of **1000 documents** used for examples, tests, and the Shiny
demo. Each row is one document with the columns:

| Column | Type | Description |
|---|---|---|
| `doc_id` | character | Unique document identifier |
| `text` | character | Raw document text |
| `date` | character | Posting date (`YYYY-MM-DD`) |
| `subreddit` | character | Source subreddit |
| `score` | integer | Reddit score |

A random sample of **1000 documents** (Reddit comments posted across 2025,
spanning subreddits such as `changemyview`, `conspiracy`, `climatechange`,
`energy`, `climate`, `Futurology`, `worldnews`, `politics`, `climateskeptics`,
`science`, …).

## Source & licence

The documents are a random subsample of the **"Public Opinion on Climate
Change" Reddit dataset** publicly hosted on Kaggle:

<https://www.kaggle.com/datasets/asaniczka/public-opinion-on-climate-change-updated-daily>

The data are freely available for reuse from Kaggle. They are shipped here only
as a small, self-contained example so that `themescopeR` can be tried end-to-end
without any download.

> The same 1000 documents are also provided (outside the package, for
> development checks) in `.xlsx` and `.RData` formats under the project's
> `sample_collection/` folder, to verify that `read_collection()` produces an
> identical tidy result across all three formats.
