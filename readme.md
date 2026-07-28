# Topic Modeling vs. Human Coders: #WhyIStayed / #WhyILeft

Code for the structural topic model (STM) analysis in:

Rodriguez, M.Y. & Storer, H. (2020), "A Computational Social Science Perspective on Qualitative Data
Exploration: Using Topic Models for the Descriptive Analysis of Social Media
Text," *Journal of Technology in Human Services*.
DOI: [10.1080/15228835.2019.1616350](https://www.tandfonline.com/doi/full/10.1080/15228835.2019.1616350)

The study compares structural topic models fit at different numbers of topics
(K) against manual/qualitative coding of tweets tagged `#WhyIStayed` and
`#WhyILeft`.

## Repository contents

| File | Description |
|---|---|
| `STM_github_cleaned.R` | Full analysis script: co-occurrence heatmaps, text preprocessing, STM fitting at multiple K, model diagnostics, topic effect estimates, topic correlations, and word cloud/quote visualizations. |
| `STM_deidentified_data.RData` | De-identified, pre-processed data: `docs` (tokenized/stemmed document-term structure), `vocab` (416 word stems), and `meta` (row id + the three derived hashtag indicator columns). No raw tweet text or account metadata. Lets the `Estimate`/`Evaluate`/`Understand` sections of the script be reproduced without access to the original tweets. |
| `meta_deidentified.csv` | CSV export of the `meta` data frame above, for convenience. |

## Requirements
- R (tested on 4.5.3)
- R packages:
  ```r
  install.packages(c(
    "dplyr", "tm", "ggplot2", "caret", "gplots", "corrplot",
    "RColorBrewer", "stm", "streamR", "tidyverse", "tidytext",
    "stminsights", "stringr", "Rtsne", "rsvd", "geometry", "igraph"
  ))
  ```
## Data

The full script (`Visualize` and `Ingest`/`Prepare` sections) expects three
input files in a local `data/` folder, **none of which are included in this
repository:**

- `data/forSTMfinal.csv` — tweet-level dataset (raw text + metadata) — **not published, see note below**
- `data/whyileft_cooccurence.csv` — qualitative code co-occurrence matrix for `#WhyILeft`
- `data/whyistayed_cooccurence.csv` — qualitative code co-occurrence matrix for `#WhyIStayed`

**Data availability:** The underlying tweet dataset contains first-person
disclosures of domestic violence and is not published in this repository,
even in de-identified form, because verbatim social media text can often be
traced back to its author via search. The co-occurrence CSVs contain only
qualitative code counts (no tweet text) and can be shared freely.

Consistent with standard practice for sharing X (formerly Twitter) data,
and with X's Developer Agreement and Policy — which restricts
redistributing the full text/content of tweets, permitting only tweet IDs
(and/or user IDs) to be shared publicly — we can provide a list of tweet
IDs for the dataset used in this study on request. Requesters will need their 
own X developer access to "rehydrate" the IDs into full tweet text and metadata (e.g. via
[`twarc`](https://github.com/DocNow/twarc) or a similar hydration tool),
subject to X's terms of use at the time of the request. Note that tweets
that have since been deleted or made private will no longer be retrievable
— check X's current developer terms (developer.x.com) for the latest
redistribution policy, as it is subject to change.

Figures and exported tables are written to a local `output/` folder.

## Usage

Three ways to work with this repo, depending on what you need:

1. **Reproduce topic model figures/results without the raw tweets (recommended
   starting point):** load `STM_deidentified_data.RData` into an R session —
   it provides `docs`, `vocab`, and `meta` (hashtag indicators only, no text)
   — then run the `Estimate` / `Evaluate` / `Understand` sections of
   `STM_github_cleaned.R`. Steps that require `meta$Text` directly (the
   `findThoughts` example-tweet quotes and word clouds) can't be reproduced
   from this file alone, since it contains no raw text.
2. **Reproduce the co-occurrence heatmaps:** put the two `*_cooccurence.csv`
   files in `data/` and run the `Visualize` section.
3. **Run the full analysis from scratch, including preprocessing raw tweet
   text (slow — STM fitting and `searchK` diagnostics are computationally
   expensive):** requires access to the original `forSTMfinal.csv` (see Data
   availability above), placed in `data/`, then run `STM_github_cleaned.R`
   top to bottom.

The script is organized into sections matching the analysis workflow:
Visualize (code co-occurrence) → Ingest → Prepare → Estimate (fit STM models
at K = 97, auto-selected K, and K = 65) → Evaluate (`searchK` diagnostics) →
Understand (topic labels, summary plots, effect estimates, topic
correlations, word clouds/quotes) → interactive exploration via
[`stminsights`](https://cran.r-project.org/package=stminsights).

## Citation

If you use this code, please cite the paper above.
