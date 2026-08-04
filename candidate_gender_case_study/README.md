# Candidate-Gender Case Study

A real-data case study applying the anytime-valid AMCE method from the main
paper (see `../README.md`) to the candidate-choice conjoint experiment of
Teele, Kalla, and Rosenbluth (2018), "The Ties That Double Bind: Social
Roles and Women's Underrepresentation in Politics" (Harvard Dataverse
doi:10.7910/DVN/FVCGHC). We use the original 2014 general-population voter
sample (2,105 respondents; up to 3 choice tasks each, between pairs of
hypothetical political candidates), and foreground the coefficient on
candidate gender -- the attribute of central substantive interest for this
paper.

## Reproducing

From the repository root:

```
bash candidate_gender_case_study/run_all.sh
```

This will:

1. Run `code/01_prepare_data.R`, which reads the raw Dataverse file
   (`data/raw/conjoint_data.tab`), restricts to the original 2014
   general-population voter sample under the unmodified attribute set
   (`sample == "usa voter" & replication == 0`), drops one reference dummy
   per one-hot attribute group to avoid collinearity, and writes the result
   to `data/candidate_long.csv` (one row per candidate profile shown).
2. Run `code/02_sequential_analysis.R`, which:
   - Sources the anytime-valid math (`t_radius`, `log_G_t`, `p_G_t`,
     `optimal_g`, `av_tidy`) from `../code/cj.R`, so this case study always
     uses the same formulas as the rest of the paper.
   - Fits the final model on all respondents and tidies it two ways:
     conventional fixed-$n$ (`broom::tidy`) and anytime-valid (`av_tidy`),
     saved to `data/final_comparison.csv`.
   - Refits the model as respondents accumulate one at a time (`G = 5` up
     to the full sample) to trace out each AMCE's confidence sequence and
     e-value over time, saved to `data/sequential_av_estimates.csv`.
   - Saves run metadata (total clusters, $\alpha$, and the pre-registered
     $g^\star$) to `data/meta.csv`.
3. Render `report.Rmd` to `report.pdf` via `rmarkdown::render()`.

## Files

```
candidate_gender_case_study/
├── code/
│   ├── 01_prepare_data.R         # raw Dataverse .tab -> data/candidate_long.csv
│   └── 02_sequential_analysis.R  # fits + AV math -> data/*.csv
├── data/
│   └── raw/                      # conjoint_data.tab, data_dictionary.csv, dataverse_README.txt
├── figures/                      # (figures are embedded directly in report.pdf)
├── report.Rmd                    # the case study writeup
├── report.pdf                    # rendered report (generated)
├── run_all.sh
└── README.md
```

## Notes on interpretation

- Respondents are processed in the order they first appear in the raw
  Dataverse file to illustrate sequential monitoring; the dataset carries
  no survey timestamp, so this is a stand-in for true arrival order, not a
  claim about when respondents actually answered.
- $g^\star$ is optimized once for the planned final number of clusters and
  held fixed while "replaying" the data sequentially -- re-optimizing $g$
  at each step using data-dependent information would invalidate the
  anytime-valid guarantee.
- This case study restricts to the original 2014 general-population voter
  sample under the unmodified attribute set. The political elite/legislator
  sample and the 2017 legislator-only replication with modified attributes
  are real parts of the original study but out of scope here; see the
  Discussion in `report.pdf` for these as natural extensions.
