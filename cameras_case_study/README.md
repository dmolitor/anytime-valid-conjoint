# Digital-Camera Case Study

A real-data case study applying the anytime-valid AMCE method from the main
paper (see `../README.md`) to the `camera` conjoint dataset distributed with
the R package [bayesm](https://cran.r-project.org/package=bayesm) (332
respondents; 16 choice tasks each, among 4 camera profiles plus an outside
option).

## Reproducing

From the repository root:

```
bash cameras_case_study/run_all.sh
```

This will:

1. Run `code/01_prepare_data.R`, which reshapes `bayesm::camera` into
   `data/camera_long.csv` (one row per respondent-task-alternative).
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
cameras_case_study/
├── code/
│   ├── 01_prepare_data.R         # bayesm::camera -> data/camera_long.csv
│   └── 02_sequential_analysis.R  # fits + AV math -> data/*.csv
├── data/                         # generated; not hand-edited
├── figures/                      # (figures are embedded directly in report.pdf)
├── report.Rmd                    # the case study writeup
├── report.pdf                    # rendered report (generated)
├── run_all.sh
└── README.md
```

## Notes on interpretation

- Respondents are processed in the order `bayesm::camera` stores them to
  illustrate sequential monitoring; the dataset carries no survey
  timestamp, so this is a stand-in for true arrival order, not a claim
  about when respondents actually answered.
- $g^\star$ is optimized once for the planned final number of clusters and
  held fixed while "replaying" the data sequentially -- re-optimizing $g$
  at each step using data-dependent information would invalidate the
  anytime-valid guarantee.
