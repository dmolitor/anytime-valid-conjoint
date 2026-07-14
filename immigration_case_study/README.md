# Immigration-Preferences Case Study

A real-data case study applying the anytime-valid AMCE method from the main
paper (see `../README.md`) to the `immigrationconjoint` dataset distributed
with the R package [cjoint](https://cran.r-project.org/package=cjoint) (1,396
respondents; 5 choice tasks each, between 2 hypothetical immigrant profiles),
originally collected for Hainmueller, Hopkins, and Yamamoto (2014) -- the
paper that introduced the AMCE estimator itself.

## Reproducing

From the repository root:

```
bash immigration_case_study/run_all.sh
```

This will:

1. Run `code/01_prepare_data.R`, which loads `cjoint::immigrationconjoint`
   (already long-format: one row per profile shown) and selects the columns
   used as regressors, writing `data/immigration_long.csv`.
2. Run `code/02_sequential_analysis.R`, which:
   - Sources the anytime-valid math (`t_radius`, `log_G_t`, `p_G_t`,
     `optimal_g`, `av_tidy`) from `../code/cj.R`, so this case study always
     uses the same formulas as the rest of the paper.
   - Restores the attributes' original factor level order from `cjoint`
     (lost when they round-trip through CSV), so reference/baseline levels
     match Hainmueller, Hopkins & Yamamoto (2014)'s own convention (e.g.
     India, janitor, no formal schooling) rather than whichever level
     happens to sort first alphabetically.
   - Fits the final model on all respondents (9 attributes, ~41 non-baseline
     dummy coefficients) and tidies it two ways: conventional fixed-$n$
     (`broom::tidy`) and anytime-valid (`av_tidy`), saved to
     `data/final_comparison.csv`. All ~41 coefficients are kept -- nothing
     is curated or dropped here.
   - Refits the model as respondents accumulate one at a time (`G = 5` up
     to the full sample) to trace out each AMCE's confidence sequence and
     e-value over time, saved to `data/sequential_av_estimates.csv`.
   - Saves run metadata (total clusters, $\alpha$, and the pre-registered
     $g^\star$) to `data/meta.csv`.
3. Render `report.Rmd` to `report.pdf` via `rmarkdown::render()`.

## Files

```
immigration_case_study/
├── code/
│   ├── 01_prepare_data.R         # cjoint::immigrationconjoint -> data/immigration_long.csv
│   └── 02_sequential_analysis.R  # fits + AV math -> data/*.csv
├── data/                         # generated; not hand-edited
├── report.Rmd                    # the case study writeup
├── report.pdf                    # rendered report (generated)
├── run_all.sh
└── README.md
```

(figures are embedded directly in report.pdf)

## Notes on interpretation

- Respondents are processed in the order `cjoint::immigrationconjoint`
  stores them (`resp_seq`, a dense re-indexing of `CaseID`, which is not
  itself contiguous) to illustrate sequential monitoring; the dataset
  carries no survey timestamp, so this is a stand-in for true arrival
  order, not a claim about when respondents actually answered.
- $g^\star$ is optimized once for the planned final number of clusters and
  held fixed while "replaying" the data sequentially -- re-optimizing $g$
  at each step using data-dependent information would invalidate the
  anytime-valid guarantee.
- All 9 attributes are multi-level categorical factors, so the full model
  has ~41 non-baseline coefficients rather than the camera study's 10
  simple binary ones. `data/final_comparison.csv` and
  `data/sequential_av_estimates.csv` contain every coefficient; the
  report's two "evolving over time" figures curate exactly one
  representative level per attribute (the level with the largest
  |t-statistic| in the final full-sample fit) for readability -- see the
  Method section of `report.pdf` for the exact rule and selection.
