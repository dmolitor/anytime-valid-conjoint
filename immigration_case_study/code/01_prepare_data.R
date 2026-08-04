# Loads cjoint::immigrationconjoint, the conjoint dataset from Hainmueller,
# Hopkins & Yamamoto (2014) in which a representative sample of American
# adults chose which of two hypothetical immigrant profiles should be
# admitted to the United States. The data are distributed already in long
# format (one row per profile shown: 1,396 respondents x 5 tasks x 2 profiles
# = 13,960 rows), so no reshaping is needed -- we just select the columns
# used as regressors (dropping ethnocentrism, LangPos, and PriorPos, which
# are respondent-level covariates and pre-computed helper columns not used as
# conjoint attributes) and add a dense respondent-arrival index for the
# sequential analysis. See ?cjoint::immigrationconjoint for the data
# description and its original source.
suppressPackageStartupMessages({
  library(cjoint)
  library(dplyr)
  library(here)
  library(readr)
})

data(immigrationconjoint, package = "cjoint")

# CaseID identifies each respondent (cluster) but is not itself a dense
# 1..1396 sequence (e.g. it skips from 4 to 6), so we add resp_seq: a dense
# arrival-order index built from the order respondents first appear in the
# data (which is already ascending by CaseID). This is a stand-in for true
# arrival order, exactly as in the camera case study -- the raw data carry
# no survey timestamp -- and is used only to drive the sequential-monitoring
# loop in 02_sequential_analysis.R. Clustering itself still uses CaseID.
immigration_long <- immigrationconjoint |>
  mutate(resp_seq = match(CaseID, unique(CaseID))) |>
  select(
    CaseID, resp_seq, contest_no, profile,
    Chosen_Immigrant,
    Gender, Education, `Language Skills`, `Country of Origin`, Job,
    `Job Experience`, `Job Plans`, `Reason for Application`, `Prior Entry`
  )

# Sanity checks: 1,396 respondents, each with exactly 5 tasks x 2 profiles,
# resp_seq forms a dense 1..1396 sequence, and every task has exactly one
# chosen profile out of its 2.
stopifnot(
  n_distinct(immigration_long$CaseID) == 1396,
  nrow(immigration_long) == 1396 * 5 * 2,
  setequal(immigration_long$resp_seq, seq_len(1396)),
  immigration_long |>
    summarize(n_chosen = sum(Chosen_Immigrant), .by = c(CaseID, contest_no)) |>
    pull(n_chosen) |>
    (\(x) all(x == 1))()
)

write_csv(immigration_long, here("immigration_case_study", "data", "immigration_long.csv"))

cat(sprintf(
  "Wrote %d rows for %d respondents (clusters) to data/immigration_long.csv\n",
  nrow(immigration_long), n_distinct(immigration_long$CaseID)
))
