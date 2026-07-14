# Cleans the 2016 wave of the Bansak, Hainmueller & Hangartner asylum-seeker
# conjoint experiment into an analysis-ready long-format dataset (one row per
# respondent-task-profile). See data/raw/conjoint_data_codebook.txt for
# column definitions, and the References section of report.Rmd for the full
# citations and data provenance (Science 2016 study design / wave; Nature
# 2023 replication archive for this specific file).
suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
})

attribute_cols <- c(
  "cconsist", "cgender", "corigin", "cage", "cjob",
  "cvulner", "creason", "creligion", "clang"
)

raw <- read_csv(
  here("asylum_case_study", "data", "raw", "conjoint_data_2016.csv"),
  show_col_types = FALSE
)

n_before <- nrow(raw)
resp_before <- n_distinct(raw$ResponseId)

# 9 respondents (90 rows) have every one of the 9 attribute columns missing
# -- corrupted/incomplete records carrying no usable profile information.
# Drop any row where any attribute column is NA. We keep `pref` (the
# forced-choice outcome used as the AMCE outcome below) and drop `rate` /
# `ratebin`, an alternative rating-scale outcome not used in this case study.
asylum_long <- raw |>
  filter(if_all(all_of(attribute_cols), ~ !is.na(.))) |>
  select(ResponseId, task, prof, pref, all_of(attribute_cols))

# Convert the 9 attribute columns to factors before fitting.
for (col in attribute_cols) {
  asylum_long[[col]] <- factor(asylum_long[[col]])
}

# Arrival-order index: respondents are stored as contiguous 10-row blocks
# (5 tasks x 2 profiles) in the order the file was exported. The raw data
# carry no survey timestamp, so -- exactly as in cameras_case_study -- this
# storage order is used only as a stand-in for arrival order when replaying
# the data sequentially in 02_sequential_analysis.R, not as a claim about
# when respondents actually answered.
asylum_long <- asylum_long |>
  mutate(resp_index = match(ResponseId, unique(ResponseId)))

## ---- Sanity checks ---------------------------------------------------------

stopifnot(
  # Exactly the 9 corrupted respondents (90 rows) were dropped, nothing else.
  n_before - nrow(asylum_long) == 90,
  resp_before - n_distinct(asylum_long$ResponseId) == 9,
  nrow(asylum_long) == 180210,
  n_distinct(asylum_long$ResponseId) == 18021,
  # Every respondent contributes exactly 10 rows (5 tasks x 2 profiles).
  asylum_long |> count(ResponseId) |> pull(n) |> (\(x) all(x == 10))(),
  # Respondents' rows are contiguous, so resp_index is a valid arrival-order
  # relabeling (1..G_total) of ResponseId.
  length(rle(asylum_long$ResponseId)$values) == n_distinct(asylum_long$ResponseId),
  min(asylum_long$resp_index) == 1,
  max(asylum_long$resp_index) == n_distinct(asylum_long$ResponseId),
  # Forced choice: exactly one profile is preferred per respondent-task.
  asylum_long |>
    summarize(n_pref = sum(pref), .by = c(ResponseId, task)) |>
    pull(n_pref) |>
    (\(x) all(x == 1))()
)

write_csv(asylum_long, here("asylum_case_study", "data", "asylum_long.csv"))

cat(sprintf(
  "Dropped %d corrupted rows (%d respondents with all attributes missing).\n",
  n_before - nrow(asylum_long), resp_before - n_distinct(asylum_long$ResponseId)
))
cat(sprintf(
  "Wrote %d rows for %d respondents (clusters) to data/asylum_long.csv\n",
  nrow(asylum_long), n_distinct(asylum_long$ResponseId)
))
