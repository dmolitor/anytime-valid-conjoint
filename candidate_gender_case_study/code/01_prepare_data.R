# Cleans and subsets the Teele, Kalla & Rosenbluth (2018) candidate-gender
# conjoint data (Harvard Dataverse doi:10.7910/DVN/FVCGHC, file
# conjoint_data.tab) into a long-format data set with one row per candidate
# profile shown, restricted to a single clean experimental condition: the
# original 2014 general-population voter sample under the unmodified
# attribute set. See ?data/raw/dataverse_README.txt and
# data/raw/data_dictionary.csv for provenance and column definitions.
suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
})

raw <- read.delim(
  here("candidate_gender_case_study", "data", "raw", "conjoint_data.tab"),
  stringsAsFactors = FALSE
)

# Restrict to the original 2014 voter sample (general population, not
# political elites/legislators) under the unmodified attribute set. The
# legislator/elite sample and the 2017 replication with modified attributes
# are real parts of the original study but are out of scope for this case
# study; see the Discussion in report.Rmd for why they are natural
# extensions rather than being pooled in here.
candidate_long <- raw |>
  filter(sample == "usa voter" & replication == 0)

# resp_order gives each respondent an integer rank in the order their rows
# first appear in the raw file (1, 2, 3, ...). Rows for the same respondent
# are contiguous in the raw file and appear in increasing order of the
# numeric part of responseid, so this is a stand-in for arrival order, used
# only for sequential monitoring below -- exactly as cameras_case_study uses
# the order bayesm::camera stores respondents. The raw data carry no survey
# timestamp, so this is not a claim about when respondents actually
# answered. Clustering itself uses the original `responseid` column.
resp_levels <- unique(candidate_long$responseid)
candidate_long <- candidate_long |>
  mutate(resp_order = match(responseid, resp_levels))

# The five attribute groups are one-hot encoded with no reference category
# included (each group's dummies sum to 1 in every row). We drop exactly one
# reference dummy per group before fitting to avoid collinearity with the
# intercept: orig_0ys (0 years experience), orig_UN_sp (unmarried),
# orig_teach (teacher), orig_0ch (0 children), orig_29 (age 29).
candidate_long <- candidate_long |>
  select(
    responseid, resp_order, contest, winner,
    orig_1ys, orig_3ys, orig_8ys,
    orig_FM_sp, orig_MD_sp,
    orig_law, orig_may, orig_leg,
    orig_1ch, orig_3ch,
    orig_45, orig_65,
    orig_cand_female
  )

# Sanity checks against the numbers verified directly from the raw data:
# 12,450 rows, 2,105 respondents (clusters), 6,225 valid pairs (every
# respondent-contest pair has exactly 2 profiles, one of which won), and
# every one-hot attribute group sums to exactly 1 in every row.
one_hot_groups <- list(
  years    = c("orig_1ys", "orig_3ys", "orig_8ys"),          # + dropped orig_0ys
  spouse   = c("orig_FM_sp", "orig_MD_sp"),                   # + dropped orig_UN_sp
  occ      = c("orig_law", "orig_may", "orig_leg"),           # + dropped orig_teach
  children = c("orig_1ch", "orig_3ch"),                       # + dropped orig_0ch
  age      = c("orig_45", "orig_65")                          # + dropped orig_29
)

raw_groups_full <- list(
  years    = c("orig_0ys", "orig_1ys", "orig_3ys", "orig_8ys"),
  spouse   = c("orig_UN_sp", "orig_FM_sp", "orig_MD_sp"),
  occ      = c("orig_teach", "orig_law", "orig_may", "orig_leg"),
  children = c("orig_0ch", "orig_1ch", "orig_3ch"),
  age      = c("orig_29", "orig_45", "orig_65")
)
raw_sub <- raw |> filter(sample == "usa voter" & replication == 0)
group_sums_ok <- vapply(raw_groups_full, function(cols) {
  all(rowSums(raw_sub[cols]) == 1)
}, logical(1))

pair_check <- candidate_long |>
  summarize(n = n(), n_win = sum(winner), .by = c(responseid, contest))

stopifnot(
  nrow(candidate_long) == 12450,
  n_distinct(candidate_long$responseid) == 2105,
  nrow(pair_check) == 6225,
  all(pair_check$n == 2),
  all(pair_check$n_win == 1),
  all(group_sums_ok),
  max(candidate_long$resp_order) == n_distinct(candidate_long$responseid),
  !anyNA(candidate_long)
)

write_csv(candidate_long, here("candidate_gender_case_study", "data", "candidate_long.csv"))

cat(sprintf(
  "Wrote %d rows for %d respondents (clusters), %d pairs, to data/candidate_long.csv\n",
  nrow(candidate_long), n_distinct(candidate_long$responseid), nrow(pair_check)
))
