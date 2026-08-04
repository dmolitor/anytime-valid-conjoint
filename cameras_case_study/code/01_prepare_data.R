# Reshapes bayesm::camera into a long-format conjoint dataset, one row per
# alternative shown (332 respondents x 16 tasks x 5 alternatives, including
# the outside/no-purchase option). See ?bayesm::camera for the data
# description and its original source (Allenby, Brazell, Howell & Rossi,
# 2014, QME / JLE).
suppressPackageStartupMessages({
  library(bayesm)
  library(dplyr)
  library(here)
  library(readr)
})

data(camera, package = "bayesm")

n_tasks <- 16
n_alts <- 5

camera_long <- bind_rows(lapply(seq_along(camera), function(resp_id) {
  resp <- camera[[resp_id]]
  rows <- as.data.frame(resp$X)
  rows$resp_id <- resp_id
  rows$task_id <- rep(seq_len(n_tasks), each = n_alts)
  rows$alt_id <- rep(seq_len(n_alts), times = n_tasks)
  rows$chosen <- as.integer(rows$alt_id == rep(resp$y, each = n_alts))
  rows
}))

camera_long <- camera_long |>
  select(
    resp_id, task_id, alt_id,
    canon, sony, nikon, panasonic, pixels, zoom, video, swivel, wifi, price,
    chosen
  )

# Sanity checks: every respondent's rows are contiguous and every task has
# exactly one chosen alternative out of its 5.
stopifnot(
  n_distinct(camera_long$resp_id) == length(camera),
  nrow(camera_long) == length(camera) * n_tasks * n_alts,
  camera_long |>
    summarize(n_chosen = sum(chosen), .by = c(resp_id, task_id)) |>
    pull(n_chosen) |>
    (\(x) all(x == 1))()
)

write_csv(camera_long, here("cameras_case_study", "data", "camera_long.csv"))

cat(sprintf(
  "Wrote %d rows for %d respondents (clusters) to data/camera_long.csv\n",
  nrow(camera_long), n_distinct(camera_long$resp_id)
))
