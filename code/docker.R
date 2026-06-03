library(tugboat)

dockerfile <- create(
  FROM = paste0("posit/r-base:", R.version$major, ".", R.version$minor, "-noble"),
  exclude = c(
    ".venv/",
    ".binder/",
    "renv/",
    "replication/",
    ".Rprofile",
    ".Renviron",
    "figures/",
    "!figures/.gitignore",
    "data/",
    "!data/figure_3_6_efficiency.fst",
    "!data/figure2.csv",
    "!data/figure4.fst",
    "!data/figure5.csv",
    "!data/.gitignore"
  ),
  optimize_pak = TRUE
)

build(
  image_name = "anytime-valid-conjoint",
  push = TRUE,
  dh_username = Sys.getenv("DOCKER_UNAME"),
  dh_password = Sys.getenv("DOCKER_PWD")
)

binderize()
