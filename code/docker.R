library(tugboat)

dockerfile <- create(
  FROM = paste0("posit/r-base:", R.version$major, ".", R.version$minor, "-noble"),
  exclude = c(
    "renv/",
    "replication/",
    ".Rprofile",
    ".Renviron",
    "figures/",
    "!figures/.gitignore",
    "data/",
    "!data/figure_3_6_efficiency.fst",
    "!data/figure2.csv",
    "!data/figure4.csv",
    "!data/figure5.csv",
    "!data/.gitignore"
  )
)

build(
  image_name = "anytime-valid-conjoint",
  push = FALSE,
  dh_username = Sys.getenv("DOCKER_UNAME"),
  dh_password = Sys.getenv("DOCKER_PWD")
)

binderize()