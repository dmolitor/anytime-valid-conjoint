if (!require(pak, quietly = TRUE)) {
  # Install pak binary if not already installed
  install.packages("pak", repos = sprintf(
    "https://r-lib.github.io/p/pak/stable/%s/%s/%s",
    .Platform$pkgType,
    R.Version()$os,
    R.Version()$arch
  ))
}

library(pak, quietly = TRUE)

pkg_install(c(
  "dmolitor/avlm@dev",
  "m-freitag/cjpowR",
  "broom@1.0.12",
  "dplyr@1.2.1",
  "fixest@0.14.0",
  "fst@0.9.8",
  "future@1.70.0",
  "future.apply@1.20.2",
  "ggplot2@4.0.2",
  "ggtext@0.1.2",
  "glue@1.8.0",
  "gridExtra@2.3",
  "here@1.0.2",
  "patchwork@1.3.2",
  "progressr@0.19.0",
  "R6@2.6.1",
  "readr@2.2.0",
  "scales@1.4.0",
  "tidyr@1.3.2",
  "viridis@0.6.5"
))
