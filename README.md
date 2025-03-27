# Anytime-valid Inference on the AMCE in Conjoint Experiments

What this says ☝️

## Setting up R

This project uses [`renv`](https://rstudio.github.io/renv/index.html) to manage
R dependencies. To sync your local project:
```r
# install.packages("renv")
renv::activate() # It will tell you to restart your R session
renv::restore(prompt = FALSE)
```

## Setting up Python

This project uses [`uv`](https://docs.astral.sh/uv/) to manage Python
dependencies and code. To sync your local project:
```bash
uv python install
uv sync --all-extras --dev
```