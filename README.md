# Anytime-Valid Inference in Conjoint Experiments

<!-- badges: start -->
[![Launch RStudio Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/dmolitor/anytime-valid-conjoint/main?urlpath=rstudio)
<!-- badges: end -->

Replication materials for [Anytime-Valid Inference in Conjoint Experiments (Molitor, Gosciak, and Lindon; 2026).](https://www.dmolitor.com/blog/posts/conjoint_analysis/)

## Code and data description

All empirical results in this paper are simulation-based. As a result there is no
raw data; all intermediate data outputs will be stored in the `/data` directory.
Corresponding code can be found in the `code/` directory. All figures will be stored
in the `figures/` directory.

## Replicating figures - Binder

By far the easiest way to replicate the paper figures and interact with the data is to click on the
[Binder badge](https://mybinder.org/v2/gh/dmolitor/anytime-valid-conjoint/main?urlpath=rstudio)
in the header of this document. This will bring you to an RStudio instance with all necessary data
and packages installed. Then replicate all figures by executing
```
bash main.sh
```
in the terminal.

## Replicating figures - local

### Install packages (with pinned versions)

To install the required packages with specific versions used in the analysis,
first install `renv` and activate the local project:
```r
install.packages("renv")
renv::activate() # This will ask you to restart your R session; please do so
```

Then, restore all packages from the lockfile:
```r
renv::restore(prompt = FALSE)
```

### Replicating figures

Once packages have been installed, replicate the figures with the following:
```
bash main.sh
```

> [!NOTE]
> Currently the code to replicate all the simulations are commented out
> because they take a long time to run and we have saved the intermediate results from
> those simulations in the `data/` directory. If you _really_ want to
> re-run the simulations, uncomment the relevant lines in `main.sh`.

## Docker image

A Dockerfile is provided for a Docker image with R and all necessary packages installed.
This image is also available on DockerHub at `djmolitor/anytime-valid-conjoint`.

## Table of contents
```
.
├── code
│   ├── cj.R                       # Utility functions used across scripts
│   ├── docker.R                   # Script that creates the Dockerfile and image
│   ├── figure_1.R                 # Plot Figure 1
│   ├── figure_2_simulations.R     # Simulations for Figure 2
│   ├── figure_2.R                 # Plot Figure 1
│   ├── figure_3_6_simulations.R.  # Simulations for Figures 3 and 6
│   ├── figure_3_6.R               # Plot Figures 3 and 6
│   ├── figure_4_simulations.R.    # Simulations for Figure 4
│   ├── figure_4.R                 # Plot Figure 4
│   ├── figure_5_simulations.R.    # Simulations for Figure 5
│   ├── figure_5.R                 # Plot Figure 4
│   └── install_dependencies.R     # Installs all required dependencies
├── data                           # Stores all generated intermediate data results
├── figures                        # Stores all figures
├── main.sh                        # Bash script to execute the full replication pipeline
├── README.md
└── renv.lock                      # Lockfile to install packages from
```