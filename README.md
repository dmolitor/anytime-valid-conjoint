# Anytime-Valid Inference in Conjoint Experiments

<!-- badges: start -->
[![Launch RStudio Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/dmolitor/anytime-valid-conjoint/main?urlpath=rstudio)
<!-- badges: end -->

Replication materials for [Anytime-Valid Inference in Conjoint Experiments (Molitor, Gosciak, and Lindon; 2026).](https://www.dmolitor.com/blog/posts/conjoint_analysis/)

## Code and data description

All raw data and intermediate data outputs required to replicate the 
empirical results in this paper will be stored in the `/data` directory.
Corresponding code can be found in the `code/` directory. 
All figures will be stored in the `figures/` directory.

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

## Table of contents
```
.
├── code
│   ├── cj.R                       # Utility functions used across scripts
│   ├── docker.R                   # Script that creates the Dockerfile
│   ├── figure_1.R                 # Plot Figure 1
│   ├── figure_2_simulations.R     # Simulations for Figure 2
│   ├── figure_2.R                 # Plot Figure 2
│   ├── figure_3_8_simulations.R   # Simulations for Figures 3 and 8
│   ├── figure_3_8.R               # Plot Figures 3 and 6
│   ├── figure_4_simulations.R     # Simulations for Figure 4
│   ├── figure_4.R                 # Plot Figure 4
│   ├── figure_5_6_clean.R         # Clean data for Figures 5 and 6
│   ├── figure_5_6.R               # Plot Figures 5 and 6
│   ├── figure_7_simulations.R     # Simulations for Figure 7
│   ├── figure_7.R                 # Plot Figure 7
│   ├── figure_9_simulations.R     # Simulations for Figure 9
│   ├── figure_9.R                 # Plot Figure 9
│   ├── figure_style.R             # Set styling for all figures
│   └── install_dependencies.R     # Installs all required dependencies
├── data                           # Stores all generated intermediate data results
├── figures                        # Stores all figures
├── log.txt                        # A log of the full replication run
├── main.sh                        # Bash script to execute the full replication pipeline
└── README.md
```