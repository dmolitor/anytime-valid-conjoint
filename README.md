# Anytime-Valid Inference in Conjoint Experiments

<!-- badges: start -->
[![Launch RStudio Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/dmolitor/anytime-valid-conjoint/main?urlpath=rstudio)
<!-- badges: end -->

## Code and data description

All empirical results in this paper are simulation-based. As a result there is no
raw data; all intermediate data outputs will be stored in the `/data` directory.
Corresponding code can be found in the `code/` directory. All figures will be stored
in the `figures/` directory.

## Computational requirements (and versions used)

So as to not pollute the global environment, we recommend replicating this project
within an R project using renv. This way all dependencies will be installed in a
separate local environment.

- R (4.5.3)
- R packages:
    - avlm (GitHub: dmolitor/avlm@dev)
    - cjpowR (GitHub: m-freitag/cjpowR)
    - broom (1.0.12)
    - dplyr (1.2.1)
    - fixest (0.14.0)
    - fst (0.9.8)
    - future (1.70.0)
    - future.apply (1.20.2)
    - ggplot2 (4.0.2)
    - ggtext (0.1.2)
    - glue (1.8.0)
    - grid (4.5.3)
    - gridExtra (2.3)
    - here (1.0.2)
    - patchwork (1.3.2)
    - progressr (0.19.0)
    - R6 (2.6.1)
    - readr (2.2.0)
    - scales (1.4.0)
    - tidyr (1.3.2)
    - viridis (0.6.5)

### Hardware specification

All figures were generated on a Macbook Pro running macOS Sequoia (15.7.4) with
36 GB of RAM and 14 CPU cores. Many of the computationally expensive simulations
utilize these resources via parallelization. The total RAM needed for the analysis 
should be < 28GB (and could be significantly less if your machine has few cores).
Required disk space for all replication files (including data) is < 50 MB. The computing
time on the Macbook Pro took ~ 437 minutes (7.3 hours).
> [!NOTE]  
> The VAST majority of compute time is required by the simulation scripts. These scripts are 
> the ones ending with `code/{...}_simulations.R`. All figures can be replicated without 
> re-running the simulations because we have stored the intermediate simulation results in the
> `data/` directory. In it's current form, `main.sh` has commented
> out the simulation scripts and will replicate figures from the intermediate results
> in a fraction of the time (~ 1 minute). If you would _really_ like to re-run the
> simulations, simply uncomment the corresponding lines in `main.sh` and it will
> replicate everything, including the time-intensive simulations.

## Replicating figures

To replicate figures, execute the `main.sh` script with Bash:
```bash
bash ./main.sh
```

## Table of contents
```
.
├── code
│   ├── cj.R                       # Utility functions used across scripts
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
├── data.                          # Stores all generated intermediate data results
├── figures                        # Stores all figures
├── log.txt                        # A log of the full replication run
├── main.sh                        # Bash script to execute the full replication pipeline
└── README.md
```