#!/usr/bin/env bash

set -euo pipefail

LOG_FILE="log.txt"

# Send all stdout/stderr to both terminal and log file
exec > >(tee "$LOG_FILE") 2>&1

start_time=$(date +%s)

echo
echo "--------------------------------------------------------------"
echo "Timestamp started: $(date "+%Y-%m-%d %H:%M:%S %Z")"

# Ensure all dependencies are installed
echo
echo "---------------------------- code/install_dependencies.R -----"
Rscript code/install_dependencies.R
echo "✔ Done!"


# Replicate all figures from the main text
echo
echo "---------------------------- code/figure_1.R -----------------"
Rscript code/figure_1.R
echo "✔ Done!"

# Replicate simulations for Figures 2, 3/6, and 5

# ----------------------------------------------------------------------------------------------------

## NOTE: This part is what takes BY FAR the longest (hours of compute).
## If you uncomment the lines below, the simulation results will fully replicate.
## However, we recommend just using the saved intermediate files and this will run much faster.

echo
echo "---------------------------- code/figure_2_simulations.R -----"
Rscript code/figure_2_simulations.R
echo "✔ Done!"

echo
echo "---------------------------- code/figure_3_6_simulations.R ---"
Rscript code/figure_3_6_simulations.R
echo "✔ Done!"

echo
echo "---------------------------- code/figure_5_simulations.R -----"
Rscript code/figure_5_simulations.R
echo "✔ Done!"

# ----------------------------------------------------------------------------------------------------

# Replicate all remaining figures
echo
echo "---------------------------- code/figure_2.R -----------------"
Rscript code/figure_2.R
echo "✔ Done!"

echo
echo "---------------------------- code/figure_3_6.R ---------------"
Rscript code/figure_3_6.R
echo "✔ Done!"

echo
echo "---------------------------- code/figure_4.R -----------------"
Rscript code/figure_4.R
echo "✔ Done!"

echo
echo "---------------------------- code/figure_5.R -----------------"
Rscript code/figure_5.R
echo "✔ Done!"

end_time=$(date +%s)
runtime_minutes=$(awk "BEGIN {printf \"%.2f\", (${end_time} - ${start_time}) / 60}")

echo
echo "--------------------------------------------------------------"
echo "Timestamp results finished: $(date "+%Y-%m-%d %H:%M:%S %Z")"

echo
echo "--------------------------------------------------------------"
echo "Total runtime (minutes): ${runtime_minutes}"