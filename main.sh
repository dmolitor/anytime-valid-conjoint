#!/usr/bin/env bash

set -euo pipefail

LOG_FILE="log.txt"

# Send all stdout/stderr to both terminal and log file
exec > >(tee "$LOG_FILE") 2>&1

start_time=$(date +%s)

echo
echo "--------------------------------------------------------------"
echo "Timestamp started: $(date "+%Y-%m-%d %H:%M:%S %Z")"

# Replicate all figures from the main text
echo
echo "---------------------------- code/figure_1.R -----------------"
Rscript code/figure_1.R
echo "✔ Done!"

# Replicate simulations for Figures 2, 3/8, 4, 7, and 9

# ----------------------------------------------------------------------------------------------------

## NOTE: This part is what takes BY FAR the longest (hours of compute).
## If you uncomment the lines below, the simulation results will fully replicate.
## However, we recommend just using the saved intermediate files and this will run much faster.

# echo
# echo "---------------------------- code/figure_2_simulations.R -----"
# Rscript code/figure_2_simulations.R
# echo "✔ Done!"

# echo
# echo "---------------------------- code/figure_3_8_simulations.R ---"
# Rscript code/figure_3_8_simulations.R
# echo "✔ Done!"

# echo
# echo "---------------------------- code/figure_4_simulations.R -----"
# Rscript code/figure_4_simulations.R
# echo "✔ Done!"

# echo
# echo "---------------------------- code/figure_7_simulations.R -----"
# Rscript code/figure_7_simulations.R
# echo "✔ Done!"

# echo
# echo "---------------------------- code/figure_9_simulations.R -----"
# Rscript code/figure_9_simulations.R
# echo "✔ Done!"

# ----------------------------------------------------------------------------------------------------

# Replicate all remaining figures
echo
echo "---------------------------- code/figure_2.R -----------------"
Rscript code/figure_2.R
echo "✔ Done!"

echo
echo "---------------------------- code/figure_3_8.R ---------------"
Rscript code/figure_3_8.R
echo "✔ Done!"

echo
echo "---------------------------- code/figure_4.R -----------------"
Rscript code/figure_4.R
echo "✔ Done!"

echo
echo "---------------------------- code/figure_5_6.R ---------------"
Rscript code/figure_5_6_clean.R
Rscript code/figure_5_6.R
echo "✔ Done!"

echo
echo "---------------------------- code/figure_7.R -----------------"
Rscript code/figure_7.R
echo "✔ Done!"

echo
echo "---------------------------- code/figure_9.R -----------------"
Rscript code/figure_9.R
echo "✔ Done!"

end_time=$(date +%s)
runtime_minutes=$(awk "BEGIN {printf \"%.2f\", (${end_time} - ${start_time}) / 60}")

echo
echo "--------------------------------------------------------------"
echo "Timestamp results finished: $(date "+%Y-%m-%d %H:%M:%S %Z")"

echo
echo "--------------------------------------------------------------"
echo "Total runtime (minutes): ${runtime_minutes}"