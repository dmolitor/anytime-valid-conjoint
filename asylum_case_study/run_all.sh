#!/usr/bin/env bash
set -euo pipefail

# Always run from the repository root, regardless of where this script is
# invoked from, so here::here() resolves consistently.
cd "$(dirname "${BASH_SOURCE[0]}")/.."

echo "---------------------------- 01_prepare_data.R -----------------"
Rscript asylum_case_study/code/01_prepare_data.R

echo
echo "---------------------------- 02_sequential_analysis.R ----------"
Rscript asylum_case_study/code/02_sequential_analysis.R

echo
echo "---------------------------- report.Rmd -> report.pdf ----------"
Rscript -e 'rmarkdown::render("asylum_case_study/report.Rmd", quiet = TRUE)'

echo
echo "Done. See asylum_case_study/report.pdf"
