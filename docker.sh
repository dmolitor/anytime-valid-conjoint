tugboat create \
  -e .binder/ \
  -e .claude/ \
  -e .venv/ \
  -e 'data/*' \
  -e '!data/.gitignore' \
  -e '!data/figure_3_8.fst' \
  -e '!data/figure2.csv' \
  -e '!data/figure4.fst' \
  -e '!data/figure5.csv' \
  -e '!data/figure7.csv' \
  -e '!data/figure9.csv' \
  -e '!data/conjoint_data_2016.csv' \
  -e 'figures/*' \
  -e '!figures/.gitignore' \
  -e renv/ \
  -e replication/ \
  -e .Renviron \
  -e .Rprofile \
  -e .env \
  -e pyproject.toml \
  -e uv.lock \
  -e log.txt \
  --no-detect-python

# Ensure that building the Docker image works
tugboat build -n anytime-valid-conjoint

tugboat binderize --no-detect-python -b "main"