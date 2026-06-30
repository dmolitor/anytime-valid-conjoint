source .env

tugboat create \
  -e data/ \
  -e '!data/figure_3_6_efficiency.fst' \
  -e '!data/figure2.csv' \
  -e '!data/figure4.fst' \
  -e '!data/figure5.csv' \
  -e '!data/.gitignore' \
  -e .binder/ \
  -e .venv/ \
  -e 'figures/*' \
  -e '!figures/.gitignore' \
  -e renv/ \
  -e replication/ \
  -e .Renviron \
  -e .Rprofile \
  -e .env \
  -e pyproject.toml \
  -e uv.lock \
  --no-detect-python

tugboat build \
  -n anytime-valid-conjoint \
  --dh-username "$DOCKER_UNAME" \
  --dh-password "$DOCKER_PWD" \
  --push

tugboat binderize --no-detect-python -b "main"