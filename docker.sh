source .env

tugboat create \
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