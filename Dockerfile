FROM posit/r-base:4.5.3-noble
COPY renv.lock renv.lock
RUN for d in /usr/local/lib/R/etc /usr/lib/R/etc; do \
      mkdir -p "$d" 2>/dev/null || true; \
      f="$d/Rprofile.site"; \
      { echo "options(renv.config.pak.enabled = TRUE)" >> "$f" 2>/dev/null || true; } \
    done
RUN R -e "install.packages('pak', repos = sprintf('https://r-lib.github.io/p/pak/stable/%s/%s/%s', .Platform[['pkgType']], R.Version()[['os']], R.Version()[['arch']]))"
RUN R -e "dist <- pak::system_r_platform_data()[['distribution']]; rel <- pak::system_r_platform_data()[['release']]; binary_url <- subset(pak::ppm_platforms(), distribution == dist & release == rel)[['binary_url']][1]; cran_binary_url <- if (!is.na(binary_url)) { sprintf('%s/__linux__/%s/latest', pak::ppm_repo_url(), binary_url)  } else { NA }; if (!is.na(cran_binary_url)) { pak::repo_add(CRAN = cran_binary_url) }; pak::pkg_install('renv@1.2.3'); if (!is.na(cran_binary_url)) { lf <- renv::lockfile_modify(repos = c('CRAN' = cran_binary_url)); tryCatch({ renv::lockfile_write(lf, './renv.lock') }, error = function(e) { invisible(NULL) }) }"
RUN R -e "renv::restore()"
COPY . /anytime-valid-conjoint

