suppressPackageStartupMessages({
  library(ggplot2)
  library(here)
})

paper_colors <- list(
  av = "#2F5597",
  conventional = "#6B6B6B",
  accent = "#C44E52",
  reference = "#4D4D4D",
  grid = "#E6E6E6"
)

method_palette <- c(
  "Anytime-valid" = paper_colors$av,
  "Conventional" = paper_colors$conventional,
  "Anytime-valid (CS)" = paper_colors$av,
  "Conventional (CI)" = paper_colors$conventional
)

cluster_cap_palette_values <- c("#2F5597", "#2CA25F", "#E17C05", "#8E63A9")

cluster_cap_palette <- function(levels) {
  stats::setNames(
    rep(cluster_cap_palette_values, length.out = length(levels)),
    levels
  )
}

conjoint_theme <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = paper_colors$grid, linewidth = 0.25),
      axis.title = element_text(color = "grey20"),
      axis.text = element_text(color = "grey25"),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0),
      plot.subtitle = element_text(color = "grey30", hjust = 0)
    )
}

paper_font_family <- function() {
  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    return("")
  }
  available_fonts <- unique(systemfonts::system_fonts()$family)
  for (candidate in c("Verdana", "Arial")) {
    match <- available_fonts[
      tolower(available_fonts) == tolower(candidate)
    ]
    if (length(match) > 0) {
      return(match[[1]])
    }
  }
  ""
}

save_paper_figure <- function(filename, plot, width, height, dpi = 500) {
  font_family <- paper_font_family()
  # Apply the font to annotation and label layers.
  update_geom_defaults("text", list(family = font_family))
  update_geom_defaults("label", list(family = font_family))
  # Apply the font to axes, legends, titles, facets, and other theme text.
  font_theme <- theme(
    text = element_text(family = font_family)
  )
  if (inherits(plot, "patchwork")) {
    plot <- plot & font_theme
  } else {
    plot <- plot + font_theme
  }
  ggsave(
    here("figures", filename),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
}
