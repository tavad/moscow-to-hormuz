# ==============================================================================
# pub_theme.R — shared styling for the publication (RIPE) figure set.
#
# Spec, per RIPE instructions for authors + T&F artwork guidance:
#   - no titles/subtitles inside the image: figure captions live in the
#     manuscript text ("figure captions (as a list)"), the image carries only
#     axes, direct labels and a Note/Source line;
#   - sans-serif (Arial), all text >= ~7 pt at final size;
#   - 300 dpi; colour is free online but print colour costs GBP 300/figure,
#     so nothing may be encoded in hue alone — every series is direct-labelled
#     and the palette (Okabe-Ito) stays distinguishable in grayscale;
#
# The in-house versions (theme_tvyal26 + tvyal_logo) live in the original
# 0*.R scripts and are not touched.
# ==============================================================================

library(ragg)
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})
Sys.setlocale("LC_TIME", "en_US.UTF-8")

PUB_DIR    <- "plots/pub"
PUB_FAMILY <- "Arial"
dir.create(PUB_DIR, recursive = TRUE, showWarnings = FALSE)

# Okabe-Ito colourblind-safe palette (yellow omitted — too light for lines)
OI <- c(blue      = "#0072B2", vermillion = "#D55E00", green  = "#009E73",
        orange    = "#E69F00", purple     = "#CC79A7", skyblue = "#56B4E9",
        black     = "#000000", grey       = "#999999")

theme_pub <- function(base_size = 10) {
  theme_minimal(base_size = base_size, base_family = PUB_FAMILY) +
    theme(
      text               = element_text(colour = "black"),
      axis.text          = element_text(size = rel(0.85), colour = "grey15"),
      axis.title         = element_text(size = rel(0.9)),
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(colour = "grey88", linewidth = 0.3),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      legend.text        = element_text(size = rel(0.8)),
      strip.text         = element_text(face = "bold", size = rel(0.9)),
      plot.caption       = element_text(size = rel(0.72), colour = "grey35",
                                        hjust = 0, margin = margin(t = 8)),
      plot.margin        = margin(8, 10, 6, 6)
    )
}
theme_set(theme_pub())

update_geom_defaults("text",  list(family = PUB_FAMILY))
update_geom_defaults("label", list(family = PUB_FAMILY))
if (requireNamespace("ggrepel", quietly = TRUE)) {
  suppressPackageStartupMessages(library(ggrepel))
  update_geom_defaults("text_repel",  list(family = PUB_FAMILY))
  update_geom_defaults("label_repel", list(family = PUB_FAMILY))
}

# Under-figure note in journal format: "Note: ..." / "Source: ..." lines.
# Hard-wrapped so the caption never overflows the panel; `width` is characters
# per line — lower it for narrow figures.
pub_note <- function(source, note = NULL, width = 105) {
  wrap <- function(x) paste(strwrap(x, width = width), collapse = "\n")
  s <- wrap(paste0("Source: ", source, "."))
  if (is.null(note)) s else paste0(wrap(paste0("Note: ", note)), "\n", s)
}

save_pub <- function(p, filename, w = 7, h = 4.2) {
  path <- file.path(PUB_DIR, filename)
  ggsave(path, p, width = w, height = h, dpi = 300, bg = "white",
         device = ragg::agg_png)
  cat(sprintf("Saved: %s\n", path))
}
