# ==============================================================================
# pub_03_hormuz_portwatch.R — publication copy of 03_hormuz_outbound.R (Figure 1).
# The in-house original is untouched.
#
# Differences vs the in-house version:
#   - data: IMF PortWatch chokepoint database (chokepoint6 = Strait of Hormuz),
#     i.e. the source the manuscript caption actually cites, instead of
#     hormuztracking.com;
#   - fixed window Dec 2025 → latest ("last 70 days from today" no longer
#     covers the February 2026 closure);
#   - closure marker at end-February 2026: PortWatch shows traffic collapsing
#     on 1–2 March (24 → 6 → single digits), not on 15 February;
#   - pub theme: no in-image title/subtitle, no logo, Note/Source line only.
# ==============================================================================

suppressPackageStartupMessages({
  library(lubridate)
  library(jsonlite)
  library(httr)
})
source("pub_theme.R")

DATE_FROM  <- as.Date("2025-12-01")
CLOSURE    <- as.Date("2026-02-28")
CACHE_FILE <- "cache/portwatch_hormuz.rds"
CACHE_AGE_HOURS <- 24
dir.create("cache", showWarnings = FALSE)

PW_URL <- paste0(
  "https://services9.arcgis.com/weJ1QsnbMYJlCHdG/arcgis/rest/services/",
  "Daily_Chokepoints_Data/FeatureServer/0/query"
)

VESSEL_COLS <- c(n_tanker = "Tanker", n_dry_bulk = "Dry bulk",
                 n_general_cargo = "General cargo", n_container = "Container",
                 n_roro = "Ro-ro")
VESSEL_ORDER <- c("Tanker", "Dry bulk", "General cargo", "Container", "Ro-ro")
VESSEL_COLORS <- c(
  "Tanker"        = unname(OI["blue"]),
  "Dry bulk"      = unname(OI["orange"]),
  "General cargo" = unname(OI["green"]),
  "Container"     = unname(OI["purple"]),
  "Ro-ro"         = unname(OI["grey"])
)

# ── Fetch (cached) ────────────────────────────────────────────────────────────
fetch_portwatch_hormuz <- function(date_from) {
  r <- GET(PW_URL, query = list(
    where             = sprintf("portid='chokepoint6' AND date >= TIMESTAMP '%s'",
                                format(date_from, "%Y-%m-%d")),
    outFields         = paste(c(names(VESSEL_COLS), "date", "n_total"), collapse = ","),
    orderByFields     = "date",
    returnGeometry    = "false",
    resultRecordCount = "4000",
    f                 = "json"
  ), timeout(120))
  stop_for_status(r)
  j <- fromJSON(content(r, "text", encoding = "UTF-8"))
  df <- as_tibble(j$features$attributes)
  # date arrives as epoch ms or as "YYYY-MM-DD" text depending on the layer
  if (is.numeric(df$date)) {
    df$date <- as.Date(as.POSIXct(df$date / 1000, origin = "1970-01-01", tz = "UTC"))
  } else {
    df$date <- as.Date(substr(df$date, 1, 10))
  }
  arrange(df, date)
}

if (file.exists(CACHE_FILE) &&
    difftime(Sys.time(), file.mtime(CACHE_FILE), units = "hours") < CACHE_AGE_HOURS) {
  cat(sprintf("Using cached PortWatch data (%s)\n", CACHE_FILE))
  pw <- readRDS(CACHE_FILE)
} else {
  cat("Fetching IMF PortWatch — Strait of Hormuz daily transit calls...\n")
  pw <- fetch_portwatch_hormuz(DATE_FROM)
  saveRDS(pw, CACHE_FILE)
}
cat(sprintf("Rows: %d | %s → %s\n", nrow(pw), min(pw$date), max(pw$date)))

daily_long <- pw |>
  select(date, all_of(names(VESSEL_COLS))) |>
  pivot_longer(-date, names_to = "col", values_to = "n") |>
  mutate(category = factor(VESSEL_COLS[col], levels = rev(VESSEL_ORDER)),
         n = coalesce(as.integer(n), 0L))

totals <- daily_long |> group_by(date) |> summarise(total = sum(n), .groups = "drop")
peak   <- slice_max(totals, total, n = 1, with_ties = FALSE)

# ── Figure 1 ──────────────────────────────────────────────────────────────────
p1 <- ggplot(daily_long, aes(date, n, fill = category)) +
  geom_col(width = 0.9, position = "stack") +
  geom_vline(xintercept = as.numeric(CLOSURE), color = "grey20",
             linetype = "dashed", linewidth = 0.45) +
  annotate("text", x = CLOSURE + 2, y = max(totals$total) * 0.97,
           label = "Closure of the strait\n(end of February 2026)",
           hjust = 0, size = 2.7, color = "grey20", lineheight = 0.95) +
  geom_text(data = peak, aes(x = date, y = total, label = total, fill = NULL),
            vjust = -0.5, size = 2.6, color = "grey25", inherit.aes = FALSE) +
  scale_fill_manual(values = VESSEL_COLORS, breaks = VESSEL_ORDER,
                    guide = guide_legend(nrow = 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12)),
                     breaks = scales::breaks_pretty(n = 5)) +
  labs(x = NULL, y = "Vessels per day",
       caption = pub_note(
         "IMF PortWatch (portwatch.imf.org), daily transit calls, Strait of Hormuz",
         note = "AIS-based counts of outbound and inbound transits by vessel type."
       ))

save_pub(p1, "fig1_hormuz_transits.png", w = 7.2, h = 4.2)

# ── Console summary ───────────────────────────────────────────────────────────
latest <- slice_max(totals, date, n = 1)
cat(sprintf("Peak:   %s (%d vessels)\nLatest: %s (%d vessels)\n",
            format(peak$date, "%b %d"), peak$total,
            format(latest$date, "%b %d"), latest$total))
