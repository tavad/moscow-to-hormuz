# ==============================================================================
# pub_04_armenia_cba_timeline.R — publication copy of 04_armenia_cba_timeline.R
# (manuscript Figure 5). The in-house original is untouched.
#
# Differences vs the in-house version:
#   - fixed window July 2025 – June 2026, matching the manuscript caption
#     (the original runs to Sys.Date() and would drift past the caption);
#   - Hormuz closure marker at end-Feb 2026 (PortWatch shows the collapse on
#     1–2 March, not 15 February);
#   - pub theme: no in-image title/subtitle, no logo, no tvyal.com URLs;
#   - own cache file so the in-house script's cache never gets a truncated
#     date range.
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(httr)
  library(lubridate)
})
source("pub_theme.R")

# ── Config ────────────────────────────────────────────────────────────────────
DATE_FROM  <- as.Date("2025-07-01")
DATE_TO    <- as.Date("2026-06-30")   # caption: July 2025 – June 2026
CACHE_FILE <- "cache/pub_04_cba_accounts.rds"
CACHE_AGE_HOURS <- 24
dir.create("cache", showWarnings = FALSE)

CBA_URL <- paste0(
  "https://www.cba.am/en/statistics/",
  "analytical-accounts-of-the-cba-daily/48/export-filtered"
)
HEADERS <- c(
  "User-Agent" = paste0(
    "Mozilla/5.0 (X11; Linux x86_64) ",
    "AppleWebKit/537.36 (KHTML, like Gecko) ",
    "Chrome/123.0.0.0 Safari/537.36"
  )
)

COL_NAMES <- c(
  "date", "NIR", "NDA", "gov_account", "banks_net",
  "repo_total", "repo_main", "repo_long", "repo_fine", "repo_lombard",
  "fx_swap_attract", "deposits", "deposit_auctions", "reverse_repo",
  "fx_swap_alloc", "securities_issued", "other_net",
  "monetary_base", "currency_outside_cba", "corr_dram",
  "corr_fx", "other_banks_dram", "other_banks_fx",
  "other_residents", "other_dram", "other_fx"
)

# ── Fetch with cache (verbatim from original) ─────────────────────────────────
fetch_cba_accounts <- function(date_from, date_to) {
  tmp <- tempfile(fileext = ".xlsx")
  resp <- GET(
    CBA_URL,
    query = list(
      date_from = format(date_from, "%Y-%m-%d"),
      date_to   = format(date_to,   "%Y-%m-%d"),
      period    = "1",
      calc_type = "1",
      format    = "xlsx"
    ),
    add_headers(.headers = HEADERS),
    timeout(60)
  )
  stop_for_status(resp)
  writeBin(content(resp, "raw"), tmp)

  raw <- read_excel(tmp, sheet = 1, col_names = FALSE)
  data_rows <- slice(raw, -(1:2))
  if (ncol(data_rows) < length(COL_NAMES)) {
    for (i in (ncol(data_rows) + 1):length(COL_NAMES)) {
      data_rows[[paste0("pad", i)]] <- NA_character_
    }
  }
  data_rows |>
    set_names(COL_NAMES) |>
    mutate(
      date = suppressWarnings(as.Date(date)),
      across(-date, ~suppressWarnings(as.numeric(.)))
    ) |>
    filter(!is.na(date)) |>
    arrange(date)
}

cache_valid <- function(path, max_age_hours) {
  file.exists(path) &&
    difftime(Sys.time(), file.mtime(path), units = "hours") < max_age_hours
}

if (cache_valid(CACHE_FILE, CACHE_AGE_HOURS)) {
  cat(sprintf("Using cached CBA accounts (%s)\n", CACHE_FILE))
  accounts <- readRDS(CACHE_FILE)
} else {
  cat(sprintf("Fetching CBA analytical accounts %s → %s ...\n", DATE_FROM, DATE_TO))
  accounts <- fetch_cba_accounts(DATE_FROM, DATE_TO)
  saveRDS(accounts, CACHE_FILE)
}
cat(sprintf("Rows: %d | %s → %s\n",
            nrow(accounts), min(accounts$date), max(accounts$date)))

repo_df <- accounts |>
  filter(date >= DATE_FROM, date <= DATE_TO) |>
  transmute(date, repo_bn = repo_total / 1000) |>
  filter(!is.na(repo_bn))

# ── Events ────────────────────────────────────────────────────────────────────
spike_date  <- as.Date("2026-01-14")  # 272bn emergency repo (14–15 Jan)
ack_date    <- as.Date("2026-01-26")  # first official CBA acknowledgment
hormuz_date <- as.Date("2026-02-28")  # closure of the strait (end-Feb 2026)
gdp_date    <- as.Date("2026-04-17")  # 2025 GDP release: 7.2% vs 4.5–5.2 forecast

spike_val <- repo_df |> filter(date == spike_date) |> pull(repo_bn)
cat(sprintf("Repo on %s: %.1f bn (prev business day %.1f bn)\n",
            spike_date, spike_val,
            repo_df |> filter(date < spike_date) |> slice_max(date) |> pull(repo_bn)))

EV_COLS <- c(spike  = unname(OI["vermillion"]),
             ack    = unname(OI["blue"]),
             hormuz = "grey35",
             gdp    = unname(OI["green"]))

event_lab <- function(x, y, label, color) {
  annotate("label", x = x, y = y, label = label, size = 2.5, color = color,
           fill = "white", label.padding = unit(0.22, "lines"),
           fontface = "bold", hjust = 0, lineheight = 0.98)
}

y_top <- max(repo_df$repo_bn)

p14 <- ggplot(repo_df, aes(date, repo_bn)) +
  annotate("rect", xmin = hormuz_date, xmax = max(repo_df$date),
           ymin = -Inf, ymax = Inf, fill = "grey55", alpha = 0.10) +
  geom_vline(xintercept = as.numeric(c(spike_date, ack_date, hormuz_date, gdp_date)),
             linetype = "dashed", linewidth = 0.4,
             color = unname(EV_COLS[c("spike","ack","hormuz","gdp")])) +
  geom_step(color = "grey15", linewidth = 0.7) +
  annotate("segment", x = spike_date, xend = ack_date,
           y = y_top * 0.56, yend = y_top * 0.56, color = "grey30",
           linewidth = 0.45,
           arrow = arrow(ends = "both", length = unit(0.12, "cm"), type = "closed")) +
  annotate("text", x = spike_date + (ack_date - spike_date) / 2,
           y = y_top * 0.61, label = "11 days", size = 2.7,
           fontface = "bold.italic", color = "grey30") +
  event_lab(as.Date("2025-11-20"), y_top * 0.085,
            "14–15 Jan: repo stock jumps\novernight from 130 to 272 bn",
            EV_COLS["spike"]) +
  event_lab(ack_date + 4,    y_top * 0.74,
            "26 Jan: the central bank acknowledges\nIranian-origin inflows",
            EV_COLS["ack"]) +
  event_lab(hormuz_date + 4, y_top * 0.94,
            "End-Feb: closure\nof the strait",
            EV_COLS["hormuz"]) +
  event_lab(gdp_date + 4,    y_top * 0.47,
            "17 Apr: 2025 GDP growth 7.2%\n(forecasts: 4.5–5.2%)",
            EV_COLS["gdp"]) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.05))) +
  scale_y_continuous(labels = label_number(big.mark = ","),
                     limits = c(0, y_top * 1.04)) +
  labs(x = NULL, y = "Outstanding repo operations, AMD bn",
       caption = pub_note(
         "Central Bank of Armenia, analytical accounts of the CBA (daily), cba.am",
         note = "Daily stock of the central bank's repo provision to banks."))

save_pub(p14, "fig5_armenia_cba_timeline.png", w = 7.5, h = 4.4)
