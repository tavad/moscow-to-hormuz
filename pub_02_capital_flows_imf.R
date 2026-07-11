# ==============================================================================
# pub_02_capital_flows_imf.R — publication copy of 02_capital_flows_imf.R.
# The in-house original is untouched. Produces manuscript Figures 2, 3, 4, 6.
#
# Data section (IMF IRFCL / MFS / WEO fetch + parsing) is copied verbatim from
# the original and shares its cache/. Differences are presentation-side:
#   - pub theme (no in-image titles/subtitles, no logo, no tvyal.com URLs);
#   - Figure 2: shaded bands now match the manuscript caption — Russian wave
#     (Feb–Dec 2022) and Iranian wave (Oct 2025 → latest); Hormuz closure is a
#     dated line at end-Feb 2026 (PortWatch shows the collapse on 1–2 March);
#   - Figure 6: axes transposed to match the caption (reserves on x, rates on
#     y: absorbers lower-right, defenders upper-left) and shown for BOTH waves
#     (Feb–Aug 2022 and Oct 2025 → latest) instead of the latest 3 months;
#   - Okabe-Ito palette, every series direct-labelled (grayscale-print safe).
# ==============================================================================

suppressPackageStartupMessages({
  library(lubridate)
  library(ggrepel)
  library(httr)
  library(jsonlite)
  library(countrycode)
})
source("pub_theme.R")

# ── Config (as in original) ───────────────────────────────────────────────────
IMF_BASE  <- "https://api.imf.org/external/sdmx/2.1"
CACHE_DIR <- "cache"
dir.create(CACHE_DIR, showWarnings = FALSE)

CRISIS_START  <- as.Date("2022-02-01")
FTO_PER_TONNE <- 32150.7466574

HIGHLIGHT <- c("CHE","SGP","IND","CHN","ARM","SAU","RUS","TUR","HKG",
               "ARE","JPN","UKR","GEO","AZE","USA","DEU")

iso_label <- function(iso3) {
  lab <- countrycode(iso3, "iso3c", "country.name", warn = FALSE)
  ov  <- c(USA="USA", GBR="UK", KOR="S. Korea", ARE="UAE", HKG="Hong Kong",
           CHE="Switzerland", SGP="Singapore", CZE="Czechia", RUS="Russia",
           TUR="Turkey", VNM="Vietnam", LAO="Laos")
  out <- ifelse(iso3 %in% names(ov), ov[iso3], lab)
  out[is.na(out)] <- iso3[is.na(out)]
  unname(out)
}

# ── IMF SDMX-JSON helpers (verbatim from original) ────────────────────────────
imf_time_to_date <- function(x) {
  d  <- as.Date(rep(NA_character_, length(x)))
  m  <- str_match(x, "^(\\d{4})-M(\\d{2})$")
  q  <- str_match(x, "^(\\d{4})-Q(\\d)$")
  mm <- !is.na(m[,1]); d[mm] <- as.Date(sprintf("%s-%s-01", m[mm,2], m[mm,3]))
  qq <- !is.na(q[,1]); d[qq] <- as.Date(sprintf("%s-%02d-01", q[qq,2],
                                                 (as.integer(q[qq,3]) - 1) * 3 + 1))
  yy <- str_detect(x, "^\\d{4}$"); d[yy] <- as.Date(sprintf("%s-01-01", x[yy]))
  d
}

parse_sdmx_json <- function(j) {
  sdims    <- j$structure$dimensions$series
  dim_ids  <- vapply(sdims, function(d) d$id, character(1))
  dim_code <- lapply(sdims, function(d) vapply(d$values, function(v) v$id, character(1)))
  tvals    <- j$structure$dimensions$observation[[1]]$values
  time_ids <- vapply(tvals, function(t) t$id, character(1))
  series   <- j$dataSets[[1]]$series
  if (length(series) == 0) return(tibble())

  out <- vector("list", length(series))
  keys <- names(series)
  for (si in seq_along(series)) {
    idx     <- as.integer(strsplit(keys[si], ":", fixed = TRUE)[[1]]) + 1L
    keyvals <- setNames(mapply(function(codes, i) codes[i], dim_code, idx), dim_ids)
    obs     <- series[[si]]$observations
    if (length(obs) == 0) next
    oi   <- as.integer(names(obs)) + 1L
    vals <- vapply(obs, function(o) { v <- o[[1]]
                   if (is.null(v)) NA_character_ else as.character(v) }, character(1))
    out[[si]] <- tibble(!!!as.list(keyvals),
                        time  = time_ids[oi],
                        value = suppressWarnings(as.numeric(vals)))
  }
  bind_rows(out)
}

imf_fetch <- function(flow, key, start = "2015-01",
                      cache_name = NULL, ttl_hours = 24) {
  cache_file <- if (!is.null(cache_name)) file.path(CACHE_DIR, cache_name) else NULL
  if (!is.null(cache_file) && file.exists(cache_file) &&
      difftime(Sys.time(), file.mtime(cache_file), units = "hours") < ttl_hours) {
    return(readRDS(cache_file))
  }
  url <- sprintf("%s/data/%s/%s?startPeriod=%s", IMF_BASE, flow, key, start)
  r   <- GET(url, add_headers(Accept = "application/json"), timeout(180))
  stop_for_status(r)
  j   <- fromJSON(content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  df  <- parse_sdmx_json(j)
  if (nrow(df) == 0) { warning("IMF returned no data: ", flow, "/", key) ; return(df) }
  df  <- df |> mutate(date = imf_time_to_date(time))
  if (!is.null(cache_file)) saveRDS(df, cache_file)
  df
}

gdp_fetch <- function(year = "2025", cache_name = "02_imf_gdp.rds", ttl_hours = 168) {
  cache_file <- file.path(CACHE_DIR, cache_name)
  if (file.exists(cache_file) &&
      difftime(Sys.time(), file.mtime(cache_file), units = "hours") < ttl_hours) {
    return(readRDS(cache_file))
  }
  r <- GET("https://www.imf.org/external/datamapper/api/v1/NGDPD", timeout(120))
  stop_for_status(r)
  vals <- fromJSON(content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)$values$NGDPD
  df <- tibble(
    iso3  = names(vals),
    gdp_b = vapply(vals, function(v) { x <- v[[year]]
                  if (is.null(x)) NA_real_ else as.numeric(x) }, numeric(1))
  ) |> filter(!is.na(gdp_b))
  saveRDS(df, cache_file)
  df
}

# ── Load (verbatim from original) ─────────────────────────────────────────────
cat("Fetching IMF IRFCL total reserves (USD)...\n")
fx_total <- imf_fetch("IRFCL", ".IRFCLDT1_IRFCL65_USD.S1XS1311.M",
                      start = "2015-01", cache_name = "02_imf_fx_total.rds", ttl_hours = 24) |>
  transmute(iso3 = COUNTRY, date, total_musd = value / 1e6)

cat("Fetching IMF IRFCL reserve gold value (USD)...\n")
gold_usd <- imf_fetch("IRFCL", ".IRFCLDT1_IRFCL56_USD.S1XS1311.M",
                      start = "2015-01", cache_name = "02_imf_gold_usd.rds", ttl_hours = 24) |>
  transmute(iso3 = COUNTRY, date, gold_musd = value / 1e6)

fx_raw <- fx_total |>
  left_join(gold_usd, by = c("iso3", "date")) |>
  mutate(value = total_musd - coalesce(gold_musd, 0)) |>
  filter(!is.na(value), value > 0, date <= Sys.Date()) |>
  mutate(label = iso_label(iso3))

cat("Fetching IMF IRFCL gold volume (fine troy oz)...\n")
# IRFCL 12.0.0 (2026-07-09) renamed IRFCL56_FTO -> IRFCL56V_FTO (volume; GB/UG
# split out bullion vs unallocated). Own cache name — the original script's
# cache stays on the old code.
gold_raw <- imf_fetch("IRFCL", ".IRFCLDT1_IRFCL56V_FTO.S1XS1311.M",
                      start = "2015-01", cache_name = "pub_02_imf_gold_v.rds", ttl_hours = 24) |>
  transmute(iso3 = COUNTRY, date, value = value / FTO_PER_TONNE) |>
  filter(!is.na(value), value > 0, date <= Sys.Date()) |>
  group_by(iso3) |>
  filter(max(value) <= 9000) |>
  # drop unit-break series: any 100x month-over-month jump is a reporting
  # error, not a purchase (e.g. Honduras 0.696 t -> 696 t in May 2026)
  arrange(date, .by_group = TRUE) |>
  filter(all(abs(diff(log10(value))) < 2)) |>
  ungroup() |>
  mutate(label = iso_label(iso3))

cat("Fetching IMF MFS money-market rate...\n")
rate_raw <- imf_fetch("MFS_IR", ".MMRT_RT_PT_A_PT.M",
                      start = "2015-01", cache_name = "02_imf_rate.rds", ttl_hours = 24) |>
  transmute(iso3 = COUNTRY, date, value) |>
  filter(!is.na(value), date <= Sys.Date())

cat("Fetching IMF WEO GDP...\n")
gdp_tbl <- gdp_fetch("2025")

cat(sprintf("Coverage: reserves %d economies | gold %d | rates %d | gdp %d\n",
            n_distinct(fx_raw$iso3), n_distinct(gold_raw$iso3),
            n_distinct(rate_raw$iso3), nrow(gdp_tbl)))

SRC_RES  <- "IMF International Reserves and Foreign Currency Liquidity (IRFCL)"
SRC_GOLD <- "IMF IRFCL, reserve gold in fine troy ounces, converted to tonnes"

# ══ Figure 2 — FX reserve divergence since Feb 2022, indexed ═════════════════
cat("Figure 2...\n")
P12 <- c("CHE","SGP","IND","ARM","GEO","CHN","TUR","HKG","RUS","JPN")
P12_COLORS <- c("Armenia"     = unname(OI["black"]),
                "Georgia"     = "#8C510A",
                "Switzerland" = unname(OI["vermillion"]),
                "Singapore"   = unname(OI["blue"]),
                "India"       = unname(OI["orange"]),
                "China"       = unname(OI["purple"]),
                "Turkey"      = unname(OI["skyblue"]),
                "Japan"       = unname(OI["green"]),
                "Hong Kong"   = "#8C8C8C",
                "Russia"      = "#BDBDBD")

p12_df <- fx_raw |>
  filter(iso3 %in% P12) |>
  group_by(iso3) |>
  arrange(date) |>
  mutate(base_val = { v <- value[date >= CRISIS_START]; if (length(v) > 0) v[1] else NA_real_ },
         indexed  = value / base_val * 100) |>
  filter(!is.na(indexed), date >= CRISIS_START) |>
  ungroup()

p12_labels <- p12_df |> group_by(iso3, label) |> slice_max(date, n = 1) |> ungroup()

russian_wave <- c(as.Date("2022-02-01"), as.Date("2022-12-31"))
iranian_wave <- c(as.Date("2025-10-01"), max(p12_df$date))
closure_date <- as.Date("2026-02-28")
y_top12      <- max(p12_df$indexed)

p12 <- ggplot(p12_df, aes(date, indexed, color = label)) +
  annotate("rect", xmin = russian_wave[1], xmax = russian_wave[2],
           ymin = -Inf, ymax = Inf, fill = "grey55", alpha = 0.13) +
  annotate("rect", xmin = iranian_wave[1], xmax = iranian_wave[2],
           ymin = -Inf, ymax = Inf, fill = "grey55", alpha = 0.13) +
  annotate("text", x = russian_wave[1] + 12, y = y_top12 * 1.03,
           label = "Russian wave (2022)", hjust = 0, size = 2.7, color = "grey30") +
  annotate("text", x = iranian_wave[1] + 12, y = y_top12 * 1.03,
           label = "Iranian wave (2025–26)", hjust = 1, size = 2.7, color = "grey30") +
  geom_vline(xintercept = as.numeric(closure_date), color = "grey20",
             linetype = "dotted", linewidth = 0.45) +
  annotate("text", x = closure_date + 10, y = 45,
           label = "Hormuz\nclosure", hjust = 0, size = 2.5, color = "grey30",
           lineheight = 0.95) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey55", linewidth = 0.4) +
  geom_line(data = filter(p12_df, iso3 != "ARM"), linewidth = 0.65) +
  geom_line(data = filter(p12_df, iso3 == "ARM"), linewidth = 1.1) +
  geom_label_repel(data = p12_labels,
                   aes(label = sprintf("%s (%.0f)", label, indexed)),
                   size = 2.6, fontface = "bold", nudge_x = 40, direction = "y",
                   box.padding = 0.22, max.overlaps = 12, label.size = 0.15,
                   alpha = 0.95, seed = 42) +
  scale_color_manual(values = P12_COLORS, guide = "none") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.09))) +
  scale_y_continuous(labels = \(x) sprintf("%.0f", x)) +
  labs(x = NULL, y = "FX reserves excluding gold (Feb 2022 = 100)",
       caption = pub_note(SRC_RES,
         note = "Monthly, in US dollars. Armenia in black."))

save_pub(p12, "fig2_fx_reserve_divergence.png", w = 7.5, h = 4.4)

# ══ Figure 3 — cumulative reserve change since Feb 2022, % of GDP ═════════════
cat("Figure 3...\n")
EURO_ARTIFACTS <- c("HRV", "BGR")
FORCE_10B      <- c("ARM","MDA","GEO","KAZ","JOR","CHE","SGP","HKG","JPN","CHN","RUS")
CORRIDOR_10B   <- c("ARM","MDA","GEO","KAZ","JOR","SRB","MNG")

cum_2022 <- fx_raw |>
  group_by(iso3, label) |>
  arrange(date) |>
  summarise(
    base_feb22  = value[date == as.Date("2022-02-01")][1],
    latest_val  = last(value),
    latest_date = max(date),
    .groups = "drop"
  ) |>
  filter(!is.na(base_feb22), latest_date >= as.Date("2025-10-01"),
         !iso3 %in% EURO_ARTIFACTS) |>
  left_join(gdp_tbl, by = "iso3") |>
  mutate(gdp_cum_pct = (latest_val - base_feb22) / 1e3 / gdp_b * 100) |>
  filter(!is.na(gdp_cum_pct), is.finite(gdp_cum_pct))

top_10b <- bind_rows(
    slice_max(cum_2022, gdp_cum_pct, n = 10),
    slice_min(cum_2022, gdp_cum_pct, n = 4),
    filter(cum_2022, iso3 %in% FORCE_10B)
  ) |>
  distinct(iso3, .keep_all = TRUE) |>
  arrange(gdp_cum_pct) |>
  mutate(direction  = if_else(gdp_cum_pct >= 0, "Accumulation", "Decline"),
         is_hl      = iso3 %in% CORRIDOR_10B,
         label_disp = fct_inorder(label),
         label_face = if_else(is_hl, "bold", "plain"))

p10b <- ggplot(top_10b, aes(gdp_cum_pct, label_disp, fill = direction)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "grey40") +
  geom_text(aes(label = sprintf("%+.1f", gdp_cum_pct),
                hjust = if_else(gdp_cum_pct >= 0, -0.15, 1.15)),
            size = 2.5, color = "grey30") +
  scale_fill_manual(values = c("Accumulation" = unname(OI["blue"]),
                               "Decline"      = unname(OI["vermillion"]))) +
  scale_x_continuous(labels = \(x) sprintf("%+.0f%%", x),
                     expand = expansion(mult = c(0.08, 0.12))) +
  labs(x = "Change in FX reserves since February 2022, % of GDP", y = NULL,
       caption = pub_note(paste0(SRC_RES, "; GDP: IMF WEO"),
         note = paste0("FX reserves exclude gold. Corridor and transit economies ",
                       "in bold.\nCroatia and Bulgaria excluded (euro-accession ",
                       "transfers to the ECB, not flows)."))) +
  theme(axis.text.y = element_text(
          face = top_10b$label_face[order(as.integer(top_10b$label_disp))],
          size = 8),
        legend.position = "none")

save_pub(p10b, "fig3_reserve_change_gdp.png", w = 6.8, h = 5.2)

# ══ Figure 4 — gold accumulation since Feb 2022 ═══════════════════════════════
cat("Figure 4...\n")
gold_since_2022 <- gold_raw |>
  filter(date >= CRISIS_START) |>
  group_by(iso3, label) |>
  arrange(date) |>
  filter(n() >= 3) |>
  mutate(base_val = first(value), cum_change = value - base_val) |>
  ungroup()

top_gold_buyers <- gold_since_2022 |>
  group_by(iso3, label) |>
  summarise(total_change = last(cum_change), .groups = "drop") |>
  slice_max(total_change, n = 12)

gold_plot_df <- gold_since_2022 |> filter(iso3 %in% top_gold_buyers$iso3)

gold_cols <- setNames(
  rep(unname(OI[c("blue","vermillion","green","orange","purple","skyblue")]),
      length.out = nrow(top_gold_buyers)),
  top_gold_buyers$label
)

p9 <- ggplot(gold_plot_df, aes(date, cum_change, color = label)) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey60") +
  geom_line(linewidth = 0.7) +
  geom_label_repel(
    data = gold_plot_df |> group_by(iso3) |> slice_max(date, n = 1),
    aes(label = sprintf("%s (%+.0f t)", label, cum_change)),
    size = 2.5, fontface = "bold", nudge_x = 60, direction = "y",
    box.padding = 0.18, max.overlaps = 15, label.size = 0.15, seed = 42
  ) +
  scale_color_manual(values = gold_cols, guide = "none") +
  scale_x_date(breaks = seq(as.Date("2022-07-01"), max(gold_plot_df$date),
                            by = "6 months"),
               date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.14))) +
  scale_y_continuous(labels = \(x) sprintf("%+.0f", x)) +
  labs(x = NULL, y = "Cumulative change since Feb 2022, tonnes",
       caption = pub_note(SRC_GOLD,
         note = "The twelve largest accumulators among IRFCL reporters."))

save_pub(p9, "fig4_gold_accumulation.png", w = 7.2, h = 4.8)

# ══ Figure 6 — absorption vs defence, both waves ══════════════════════════════
# Axes follow the manuscript caption: reserve dynamics on x, policy-rate proxy
# (money-market rate) change on y. Absorbers sit lower-right (reserves up,
# rates flat), defenders upper-left (reserves down, rates up).
cat("Figure 6...\n")

window_change <- function(res_df, rate_df, start, end, wave_label) {
  res <- res_df |>
    group_by(iso3, label) |>
    summarise(
      v_start = { v <- value[date <= start]; if (length(v)) last(v) else NA_real_ },
      v_end   = { v <- value[date <= end];   if (length(v)) last(v) else NA_real_ },
      d_start = { d <- date[date <= start];  if (length(d)) max(d) else as.Date(NA) },
      .groups = "drop"
    ) |>
    filter(!is.na(v_start), !is.na(v_end), v_start > 0,
           d_start >= start %m-% months(3)) |>
    mutate(res_pct = (v_end - v_start) / v_start * 100)

  rt <- rate_df |>
    group_by(iso3) |>
    summarise(
      r_start = { v <- value[date <= start]; if (length(v)) last(v) else NA_real_ },
      r_end   = { v <- value[date <= end];   if (length(v)) last(v) else NA_real_ },
      .groups = "drop"
    ) |>
    filter(!is.na(r_start), !is.na(r_end)) |>
    mutate(rate_chg = r_end - r_start)

  res |>
    inner_join(rt, by = "iso3") |>
    filter(abs(res_pct) < 100, abs(rate_chg) < 15) |>
    mutate(wave = wave_label)
}

wave1_end <- as.Date("2022-08-01")
wave2_end <- max(fx_raw$date)

waves_df <- bind_rows(
  window_change(fx_raw, rate_raw, as.Date("2022-02-01"), wave1_end,
                "Russian wave (Feb–Aug 2022)"),
  window_change(fx_raw, rate_raw, as.Date("2025-10-01"), wave2_end,
                sprintf("Iranian wave (Oct 2025–%s)", format(wave2_end, "%b %Y")))
) |>
  # euro-accession reserve transfers (Bulgaria joined the euro area Jan 2026)
  # are not capital flows — same exclusion as Figure 3
  filter(!iso3 %in% EURO_ARTIFACTS) |>
  mutate(
    wave = fct_inorder(wave),
    zone = case_when(
      res_pct < -2 & rate_chg > 0  ~ "Defending (reserves down, rates up)",
      res_pct < -2 & rate_chg <= 0 ~ "Reserve drain (reserves down, rates flat)",
      res_pct >= 2 & rate_chg <= 0 ~ "Absorbing (reserves up, rates flat)",
      res_pct >= 2 & rate_chg > 0  ~ "Absorbing while tightening",
      TRUE ~ "Stable"),
    show_label = iso3 %in% HIGHLIGHT | abs(res_pct) > 40 | abs(rate_chg) > 5
  )

zone_cols <- c(
  "Defending (reserves down, rates up)"       = unname(OI["vermillion"]),
  "Reserve drain (reserves down, rates flat)" = unname(OI["orange"]),
  "Absorbing (reserves up, rates flat)"       = unname(OI["blue"]),
  "Absorbing while tightening"                = unname(OI["skyblue"]),
  "Stable"                                    = "grey72")

p11 <- ggplot(waves_df, aes(res_pct, rate_chg, color = zone)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = 0, ymax = Inf,
           fill = "grey50", alpha = 0.08) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "grey50") +
  geom_point(data = \(d) filter(d, !show_label), size = 1.5, alpha = 0.45) +
  geom_point(data = \(d) filter(d, show_label), size = 2.6, alpha = 0.9) +
  geom_label_repel(data = \(d) filter(d, show_label), aes(label = label),
                   size = 2.4, fontface = "bold", box.padding = 0.35,
                   max.overlaps = 20, label.size = 0.15, alpha = 0.92,
                   seed = 42, show.legend = FALSE) +
  facet_wrap(~wave, scales = "free_x") +
  scale_color_manual(values = zone_cols,
                     guide = guide_legend(nrow = 2, byrow = TRUE,
                                          override.aes = list(size = 2.6))) +
  scale_x_continuous(labels = \(x) sprintf("%+.0f%%", x)) +
  scale_y_continuous(labels = \(x) sprintf("%+.1f", x)) +
  labs(x = "Change in FX reserves excluding gold over the wave window, %",
       y = "Change in money-market rate, pp",
       caption = pub_note(
         paste0(SRC_RES, "; rates: IMF MFS money-market rate"),
         note = paste0("Shaded quadrant: currency defence (reserves down, ",
                       "rates up)."))) +
  theme(legend.position = "bottom")

save_pub(p11, "fig6_absorption_vs_defence.png", w = 7.4, h = 4.8)

# ── Console summary ───────────────────────────────────────────────────────────
cat("\n── Publication-figure data summary ─────────────────────────────────\n")
cat(sprintf("IRFCL latest month: %s\n", format(max(fx_raw$date), "%B %Y")))

arm2 <- p12_df |> filter(iso3 == "ARM") |> slice_max(date, n = 1)
cat(sprintf("Fig 2 — Armenia index at %s: %.0f (Feb 2022 = 100)\n",
            format(arm2$date, "%b %Y"), arm2$indexed))

cat("Fig 3 — corridor bars (% of GDP):\n")
top_10b |> filter(is_hl) |>
  select(label, gdp_cum_pct, latest_date) |>
  pwalk(\(label, gdp_cum_pct, latest_date, ...)
        cat(sprintf("  %-12s %+6.1f%%  (data: %s)\n", label, gdp_cum_pct, latest_date)))

cat("Fig 4 — top gold accumulators (tonnes since Feb 2022):\n")
gold_plot_df |> group_by(label) |> slice_max(date, n = 1) |> ungroup() |>
  arrange(desc(cum_change)) |> slice_head(n = 6) |>
  select(label, cum_change, date) |>
  pwalk(\(label, cum_change, date, ...)
        cat(sprintf("  %-12s %+6.0f t  (data: %s)\n", label, cum_change, date)))

cat("Fig 6 — Armenia position:\n")
waves_df |> filter(iso3 == "ARM") |>
  select(wave, res_pct, rate_chg, zone) |>
  pwalk(\(wave, res_pct, rate_chg, zone, ...)
        cat(sprintf("  %-32s reserves %+6.1f%%  rate %+5.2f pp  -> %s\n",
                    wave, res_pct, rate_chg, zone)))
