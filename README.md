# From Moscow to Hormuz — replication package

Code, data, and figures for:

> Tavadyan, A. (2026). *From Moscow to Hormuz: Central Bank Balance Sheets as Early
> Warnings of Geopolitical Capital Flight.*

A live implementation of the monitoring procedure described in section 5 runs at
[https://tvyal.com/rate/stress](https://tvyal.com/rate/stress).

## Contents

| Path | What it is |
|---|---|
| `manuscript.md` | The paper (pandoc Markdown, citations resolve against `references.bib`) |
| `references.bib` | Bibliography (86 entries) |
| `pub_02_capital_flows_imf.R` | Figures 2, 3, 4, 6 — IMF IRFCL reserves, gold, money-market rates, WEO GDP |
| `pub_03_hormuz_portwatch.R` | Figure 1 — Strait of Hormuz daily transits (IMF PortWatch) |
| `pub_04_armenia_cba_timeline.R` | Figure 5 — Central Bank of Armenia daily analytical accounts |
| `pub_theme.R` | Shared figure styling (sourced by the three scripts above) |
| `plots/pub/` | The six figures as published |
| `data/` | Frozen CSV snapshot of every input series, as fetched in July 2026 |

## Reproducing the figures

```sh
Rscript pub_02_capital_flows_imf.R
Rscript pub_03_hormuz_portwatch.R
Rscript pub_04_armenia_cba_timeline.R
```

Each script fetches its data live from the primary source (no API keys needed),
caches it in `cache/`, and writes PNGs to `plots/pub/`. R packages used:
tidyverse, scales, lubridate, httr, jsonlite, countrycode, ggrepel, readxl, ragg.
Figures use the Arial font family.

Note that live fetches will reflect source revisions made after July 2026 (the
IMF revises IRFCL, and has renamed series identifiers before). The `data/`
directory holds the exact vintage the published figures were built from:

| File | Source |
|---|---|
| `imf_irfcl_reserves_total_usd.csv` | IMF IRFCL, total reserve assets, USD |
| `imf_irfcl_reserve_gold_usd.csv` | IMF IRFCL, reserve gold, USD |
| `imf_irfcl_reserve_gold_fto.csv` | IMF IRFCL, reserve gold, fine troy ounces |
| `imf_mfs_money_market_rate.csv` | IMF MFS, money-market rate, % p.a. |
| `imf_weo_gdp_2025.csv` | IMF WEO, nominal GDP 2025, USD bn |
| `portwatch_hormuz_daily_transits.csv` | IMF PortWatch, Strait of Hormuz daily transit counts by vessel type |
| `cba_analytical_accounts_daily.csv` | Central Bank of Armenia, daily analytical accounts, mln AMD |

Reserve series in the paper are reserves *excluding* gold (total reserve assets minus reserve gold), so that valuation gains on gold do not contaminate the flow signal; see the data note in the manuscript.

## License

Code is released under the MIT License (see `LICENSE`). The manuscript text and figures are © the author; data belong to their primary sources (IMF, Central Bank of Armenia) and are redistributed here as fetched from their public APIs.
