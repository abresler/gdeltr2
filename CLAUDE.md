# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

gdeltr2 is an R package providing a modern interface to the [GDELT Project](http://gdeltproject.org/) (Global Database of Events, Language, and Tone). It wraps 7 GDELT data stores: Events DB, Global Knowledge Graph (GKG), Full Text API, TV API (V2), V1/V2/V3 APIs, Visual GKG, and Social Links.

Parent-level conventions (devtools workflow, naming, dependencies) are in `r_packages/CLAUDE.md`. This file covers gdeltr2-specific architecture only.

**No tests directory exists in this package.**

## Architecture (17 source files, ~19K lines)

| File | Lines | Purpose |
|------|-------|---------|
| `gdelt_event_gkg.R` | ~7400 | Events DB + GKG bulk download/parse, codebook accessors (`get_codes_*`), GKG column parsers (`parse_gkg_*`), schema definitions |
| `gdelt_full_text_api.R` | ~4950 | Full Text API (`ft_v2_api`), instability API (`instability_api_locations`), trelliscope visualizations, wordcloud generation |
| `v2.R` | ~1424 | V2 Doc API + V2 TV API URL generation, query building helpers |
| `tv_api.R` | ~1037 | TV Knowledge Graph API (`v2_tv_api`), trending topics, station/show dictionaries, term collapsing, inventory parsing |
| `v1_api.R` | ~764 | Legacy V1 Full Text Search API |
| `context_api.R` | ~470 | V2 Context API (`context_v2_api`), sentence-level search (past 72 hours) |
| `bigquery.R` | ~310 | BigQuery integration (`gdelt_bq_query`, `gdelt_bq_events`, `gdelt_bq_gkg`), requires bigrquery |
| `v3_entities.R` | ~299 | V3 entity/knowledge graph API |
| `api_readers.R` | ~285 | V2 API response parsing + column name munging, uses .safe_http_get |
| `ngrams.R` | ~273 | TV + web ngram data (`get_data_tv_ngrams`, `get_data_web_ngrams`) |
| `gqg.R` | ~249 | Global Quotation Graph (`get_data_gqg`), quoted statements from news |
| `gfg.R` | ~237 | Global Frontpage Graph (`get_data_gfg`), hourly homepage link snapshots |
| `gdeltr2-package.R` | ~225 | Package roxygen, cache env, parallel infra, HTTP retry, .onLoad warming |
| `social_links.R` | ~141 | Social media link extraction from articles |
| `vgkg.R` | ~64 | Visual GKG entity/label counts (memoized) |
| `other.R` | ~32 | Outlet dictionary |

## Function Naming Conventions

| Prefix | Purpose | Example |
|--------|---------|---------|
| `get_data_*` | Download and read data | `get_data_gdelt_periods_event()` |
| `get_urls_*` | Retrieve data store log URLs | `get_urls_gkg_15_minute_log()` |
| `get_codes_*` | Access CAMEO/GKG codebooks | `get_codes_cameo_country()` |
| `parse_gkg_*` | Parse concatenated GKG columns | `parse_gkg_mentioned_themes()` |
| `parse_vgkg_*` | Parse VGKG features | `parse_vgkg_labels()` |
| `ft_*` | Full Text API queries | `ft_v2_api()` |
| `v2_tv_*` | V2 Television API | `v2_tv_api()` |
| `dictionary_*` | Reference data lookups | `dictionary_stability_locations()` |
| `instability_api_*` | Stability/sentiment API | `instability_api_locations()` |
| `.dot_prefix()` | Internal helpers (not exported) | `.parse_tv()`, `.munge_names()` |

## Key Patterns

- **Memoise caching**: Results from codebook and URL log functions are memoised via `memoise::memoise()` to avoid redundant HTTP calls
- **data.table for bulk reads**: Large GKG/Event files use `data.table::fread()` for performance, then convert to tibble for output
- **API URL construction**: Functions build query URLs from parameters, then pass to internal readers. Pattern: build URL string -> `httr::GET()` or `curl` -> parse JSON/CSV response -> munge column names -> return tibble
- **Column name munging**: `api_readers.R` standardizes API response column names to package conventions (`type_*`, `name_*`, `date_*`)
- **`return_message` parameter**: Most data functions accept `return_message = TRUE` to print progress via `cli`/`glue`
- **`return_wide` parameter**: Parse functions accept `return_wide` to toggle between long and wide output formats
- **Parallel execution**: Uses `furrr::future_map()` for parallel downloads when processing multiple dates/periods

## GDELT API Base URLs

| Endpoint | Base URL |
|----------|----------|
| V2 bulk data | `http://data.gdeltproject.org/gdeltv2/` |
| V1 events | `http://data.gdeltproject.org/events/` |
| V2 API | `https://api.gdeltproject.org/api/v2/` |
| TV API | `https://api.gdeltproject.org/api/v2/tv/tv` |
| TV inventory | `http://data.gdeltproject.org/gdeltv3/iatv/inventory` |
| Television legacy | `http://television.gdeltproject.org/cgi-bin/iatv_ftxtsearch/` |

## Modernization Status (2026-02-19)

**Phase 1 (Complete)**: Deprecated tidyverse patterns migrated (funs->lambda, scoped verbs->across, gather->pivot_longer, spread->pivot_wider, native pipe).

**Phase 2 (Complete)**: New API modules added (bigquery.R, context_api.R, ngrams.R, gqg.R, gfg.R). Parallel processing infrastructure (.safe_map_dfr, .setup_parallel). HTTP retry infrastructure (.safe_http_get). Package-level cache environment (.pkg_cache). All bulk-download functions wired to use_parallel/workers params.

**Phase 3 (Complete)**: Optimization pass.
- Replaced all deprecated `flatten_chr()`/`flatten_dbl()` with `list_c()`/`str_split_1()` (~35 occurrences across 9 files)
- Memoized all HTTP-calling dictionary/codebook functions (dictionary_social_links, dictionary_geo_codebook, dictionary_ft_codebook, vgkg_label_count, vgkg_entity_count, get_urls_gkg_15_minute_log, get_urls_gdelt_event_log)
- Wired `.safe_http_get` retry logic into `.read_gdelt_api_json()` and `.read_gdelt_api_csv()` (core API readers)
- NAMESPACE cleanup: removed `.plot_ft_v2_highchart` export, removed deprecated `UQ`/`UQS` rlang imports, added all Phase 2 exports (17 new functions), added missing `importFrom` entries (httr::timeout, httr::status_code, jsonlite::stream_in, readr::read_lines, readr::parse_date, tidyr::separate_longer_delim, urltools::url_decode, lubridate::mdy_hm, lubridate::mdy_hms, lubridate::floor_date, lubridate::hours)
- DESCRIPTION: added `Suggests: bigrquery, callr`, added `parallel` to Imports
- Fixed critical `separate()` calls in log parsers with `extra = "merge", fill = "right"` and `show_col_types = FALSE` to eliminate blanket `suppressWarnings()`/`suppressMessages()` chains
- Added `rlang::abort()` with `gdeltr2_input_error` class to key exported functions
- Added `.onLoad()` background cache warming for CAMEO codebooks (via callr::r_bg)
- Added curl connection pooling infrastructure (.get_curl_handle, .gdelt_curl_pool)

**Phase 4 (Complete)**: Critical bug fixes in ft_v2_api pipeline.
- Fixed missing use_parallel/workers param forwarding from ft_v2_api to .query_gdelt_ft_v2_api
- Fixed vector if-condition in .parse_json_api_2 (`==` to `%in%` for data_names check)
- Fixed sourcelang URL encoding, curl/httr/jsonlite namespace prefixes
- Fixed .parse_timespan() native pipe compatibility, colMeans/flatten_df deprecations
- All ft_v2_api modes verified working: ArtList, TimelineVol, TimelineTone, TimelineSourceCountry, ToneChart

## Missing V3 Data Stores (Not Yet Implemented)

| Dataset | Path | Description |
|---------|------|-------------|
| Global Difference Graph (GDG) | `gdeltv3/gdg/` | News article deletions, URL redirects, title/text changes |
| Global Geographic Graph (GGG) | `gdeltv3/ggg/` | 1.6B+ location mentions with 600-char context snippets |
| Global Embedded Metadata Graph (GEMG) | `gdeltv3/gemg/` | HTML META tags and JSON-LD structured data from articles |
| GDELT Article List (GAL) | `gdeltv3/gal/` | Standardized article metadata with RSS feed |
| Web NGrams 3.0 (JSONNL) | `gdeltv3/webngrams/` | Per-ngram provenance, 152 languages (newer format than current) |
| Visual Global Entity Graph v2 (VGEG) | `gdeltv3/iatv/vgeg/` | Google Cloud Video AI on TV news (vgkg.R only has static 2019 CSVs) |

Also missing: Summary API (`api/v2/summary/summary`), Thematic Word Cloud Dashboard API (`api/v1/dash_thematicwordcloud/`).

Key remaining items:
- Fix remaining ~200 suppressWarnings in gdelt_event_gkg.R GKG field parsers (case-by-case separate() fixes)
- Fix known bugs (ft_v1_api_urls domain variable, .parse_v1_url_parameters no-op)
- Wire .safe_http_get into more functions beyond core API readers
- Add testthat test suite
- Implement missing V3 data stores (GDG, GGG, GEMG, GAL, WebNgrams 3.0, VGEG v2)
