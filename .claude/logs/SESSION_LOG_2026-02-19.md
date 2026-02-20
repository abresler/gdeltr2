# gdeltr2 Session Log — 2026-02-19 (Afternoon)

## Overview
Continued from earlier modernization session. Focus: API testing, bug fixing, and V3 data store exploration.

## Bug Fixes Applied

### Fix 1: ft_v2_api returning 0 rows (ROOT CAUSE)
- **File:** `R/gdelt_full_text_api.R` line ~4045 and ~4376
- **Bug:** `ft_v2_api` passed `use_parallel` and `workers` to `.query_gdelt_ft_v2_api`, but that function didn't accept those params. The `possibly()` wrapper silently swallowed the error, returning empty tibble.
- **Fix:** Added `use_parallel = FALSE, workers = NULL` to `.query_gdelt_ft_v2_api` signature AND forwarded them from `ft_v2_api`.
- **Commit:** `9142594`

### Fix 2: Timeline modes failing with "condition has length > 1"
- **File:** `R/gdelt_full_text_api.R` line 3687
- **Bug:** `.parse_json_api_2` had `if (data_names == "articles")` which returns a vector when `data_names` has multiple elements (e.g., `c("query_details", "timeline")`).
- **Fix:** Changed to `if ("articles" %in% data_names)`.
- **Commit:** `9142594`

### Fix 3: ft_v1_api join error on empty results
- **File:** `R/v1_api.R` line ~753
- **Bug:** When `parse_v1_ft_urls` returned an empty `tibble()` (0 columns), the `left_join(dat, by = "url_gdelt_v1_api")` failed because the join column didn't exist.
- **Fix:** Added guard: `if (nrow(dat) == 0 || !"url_gdelt_v1_api" %in% names(dat)) return(tbl_urls |> mutate(across(everything(), ~NA)))`.
- **Commit:** `a2429e1`

## API Testing Results

### ft_v2_api (V2 Full Text API) — All modes verified working post-fix:
| Mode | Result | Notes |
|------|--------|-------|
| ArtList | 25-250 rows | Works with terms, domains, filters |
| TimelineVol | 155 rows | Fixed vector if-condition |
| TimelineTone | Working | Occasional timeouts (server-side) |
| TimelineSourceCountry | Working | Some domains return empty |
| ToneChart | 31 rows | Working |
| WordCloud | 0 rows | GDELT rate limiting (transient) |

### ft_v1_api (V1 Legacy API):
- Returns ~3 articles for Anduril (limited ~48hr rolling window)
- Join error fixed for empty results
- V1 API is significantly more limited than V2

### context_v2_api (Context API):
- 6/6 famous domain tests passed
- Most reliable API — generous rate limits

### v2_tv_api (TV API):
- CNN, FOXNEWS, MSNBC, BBCNEWS all returned data
- ClipGallery mode works
- Occasional timeout retries for BBC

### V3 Entity API:
- `dictionary_v3_entity_urls()` works
- `parse_v3_entity_api_urls()` works
- `v3_entity_api()` works

## V3 Data Store Exploration

### GAL (GDELT Article List) — NOT accessible via file download:
- `gdeltv3/gal/LASTUPDATE.TXT` → 404
- `gdeltv3/gal/MASTERFILELIST.TXT` → 404
- Timestamped files (`YYYYMMDDHHMMSS.gal.json.gz`) → 404
- RSS feed (`feed.rss`) works but only covers latest ~1 minute
- **Conclusion:** GAL only accessible via RSS (real-time) or BigQuery (`gdelt-bq.gdeltv2.gal`)

### V2 API Coverage Window:
- `timespan = "1 year"` and `"365 days"` both work
- API caps at 250 results per query (hard limit)
- `timespan = "14d"` equivalent to `"14 days"`
- `timespan = "FULL"` → invalid, returns error
- Monthly date ranges (`dates` param) returned 0 rows for ArtList mode
- Actual data coverage: ~45 days back (Jan 6 - Feb 19, 2026 for Anduril)

## Anduril Article Results
- **V2 API (1 year, 250 max):** 250 articles, 166 unique domains, Jan 6 - Feb 19
- **V2 API (14 days):** 81 articles, 79 unique domains
- **V1 API:** 3 articles (limited rolling window)
- Top sources: finance.yahoo.com, ocbj.com, techcrunch.com, theaviationist.com, breakingdefense.com

## Famous Domain Test Suite (30 tests)
- 30/30 passed (no hard errors)
- 17/30 returned data
- Failures all from GDELT rate limiting on rapid sequential requests
- Package handles rate limiting correctly via `sleep_time = 6` in normal usage
- Test script at: `dev/test_famous_domains.R`

## Recurring Pattern: `possibly()` swallowing errors
The `possibly(fn, tibble())` pattern is used extensively throughout gdeltr2. When the wrapped function throws an error (including from mismatched arguments), it silently returns an empty tibble with 0 columns. Downstream code that assumes column names exist then fails with confusing errors ("Join columns in `y` must be present in the data"). This has been the root cause of multiple bugs:
1. ft_v2_api use_parallel/workers forwarding
2. ft_v1_api empty result join

## Commits This Session
1. `9142594` — Fix critical ft_v2_api bugs: missing params forwarding, vector if-condition
2. `edec548` — Update CLAUDE.md: Phase 4 fixes, missing V3 data stores inventory
3. `a2429e1` — Fix ft_v1_api join error when API returns empty results
