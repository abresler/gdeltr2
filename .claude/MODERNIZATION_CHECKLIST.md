# gdeltr2 Modernization Checklist
## Phase 2: Parallel Processing, Performance, Correctness

**Created**: 2026-02-19
**Status**: Tasks 1-4 Complete, Remaining Items Below
**Previous Phase**: Tidyverse deprecation cleanup (completed)

---

## 1. DESCRIPTION / NAMESPACE Cleanup — COMPLETED

- [x] **1.1** Remove `purrrlyr` from Imports (deprecated, replaced by dplyr/tidyr)
- [x] **1.2** Remove `import(purrrlyr)` from NAMESPACE/roxygen
- [x] **1.3** Remove duplicate `tidyr` and `tibble` entries in Imports
- [x] **1.4** Remove `devtools` from Imports (runtime dep on a dev tool)
- [x] **1.5** Remove `import(tidyverse)` from NAMESPACE/roxygen (should import specific pkgs)
- [x] **1.6** Add `janitor` to Imports (used but not declared)
- [x] **1.7** Update `magrittr` importFrom: remove `|>` pipe import (native pipe, not magrittr)
- [x] **1.8** Remove `import(readxl)` from NAMESPACE (not used in any source file)
- [x] **1.9** Remove `import(stringi)` from NAMESPACE (not used directly)
- [x] **1.10** Ensure `furrr` and `future` are properly declared for parallel work
- [x] **1.11** Run `devtools::document()` to regenerate NAMESPACE after roxygen fixes

## 2. Deprecated tidyr/dplyr Patterns Still Present — COMPLETED

- [x] **2.1** Fix `unnest()` calls without `cols=` argument
- [x] **2.2** Replace all `nest_legacy()` calls with modern `nest()`
- [x] **2.3** Replace all `unnest_legacy()` calls with modern `unnest()`
- [x] **2.4** Fix `nest(-col)` deprecated syntax -> `nest(data = -col)`
- [x] **2.5** Replace `separate_rows()` with `separate_longer_delim()`
- [x] **2.6** Fix bare `unnest()` calls

## 3. Parallel Processing Infrastructure — COMPLETED

- [x] **3.1** Create `.setup_parallel()` internal helper
- [x] **3.2** Create `.safe_map_dfr()` wrapper with parallel fallback
- [x] **3.3** Add `use_parallel = FALSE` and `workers = NULL` parameters to bulk-download functions
- [x] **3.4** `get_data_gdelt_periods_event()` — parallelized + fixed FALSEALSE bug
- [x] **3.5** `get_data_gkg_days_detailed()` — parallelized + fixed FALSEALSE bug
- [x] **3.6** `get_data_gkg_days_summary()` — parallelized + fixed FALSEALSE bug
- [x] **3.7** `get_data_vgkg_dates()` — parallelized
- [x] **3.8** `instability_api_locations()` — parallelized
- [x] **3.9** `parse_v3_entity_api_urls()` — replaced curl_fetch_multi + <<- accumulator
- [x] **3.10** `parse_v3_social_urls()` — replaced curl_fetch_multi + <<- accumulator
- [x] **3.11** `parse_v2_tv_api_urls()` — replaced curl_fetch_multi + <<- accumulator
- [x] **3.12** `parse_v2_doc_api_urls()` — replaced curl_fetch_multi + <<- accumulator
- [x] **3.13** `parse_v1_ft_urls()` — replaced curl_fetch_multi + <<- accumulator
- [x] **3.14** `parse_summary_inventory_data_urls()` — replaced curl_fetch_multi + <<- accumulator
- [x] **3.15** `ft_v2_api()` — parallelized via pass-through
- [x] **3.16** `v2_doc_api()` — parallelized via pass-through
- [x] **3.17** `v2_tv_api()` — parallelized via pass-through

## 4. Bug Fixes and Code Quality — COMPLETED

- [x] **4.4** Fix `ft_v1_api_urls()` bug: `domain = domain` -> `domain = NA`
- [x] **4.5** Fix `str_remove_all("")` no-op in v1_api.R
- [x] **4.new1** Fix `as.numeric(., digits = N)` -> `round(as.numeric(.x), digits = N)` (5 locations in gdelt_event_gkg.R)
- [x] **4.new2** Replace `assign(x = ..., env = .GlobalEnv)` with package-level `.pkg_cache` (6 locations)
- [x] **4.new3** Remove `gc()` call in gdelt_full_text_api.R
- [x] **4.new4** Fix deprecated `@docType package` -> `"_PACKAGE"` pattern
- [x] **4.new5** Fix all bare function references after native pipe `|>` (ymd, ymd_hms, ungroup, flatten_chr, urltools::domain)
- [x] **4.new6** Fix `FALSEALSE` typos -> `FALSE` (2 locations)

---

## Remaining Work (Not Yet Started)

### 4. Error Handling and Resilience (P4)

- [ ] **4.1** Add `tryCatch` to exported functions in gdelt_event_gkg.R (49 exports, 0 have tryCatch)
- [ ] **4.2** Add HTTP status code checking before parsing (currently blind `read_tsv(url)` calls)
- [ ] **4.3** Add retry logic for transient HTTP failures in `.read_gdelt_api_json` and `.read_gdelt_api_csv`
- [ ] **4.6** Add URL validation before HTTP calls

### 5. Performance Optimizations (P5)

- [x] **5.4** Replace `curl_fetch_multi` + `walk` + `<<-` accumulator pattern (done in Task 3)
- [ ] **5.1** Replace `data.table::fread(url)` pattern with `arrow::read_csv_arrow()` where beneficial
- [ ] **5.2** Add `data.table::rbindlist()` instead of iterative `bind_rows()` in accumulator patterns
- [ ] **5.3** Memoize all codebook/dictionary functions that aren't already memoised

### 6. Code Quality / Modernization (P6)

- [x] **6.6** Remove `gc()` calls — DONE
- [x] **6.7** Remove `assign(x = ..., env = .GlobalEnv)` — DONE (replaced with .pkg_cache)
- [ ] **6.1** Replace `paste0(x, collapse=)` with `str_c()` or `glue()`
- [ ] **6.2** Replace `cat(fill = TRUE)` messaging with `cli::cli_alert_info()` (~20 locations)
- [ ] **6.3** Replace `message()` calls with `cli::cli_inform()`
- [ ] **6.4** Replace `crayon::red/blue/green()` with `cli::col_*()` (3 locations)
- [ ] **6.8** Replace `ifelse()` with `if_else()` for type safety
- [ ] **6.new** Migrate `separate()` to `separate_wider_delim()` (~65 calls across all files)

### 7. Function Verification (P7)

- [ ] **7.1** Verify `v2_tv_api()` returns data for basic query
- [ ] **7.2** Verify `v2_doc_api()` returns data for basic query
- [ ] **7.3** Verify `ft_v2_api()` returns data for basic query
- [ ] **7.4** Verify `v2_tv_trends()` returns trending data
- [ ] **7.5** Verify `dictionary_gdelt_tv_stations()` returns station list
- [ ] **7.6** Verify `get_codes_cameo_country()` returns codebook
- [ ] **7.7** Verify `get_urls_gkg_15_minute_log()` returns log URLs
- [ ] **7.8** Verify `instability_api_locations()` returns data
- [ ] **7.9** Verify `v3_entity_api()` returns entity data
- [ ] **7.10** Verify `dictionary_outlets()` returns outlet data

### 8. Documentation (P8)

- [ ] **8.1** Add `@examples` with `\dontrun{}` for all exported functions missing examples
- [ ] **8.2** Fill in missing `@return` tags across all exported functions
- [ ] **8.3** Fix typos in roxygen

---

## Summary

| Phase | Status | Items | Notes |
|-------|--------|-------|-------|
| Phase 1 (tidyverse deprecations) | COMPLETED | ~80 items | %>% to \|>, funs(), scoped verbs, gather/spread, select_/rename_, T/F |
| Task 1 (DESCRIPTION/NAMESPACE) | COMPLETED | 11 items | Package loads and documents cleanly |
| Task 2 (deprecated nest/unnest) | COMPLETED | 6 items | All legacy tidyr patterns replaced |
| Task 3 (parallel processing) | COMPLETED | 17 items | .safe_map_dfr infrastructure, curl_fetch_multi eliminated |
| Task 4 (bugs/code quality) | COMPLETED | 10 items | as.numeric bug, .GlobalEnv, gc(), bare functions, FALSEALSE |
| Remaining (P4-P8) | NOT STARTED | ~25 items | Error handling, performance, code quality, verification, docs |
