# Plan: Create CLAUDE.md for gdeltr2

## Context

The `/init` command was run to create a CLAUDE.md for the gdeltr2 R package — an interface to the GDELT Project (Global Database of Events, Language, and Tone). No CLAUDE.md exists yet. A parent-level CLAUDE.md at `r_packages/CLAUDE.md` covers generic R package conventions, so this file should be gdeltr2-specific only.

## Approach

Create `/Users/alexbresler/Desktop/r_packages/gdeltr2/CLAUDE.md` with:

1. **Required prefix** — exact text specified by user
2. **Build/dev commands** — standard devtools workflow (no tests exist in this package)
3. **Architecture** — 12 source files (16K lines) organized by GDELT data store:
   - `gdelt_event_gkg.R` (7424 lines) — Events DB + GKG bulk data download/parse
   - `gdelt_full_text_api.R` (4950 lines) — Full Text API, instability API, trelliscope viz
   - `v2.R` (1424 lines) — V2 Doc API + V2 TV API URL generation
   - `tv_api.R` (1037 lines) — TV Knowledge Graph API, term collapsing, parameter generation
   - `v1_api.R` (764 lines) — Legacy V1 Full Text Search API
   - `v3_entities.R` (299 lines) — V3 entity/knowledge graph API
   - `api_readers.R` (274 lines) — V2 API response parsing + name munging
   - `social_links.R` (128 lines) — Social media link extraction
   - `vgkg.R` (47 lines) — Visual GKG entity/label counts
   - `other.R` (32 lines) — Outlet dictionary
   - `gfg.R` (2 lines) — Global Frontpage Graph (stub)
   - `gdeltr2-package.R` (11 lines) — Package-level roxygen
4. **Function naming conventions** — prefix families
5. **Key patterns** — memoise caching, data.table for large reads, internal `.dot_prefix()` helpers, API URL construction pattern
6. **GDELT API base URLs** referenced throughout the code
7. **No tests directory** — note this explicitly

## Critical Files
- `/Users/alexbresler/Desktop/r_packages/gdeltr2/CLAUDE.md` (CREATE)

## Verification
- File exists with correct prefix
- Content is gdeltr2-specific, not repeating parent CLAUDE.md
- No boilerplate or made-up sections
