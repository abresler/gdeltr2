# GDELT Skills, Agents & Slash Commands -- Strategic Scaffold

**Date:** 2026-02-20
**Method:** !TREEDOC + !PARETO-REBEL + !FREE + !SIGMA-FLOW

---

## !TREEDOC -- Capability Map of gdeltr2

The package exposes **107 functions** across **7 GDELT data stores**. Here is the tree of what is actually callable today:

```
gdeltr2/
|-- Events DB (V1/V2 bulk)
|   |-- get_data_gdelt_periods_event()     # bulk event downloads by date
|   |-- get_data_gdelt_period_event_totals()
|   |-- get_urls_gdelt_event_log()
|   |-- get_codes_cameo_*() [6 codebooks]  # country, ethnic, events, groups, religion, type
|   +-- gdelt_bq_events()                  # BigQuery events
|
|-- Global Knowledge Graph (GKG)
|   |-- get_data_gkg_days_summary()
|   |-- get_data_gkg_days_detailed()
|   |-- get_urls_gkg_15_minute_log()
|   |-- get_urls_gkg_most_recent_log()
|   |-- parse_gkg_mentioned_*() [12 parsers] # themes, people, orgs, locations, quotes, etc.
|   |-- get_codes_gkg_themes()
|   |-- get_codes_gcam()
|   +-- gdelt_bq_gkg()                     # BigQuery GKG
|
|-- Full Text API (V2)
|   |-- ft_v2_api()                        # ArtList, TimelineVol, TimelineTone, etc.
|   |-- ft_v1_api()                        # legacy V1
|   +-- instability_api_locations()        # stability/sentiment
|
|-- Context API (V2) -- sentence-level, 72hr window
|   +-- context_v2_api()
|
|-- TV API (V2)
|   |-- v2_tv_api()
|   |-- v2_tv_trends()
|   |-- get_data_tv_ngrams()
|   +-- dictionary_gdelt_tv_*()
|
|-- V3 Entity API
|   |-- v3_entity_api()
|   +-- v3_social_data()
|
|-- Visual GKG
|   |-- get_data_vgkg_dates()
|   |-- parse_vgkg_*() [6 parsers]        # faces, labels, landmarks, logos, OCR, safe_search
|   +-- vgkg_entity_count() / vgkg_label_count()
|
|-- Ancillary
|   |-- Global Frontpage Graph: get_data_gfg()
|   |-- Global Quotation Graph: get_data_gqg()
|   |-- Web NGrams: get_data_web_ngrams()
|   |-- Social Links: dictionary_social_links()
|   +-- BigQuery raw: gdelt_bq_query()
|
+-- NOT YET IMPLEMENTED (V3)
    |-- Global Difference Graph (GDG) -- article deletions/changes
    |-- Global Geographic Graph (GGG) -- 1.6B+ location mentions
    |-- Global Embedded Metadata (GEMG) -- HTML META/JSON-LD
    |-- GDELT Article List (GAL) -- real-time RSS only
    |-- Web NGrams 3.0 (JSONNL)
    +-- Visual GEG v2 (VGEG)
```

---

## !PARETO-REBEL -- The 20% That Drives 80% of Value

**What GDELT data actually matters for an intelligence practitioner?**

Stripping away the noise, the high-leverage functions cluster into **4 power zones**:

### Power Zone 1: Real-Time Pulse (Context API + Full Text API)
- `context_v2_api()` -- sentence-level search, 72hr window, quote extraction
- `ft_v2_api()` -- article search with tone/volume timelines, 250-result cap, ~45 day lookback

**Why it matters:** This is your real-time news intelligence layer. Every OSINT query, every newsbrief, every threat assessment starts here.

### Power Zone 2: Entity & Actor Intelligence (V3 + GKG parsers)
- `v3_entity_api()` -- named entities with sentiment, salience, Wikipedia links
- `parse_gkg_mentioned_people()`, `parse_gkg_mentioned_organizations()`, `parse_gkg_mentioned_locations()`
- `get_data_gqg()` -- who is being quoted saying what

**Why it matters:** Turns raw news into structured intelligence about WHO is doing WHAT WHERE.

### Power Zone 3: Trend & Narrative Detection (TV + NGrams + Timelines)
- `v2_tv_api()` + `v2_tv_trends()` -- what TV news is covering
- `get_data_tv_ngrams()` + `get_data_web_ngrams()` -- language frequency over time
- `ft_v2_api(mode = "TimelineVol")` -- volume spikes = breaking narratives

**Why it matters:** Detects narrative formation, media coordination, and information operations.

### Power Zone 4: Stability & Geographic Risk (Instability API + Events)
- `instability_api_locations()` -- geopolitical risk scoring
- `get_data_gdelt_periods_event()` -- CAMEO event coding (protests, conflicts, diplomacy)
- `get_codes_cameo_events()` -- event taxonomy

**Why it matters:** Converts news flow into quantified geopolitical risk signals.

**The rebel insight:** Most GDELT users treat it as a data dump. The real leverage is **composing these power zones into workflows** -- a single skill that chains Context API -> Entity extraction -> Tone analysis -> Geographic risk scoring turns raw GDELT into actionable intelligence briefs in seconds.

---

## !FREE -- Breaking Constraints

**Constraint 1: "GDELT is just an R package"**
Wrong. gdeltr2 is an **intelligence infrastructure layer**. Skills should treat it as a backend service, not a library to call manually.

**Constraint 2: "You need to know GDELT APIs to use it"**
Wrong. A well-designed skill/agent should accept natural language queries ("What's happening in Ukraine?") and translate them into the right API calls automatically.

**Constraint 3: "Each API is separate"**
Wrong. The maximum value comes from **fusing** outputs -- entity extraction + tone analysis + geographic coding + timeline trending on the same query.

**Constraint 4: "GDELT is for researchers"**
Wrong. With the right agent layer, GDELT becomes a real-time competitive intelligence, threat monitoring, and narrative analysis platform.

---

## !SIGMA-FLOW -- Skill & Agent Architecture Proposals

### Tier 1: Foundation Skill -- `/gdelt` (The Hub)

**Purpose:** Natural language interface to all gdeltr2 capabilities. The "one skill to rule them all."

**Workflow Routing:**

| Trigger | Workflow | gdeltr2 Functions |
|---------|----------|-------------------|
| "search gdelt for X" | `Search.md` | `ft_v2_api()`, `context_v2_api()` |
| "who is being mentioned with X" | `Entities.md` | `v3_entity_api()`, GKG parsers |
| "what's trending about X" | `Trends.md` | `v2_tv_api()`, ngrams, TimelineVol |
| "stability report for X" | `Stability.md` | `instability_api_locations()`, events |
| "full intel on X" | `FullIntel.md` | All power zones composed |
| "tv coverage of X" | `TV.md` | `v2_tv_api()`, `v2_tv_trends()`, ngrams |
| "quotes about X" | `Quotes.md` | `context_v2_api(is_quote=TRUE)`, `get_data_gqg()` |

**Key design:** Every workflow outputs a standardized intelligence object (tibble + markdown summary) that can be consumed by other skills (OSINT, Newsbrief, etc.).

---

### Tier 2: Specialized Skills

#### A. `/gdelt-watch` -- Continuous Monitoring Agent

**Purpose:** Set up persistent watchers on topics, entities, or regions. Runs on a schedule, alerts on threshold breaches.

**Core idea:**
```
/gdelt-watch "Ukraine" --interval 4h --alert-on tone_drop>2,volume_spike>3x
/gdelt-watch "Boeing" --entities --alert-on new_org_connection
/gdelt-watch --region "Taiwan Strait" --alert-on instability_score>threshold
```

**Architecture:**
- Stores watch configs in `~/.claude/gdelt-watches/`
- Each watch is a YAML spec: query, interval, thresholds, notification channel
- Agent runs `ft_v2_api(mode="TimelineVol")` + `context_v2_api()` at intervals
- Compares to baseline, fires alerts via voice notification or Obsidian note
- Produces daily/weekly rollup summaries

**gdeltr2 functions used:** `ft_v2_api()`, `context_v2_api()`, `instability_api_locations()`, `v3_entity_api()`

---

#### B. `/gdelt-osint` -- OSINT Enrichment Layer

**Purpose:** Extends existing OSINT skill with GDELT-backed intelligence. When investigating a person, company, or entity, automatically queries GDELT for media presence, tone, entity connections.

**Workflows:**

| Input | GDELT Enrichment |
|-------|-----------------|
| Person name | `context_v2_api(term=name, is_quote=TRUE)` for quotes, `v3_entity_api()` for co-mentioned entities, `ft_v2_api(mode="TimelineTone")` for sentiment trajectory |
| Company name | Same + `ft_v2_api(mode="TimelineVol")` for media volume, `parse_gkg_mentioned_organizations()` for network mapping |
| Country/Region | `instability_api_locations()` for risk score, `ft_v2_api(mode="TimelineSourceCountry")` for who is covering it, event data for CAMEO coding |
| Domain | `ft_v2_api(domain=X)` for what a publication covers, `dictionary_social_links()` for social presence |

**Integration point:** The existing OSINT skill's `PeopleTools.md`, `CompanyTools.md`, `EntityTools.md` each get a GDELT section.

---

#### C. `/gdelt-brief` -- Intelligence Brief Generator

**Purpose:** Generates structured intelligence briefs on any topic using multi-API fusion. Output is Obsidian-compatible markdown.

**Brief structure:**
1. **Executive Summary** -- 3-sentence overview from Context API
2. **Key Entities** -- People, orgs, locations from V3 Entity API
3. **Sentiment Trajectory** -- Tone timeline from Full Text API
4. **Media Volume** -- Coverage volume from TimelineVol
5. **Notable Quotes** -- Direct quotes from Context API (is_quote=TRUE)
6. **TV Coverage** -- TV mentions from TV API
7. **Geographic Risk** -- Instability scores if geographic topic
8. **Source Analysis** -- Which outlets are covering, from where (TimelineSourceCountry)
9. **Related Themes** -- GKG theme co-occurrence

**Output:** Saved to Obsidian vault with YAML frontmatter for Dataview queries.

**Integration with Newsbrief:** The existing Newsbrief skill's 8 sections could each call `/gdelt-brief` for data-backed analysis instead of pure web search.

---

#### D. `/gdelt-network` -- Entity Network Mapper

**Purpose:** Given a seed entity (person, org, country), maps the co-mention network from GDELT data.

**How it works:**
1. Query `v3_entity_api()` or `ft_v2_api()` for articles mentioning seed
2. Extract all co-mentioned entities via GKG parsers
3. Score edges by co-occurrence frequency and tone
4. Output as tibble (node list + edge list) suitable for network visualization
5. Generate markdown summary of key connections

**Use case:** "Show me who Boeing is connected to in the news this week" -> returns network of co-mentioned people, orgs, countries with sentiment scores.

---

### Tier 3: Slash Commands (Lightweight, No Full Skill)

These live in `~/.claude/commands/` as single-file markdown:

#### `/gdelt-search` -- Quick Search
```
Search GDELT for articles matching a query.
Usage: /gdelt-search "artificial intelligence regulation" --mode ArtList --days 7
Calls ft_v2_api() and returns formatted results.
```

#### `/gdelt-tone` -- Tone Check
```
Get sentiment trajectory for a topic.
Usage: /gdelt-tone "Federal Reserve" --days 30
Calls ft_v2_api(mode="TimelineTone") and renders a text sparkline.
```

#### `/gdelt-tv` -- TV Coverage
```
What is TV news saying about a topic?
Usage: /gdelt-tv "immigration" --stations CNN,FOXNEWS,MSNBC
Calls v2_tv_api() and compares coverage across stations.
```

#### `/gdelt-entities` -- Entity Lookup
```
Who/what is being mentioned alongside X?
Usage: /gdelt-entities "Elon Musk" --days 3
Calls v3_entity_api() filtered + GKG parsers.
```

#### `/gdelt-quotes` -- Quote Finder
```
Find direct quotes about a topic from global news.
Usage: /gdelt-quotes "tariffs" --days 1
Calls context_v2_api(is_quote=TRUE).
```

#### `/gdelt-risk` -- Geopolitical Risk
```
Instability/risk assessment for a location.
Usage: /gdelt-risk "Taiwan"
Calls instability_api_locations() + event data.
```

---

### Tier 4: Agent Definitions

#### `GDELTAnalyst` Agent
**Location:** `~/.claude/agents/gdelt-analyst.md`
**Role:** Autonomous intelligence analyst that uses gdeltr2 as its data backbone.
**Capabilities:**
- Accepts natural language intelligence questions
- Selects appropriate GDELT APIs automatically
- Fuses multi-source data into coherent assessments
- Produces structured output (tibble + markdown)
- Can be spawned as a teammate in swarm operations

**Use in teams:** When ChiefOfStaff or SonnetSwarm needs current events data, it spawns a GDELTAnalyst agent instead of relying on web search alone.

#### `GDELTMonitor` Agent
**Location:** `~/.claude/agents/gdelt-monitor.md`
**Role:** Background monitoring agent that runs periodic GDELT queries and surfaces anomalies.
**Capabilities:**
- Maintains watch list state
- Detects volume spikes, tone shifts, new entity connections
- Sends alerts via SendMessage to team lead

---

## Priority Matrix (What to Build First)

Using !PARETO-REBEL to order by impact/effort:

| Priority | Item | Impact | Effort | Rationale |
|----------|------|--------|--------|-----------|
| **P0** | `/gdelt` hub skill | 10 | M | Everything else routes through this. Build the router + Search workflow first. |
| **P1** | `/gdelt-search` command | 9 | S | Immediately useful, single-file, wraps ft_v2_api |
| **P1** | `/gdelt-quotes` command | 8 | S | Unique capability (sentence-level quote extraction), single-file |
| **P1** | `/gdelt-tone` command | 8 | S | Sentiment is the killer feature, single-file |
| **P2** | `/gdelt-brief` skill | 9 | L | High value but requires all workflows to exist first |
| **P2** | `/gdelt-osint` enrichment | 8 | M | Extends existing OSINT skill, high synergy |
| **P2** | `GDELTAnalyst` agent | 8 | M | Enables team/swarm integration |
| **P3** | `/gdelt-watch` skill | 7 | L | Requires persistence layer, scheduling |
| **P3** | `/gdelt-network` skill | 7 | L | Requires graph construction logic |
| **P3** | Remaining commands (tv, entities, risk) | 6 | S each | Fill out the command suite |

---

## Implementation Notes

### R Execution Pattern
All skills/commands that call gdeltr2 should use this pattern:
```bash
Rscript -e "
  library(gdeltr2)
  result <- ft_v2_api(term = 'YOUR_QUERY', mode = 'ArtList', maximum_records = 25)
  print(result)
"
```

Or for richer output, write to temp JSON:
```bash
Rscript -e "
  library(gdeltr2)
  result <- ft_v2_api(term = 'YOUR_QUERY', mode = 'ArtList', maximum_records = 25)
  jsonlite::write_json(result, '/tmp/gdelt_result.json')
"
```

### Key gdeltr2 Constraints to Encode in Skills
- V2 API: **250 result max per query**, ~45 day actual lookback despite params
- Context API: **72 hour window only**, 200 result max
- TV API: Different station codes needed (CNN, FOXNEWS, MSNBC, BBCNEWS, etc.)
- V3 Entity API: Data starts 2016-07-17, 15-minute update intervals
- BigQuery: Requires `bigrquery` + GCP credentials in `~/.Renviron`
- All functions return tibbles; `return_message = FALSE` for clean programmatic use

### Missing V3 Data Stores -- Future Expansion
Once implemented in gdeltr2, these unlock new skill capabilities:
- **GDG** (article changes) -> `/gdelt-watch` can detect narrative manipulation (articles being deleted/changed)
- **GGG** (geographic graph) -> `/gdelt-risk` gets 1.6B+ location mentions with context
- **GEMG** (metadata) -> structured data extraction from articles
- **VGEG v2** -> video/image analysis from TV news

---

## Discussion Points for Alex

1. **Hub-first or commands-first?** The P1 slash commands are faster to ship and immediately useful. The hub skill provides better UX long-term. Could do commands first, then wrap them in the hub.

2. **Newsbrief integration depth?** The Newsbrief skill currently uses web search for its 8 sections. How much should GDELT data replace vs. supplement web search?

3. **OSINT integration pattern?** Extend existing OSINT skill files directly, or create `/gdelt-osint` as a separate skill that OSINT calls?

4. **Persistence for /gdelt-watch?** Options: (a) cron + R script, (b) agent that re-invokes itself, (c) simple "check since last run" stateless approach. What fits your workflow?

5. **Output format preference?** Pure markdown to Obsidian vault? Tibble printed to console? JSON to temp file for downstream processing? All three depending on context?

6. **Should the GDELTAnalyst agent be available to all swarm/team operations by default?** i.e., should ChiefOfStaff automatically consider spawning one when the task involves current events?

7. **BigQuery path?** You have the bigrquery infrastructure in gdeltr2. BQ unlocks full historical depth (no 45-day limit). Worth building skills around BQ queries, or keep it API-only for now?
