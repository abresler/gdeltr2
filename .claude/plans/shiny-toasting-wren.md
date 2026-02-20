# Plan: GDELT Skills Suite + File Organization

## Context

gdeltr2 has been modernized (Phases 1-4 complete) with 107 working functions across 7 GDELT data stores. The ChiefOfStaff produced a strategic analysis identifying 4 "power zones" and proposing skills, agents, and slash commands to make GDELT intelligence accessible via natural language. Alex wants these built as **activatable but not active by default**, and wants generated files organized cleanly in `.claude/` rather than scattered across the project.

## Part 1: File Organization Rule

**Add to `r_packages/CLAUDE.md`** (workspace-wide convention):
```
## .claude/ Directory Convention
Generated session artifacts (plans, logs, checklists, agent outputs) go in `.claude/` subdirectories, not the package root or ad-hoc folders like `Plans/`. Structure:
- `.claude/plans/` — strategic plans, design docs
- `.claude/logs/` — session logs
- `.claude/skills/` — package-specific skills
```

**For gdeltr2 specifically:**
- Move `Plans/shiny-toasting-wren-agent-acaf306.md` -> `.claude/plans/gdelt-skills-strategy.md`
- Move `Plans/unified-popping-cake.md` -> `.claude/plans/`
- Move `.claude/MODERNIZATION_CHECKLIST.md` -> keep (already in `.claude/`)
- Move `.claude/SESSION_LOG_2026-02-19.md` -> `.claude/logs/`
- Remove empty `Plans/` directory (keep `Plans/CLAUDE.md` content merged into main CLAUDE.md if needed)

## Part 2: GDELT Skills Suite (Activatable, Not Active by Default)

All skills go in `~/.claude/skills-disabled/` and agents in `~/.claude/agents-disabled/`. Users activate with `pai-skill enable GdeltSearch` etc.

### P1 — Slash Commands (Small, single-file, immediately useful)

These are always-active by nature (commands don't have a disabled state), so they go in `~/.claude/commands/`:

| Command | File | gdeltr2 Functions |
|---------|------|-------------------|
| `/gdelt-search` | `gdelt-search.md` | `ft_v2_api(mode="ArtList")` |
| `/gdelt-quotes` | `gdelt-quotes.md` | `context_v2_api(is_quote=TRUE)` |
| `/gdelt-tone` | `gdelt-tone.md` | `ft_v2_api(mode="TimelineTone")` |
| `/gdelt-tv` | `gdelt-tv.md` | `v2_tv_api()` |
| `/gdelt-entities` | `gdelt-entities.md` | `v3_entity_api()` |
| `/gdelt-risk` | `gdelt-risk.md` | `instability_api_locations()` |

Each command follows the pattern:
```markdown
---
name: gdelt-search
description: Search GDELT for articles matching a query
category: intelligence
---
# GDELT Article Search
[R execution pattern using Rscript -e with gdeltr2]
[Accepts $ARGUMENTS for query term, optional --days, --mode flags]
```

### P2 — Skills (Disabled by default, richer workflows)

| Skill | Location | Purpose |
|-------|----------|---------|
| `GdeltBrief` | `skills-disabled/GdeltBrief/` | Multi-API fusion intel brief |
| `GdeltOsint` | `skills-disabled/GdeltOsint/` | OSINT enrichment layer |

Each skill has: `SKILL.md` + `Tools/` + `Workflows/`

**GdeltBrief** workflows: generates 9-section intelligence brief (Executive Summary, Key Entities, Sentiment, Volume, Quotes, TV Coverage, Geographic Risk, Source Analysis, Related Themes). Outputs Obsidian-compatible markdown.

**GdeltOsint** workflows: Person enrichment, Company enrichment, Country/Region enrichment, Domain analysis. Each queries relevant GDELT APIs and returns structured tibble + markdown.

### P3 — Agents (Disabled by default)

| Agent | Location | Role |
|-------|----------|------|
| `GDELTAnalyst` | `agents-disabled/GDELTAnalyst.md` | Autonomous intel analyst using gdeltr2 as data backbone |
| `GDELTMonitor` | `agents-disabled/GDELTMonitor.md` | Background monitoring, anomaly detection |

Both are flat `.md` files with YAML frontmatter (model: sonnet, permissions for Bash/Read/Write/Grep).

## Execution Plan

Use **Sonnet agents in parallel** to build everything:

1. **Agent A** — Build 6 slash commands (`~/.claude/commands/gdelt-*.md`)
2. **Agent B** — Build 2 skills (`~/.claude/skills-disabled/GdeltBrief/`, `GdeltOsint/`)
3. **Agent C** — Build 2 agents (`~/.claude/agents-disabled/GDELT*.md`)
4. **Agent D** — File organization: move files, update `r_packages/CLAUDE.md` convention, clean up `Plans/`

All agents use the R execution pattern from CLAUDE.md:
```bash
Rscript -e "
  library(gdeltr2)
  result <- ft_v2_api(term = 'QUERY', mode = 'ArtList', maximum_records = 25, return_message = FALSE)
  jsonlite::write_json(result, '/tmp/gdelt_result.json')
"
```

Key constraints encoded in all commands/skills:
- V2 API: 250 result max, ~45 day lookback
- Context API: 72hr window, 200 result max
- TV API: station codes (CNN, FOXNEWS, MSNBC, BBCNEWS)
- All functions: `return_message = FALSE` for programmatic use
- Rate limiting: 6-second sleep between sequential queries

## Verification

- Each slash command: invoke `/gdelt-search "test"` and confirm R execution + formatted output
- Skills: verify `SKILL.md` has valid frontmatter with `USE WHEN` triggers
- Agents: verify `.md` files have valid YAML frontmatter with permissions
- File organization: confirm `Plans/` artifacts moved to `.claude/plans/`, `r_packages/CLAUDE.md` updated
- Activation: confirm `pai-skill enable GdeltBrief` creates symlink correctly
