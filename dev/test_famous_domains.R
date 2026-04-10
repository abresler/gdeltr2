#!/usr/bin/env Rscript
# gdeltr2 Famous Domain API Test Suite
# Tests all major GDELT APIs with well-known news domains

library(cli)

cli_h1("gdeltr2 Famous Domain API Test Suite")
cli_text("Loading package...")
suppressPackageStartupMessages(devtools::load_all(quiet = TRUE))

results <- list()
SLEEP <- 4 # seconds between API calls for rate limiting

# ── Famous domains to test ──
famous_domains <- c(
  "nytimes.com",
  "bbc.com",
  "reuters.com",
  "washingtonpost.com",
  "theguardian.com",
  "cnn.com",
  "foxnews.com",
  "aljazeera.com",
  "wsj.com",
  "apnews.com"
)

record_result <- function(name, rows, status = "ok") {
  results[[name]] <<- list(rows = rows, status = status)
}

record_error <- function(name, e) {
  results[[name]] <<- list(rows = 0, status = paste("error:", e$message))
}

# ═══════════════════════════════════════════════════════════════
# TEST 1: ft_v2_api ArtList — one domain at a time
# ═══════════════════════════════════════════════════════════════
cli_h2("TEST 1: ft_v2_api ArtList per domain")

for (dom in famous_domains) {
  cli_alert_info("Testing domain: {dom}")
  tryCatch({
    out <- ft_v2_api(
      domains = dom,
      modes = "ArtList",
      timespans = "24 hours",
      maximum_records = 5,
      visualize_results = FALSE,
      return_message = FALSE
    )
    n <- nrow(out)
    if (n > 0) {
      cli_alert_success("{dom}: {n} rows")
    } else {
      cli_alert_warning("{dom}: 0 rows (empty)")
    }
    record_result(paste0("ft_artlist_", dom), n)
  }, error = function(e) {
    cli_alert_danger("{dom}: ERROR - {e$message}")
    record_error(paste0("ft_artlist_", dom), e)
  })
  Sys.sleep(SLEEP)
}

# ═══════════════════════════════════════════════════════════════
# TEST 2: ft_v2_api TimelineVol — batch of 3 domains
# ═══════════════════════════════════════════════════════════════
cli_h2("TEST 2: ft_v2_api TimelineVol batch")

tryCatch({
  out <- ft_v2_api(
    domains = c("nytimes.com", "bbc.com", "reuters.com"),
    modes = "TimelineVol",
    timespans = "1 week",
    maximum_records = 250,
    visualize_results = FALSE,
    return_message = FALSE
  )
  cli_alert_success("TimelineVol batch: {nrow(out)} rows")
  record_result("ft_timelinevol_batch", nrow(out))
}, error = function(e) {
  cli_alert_danger("TimelineVol batch: ERROR - {e$message}")
  record_error("ft_timelinevol_batch", e)
})

Sys.sleep(SLEEP)

# ═══════════════════════════════════════════════════════════════
# TEST 3: ft_v2_api TimelineTone — foxnews vs cnn
# ═══════════════════════════════════════════════════════════════
cli_h2("TEST 3: ft_v2_api TimelineTone")

tryCatch({
  out <- ft_v2_api(
    domains = c("foxnews.com", "cnn.com"),
    modes = "TimelineTone",
    timespans = "1 week",
    maximum_records = 250,
    visualize_results = FALSE,
    return_message = FALSE
  )
  cli_alert_success("TimelineTone fox/cnn: {nrow(out)} rows")
  record_result("ft_timelinetone", nrow(out))
}, error = function(e) {
  cli_alert_danger("TimelineTone: ERROR - {e$message}")
  record_error("ft_timelinetone", e)
})

Sys.sleep(SLEEP)

# ═══════════════════════════════════════════════════════════════
# TEST 4: ft_v2_api ToneChart — wsj.com
# ═══════════════════════════════════════════════════════════════
cli_h2("TEST 4: ft_v2_api ToneChart wsj.com")

tryCatch({
  out <- ft_v2_api(
    domains = "wsj.com",
    modes = "ToneChart",
    timespans = "1 week",
    maximum_records = 250,
    visualize_results = FALSE,
    return_message = FALSE
  )
  cli_alert_success("ToneChart wsj: {nrow(out)} rows")
  record_result("ft_tonechart_wsj", nrow(out))
}, error = function(e) {
  cli_alert_danger("ToneChart wsj: ERROR - {e$message}")
  record_error("ft_tonechart_wsj", e)
})

Sys.sleep(SLEEP)

# ═══════════════════════════════════════════════════════════════
# TEST 5: ft_v2_api TimelineSourceCountry — aljazeera
# ═══════════════════════════════════════════════════════════════
cli_h2("TEST 5: ft_v2_api TimelineSourceCountry aljazeera.com")

tryCatch({
  out <- ft_v2_api(
    domains = "aljazeera.com",
    modes = "TimelineSourceCountry",
    timespans = "1 week",
    maximum_records = 250,
    visualize_results = FALSE,
    return_message = FALSE
  )
  cli_alert_success("TimelineSourceCountry aljazeera: {nrow(out)} rows")
  record_result("ft_timelinesrccountry_aljazeera", nrow(out))
}, error = function(e) {
  cli_alert_danger("TimelineSourceCountry aljazeera: ERROR - {e$message}")
  record_error("ft_timelinesrccountry_aljazeera", e)
})

Sys.sleep(SLEEP)

# ═══════════════════════════════════════════════════════════════
# TEST 6: context_v2_api — domain filter
# ═══════════════════════════════════════════════════════════════
cli_h2("TEST 6: context_v2_api domain filter")

context_domains <- c("nytimes.com", "bbc.com", "reuters.com", "cnn.com", "apnews.com")

for (dom in context_domains) {
  cli_alert_info("Context API domain: {dom}")
  tryCatch({
    out <- context_v2_api(
      domain = dom,
      timespan = "72H",
      maximum_records = 10,
      return_message = FALSE
    )
    n <- nrow(out)
    if (n > 0) {
      cli_alert_success("{dom}: {n} context rows")
    } else {
      cli_alert_warning("{dom}: 0 context rows")
    }
    record_result(paste0("context_", dom), n)
  }, error = function(e) {
    cli_alert_danger("{dom}: ERROR - {e$message}")
    record_error(paste0("context_", dom), e)
  })
  Sys.sleep(SLEEP)
}

# ═══════════════════════════════════════════════════════════════
# TEST 7: context_v2_api — domain_exact filter
# ═══════════════════════════════════════════════════════════════
cli_h2("TEST 7: context_v2_api domain_exact filter")

tryCatch({
  out <- context_v2_api(
    domain_exact = "reuters.com",
    timespan = "48H",
    maximum_records = 10,
    return_message = FALSE
  )
  cli_alert_success("context domain_exact reuters.com: {nrow(out)} rows")
  record_result("context_exact_reuters", nrow(out))
}, error = function(e) {
  cli_alert_danger("context domain_exact reuters: ERROR - {e$message}")
  record_error("context_exact_reuters", e)
})

Sys.sleep(SLEEP)

# ═══════════════════════════════════════════════════════════════
# TEST 8: ft_v2_api — term + domain combos
# ═══════════════════════════════════════════════════════════════
cli_h2("TEST 8: ft_v2_api term + domain combos")

combos <- list(
  list(term = "economy", domain = "nytimes.com"),
  list(term = "climate", domain = "theguardian.com"),
  list(term = "technology", domain = "wsj.com"),
  list(term = "election", domain = "washingtonpost.com"),
  list(term = "war", domain = "bbc.com")
)

for (combo in combos) {
  cli_alert_info("term={combo$term} + domain={combo$domain}")
  tryCatch({
    out <- ft_v2_api(
      terms = combo$term,
      domains = combo$domain,
      modes = "ArtList",
      timespans = "1 week",
      maximum_records = 5,
      visualize_results = FALSE,
      return_message = FALSE
    )
    n <- nrow(out)
    if (n > 0) {
      cli_alert_success("{combo$term}@{combo$domain}: {n} rows")
    } else {
      cli_alert_warning("{combo$term}@{combo$domain}: 0 rows")
    }
    record_result(paste0("ft_combo_", combo$term, "_", combo$domain), n)
  }, error = function(e) {
    cli_alert_danger("{combo$term}@{combo$domain}: ERROR - {e$message}")
    record_error(paste0("ft_combo_", combo$term, "_", combo$domain), e)
  })
  Sys.sleep(SLEEP)
}

# ═══════════════════════════════════════════════════════════════
# TEST 9: v2_tv_api — Famous TV stations
# ═══════════════════════════════════════════════════════════════
cli_h2("TEST 9: v2_tv_api Famous TV stations")

tv_stations <- c("CNN", "FOXNEWS", "MSNBC", "BBCNEWS")

for (stn in tv_stations) {
  cli_alert_info("Testing TV station: {stn}")
  tryCatch({
    out <- v2_tv_api(
      terms = "economy",
      stations = stn,
      modes = "TimelineVol",
      timespan = "14d",
      return_message = FALSE
    )
    n <- nrow(out)
    if (n > 0) {
      cli_alert_success("{stn}: {n} rows")
    } else {
      cli_alert_warning("{stn}: 0 rows")
    }
    record_result(paste0("tv_", stn), n)
  }, error = function(e) {
    cli_alert_danger("{stn}: ERROR - {e$message}")
    record_error(paste0("tv_", stn), e)
  })
  Sys.sleep(SLEEP)
}

# ═══════════════════════════════════════════════════════════════
# TEST 10: v2_tv_api — ClipGallery mode
# ═══════════════════════════════════════════════════════════════
cli_h2("TEST 10: v2_tv_api ClipGallery")

tryCatch({
  out <- v2_tv_api(
    terms = "artificial intelligence",
    stations = c("CNN", "FOXNEWS"),
    modes = "ClipGallery",
    timespan = "7d",
    return_message = FALSE
  )
  cli_alert_success("ClipGallery AI: {nrow(out)} rows")
  record_result("tv_clipgallery", nrow(out))
}, error = function(e) {
  cli_alert_danger("ClipGallery: ERROR - {e$message}")
  record_error("tv_clipgallery", e)
})

# ═══════════════════════════════════════════════════════════════
# SUMMARY
# ═══════════════════════════════════════════════════════════════
cli_h1("TEST RESULTS SUMMARY")

total <- length(results)
ok <- sum(sapply(results, function(x) x$status == "ok"))
has_data <- sum(sapply(results, function(x) x$status == "ok" & x$rows > 0))
errors <- sum(sapply(results, function(x) grepl("error", x$status)))

cli_alert_success("Total tests: {total}")
cli_alert_success("Passed (no error): {ok}/{total}")
cli_alert_success("Returned data: {has_data}/{total}")

if (errors > 0) {
  cli_alert_danger("Errors: {errors}/{total}")
  for (nm in names(results)) {
    r <- results[[nm]]
    if (grepl("error", r$status)) {
      cli_alert_danger("  FAIL {nm}: {r$status}")
    }
  }
}

zero_row <- names(results)[sapply(results, function(x) x$status == "ok" & x$rows == 0)]
if (length(zero_row) > 0) {
  cli_alert_warning("Zero-row (API returned empty): {paste(zero_row, collapse = ', ')}")
}

cli_h2("Per-test detail")
for (nm in names(results)) {
  r <- results[[nm]]
  if (r$status == "ok" && r$rows > 0) {
    cli_alert_success("{nm}: {r$rows} rows")
  } else if (r$status == "ok" && r$rows == 0) {
    cli_alert_warning("{nm}: 0 rows")
  } else {
    cli_alert_danger("{nm}: {r$status}")
  }
}
