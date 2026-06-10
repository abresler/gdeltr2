#' @keywords internal
#'
#' @import dplyr purrr stringr tibble tidyr glue readr utils
#' @importFrom rlang sym syms is_null set_names `:=` `!!` `!!!` .data inform warn abort
#'
#' @importFrom cli cli_alert_info cli_alert_danger cli_alert_success cli_alert_warning cli_text cli_inform col_red col_green col_blue
#' @importFrom curl curl curl_download
#' @importFrom data.table fread rbindlist
#' @importFrom glue glue
#' @importFrom grDevices colors
#' @importFrom httr GET content url_ok handle_reset timeout status_code
#' @importFrom jsonlite fromJSON stream_in
#' @importFrom lubridate ymd ymd_hms with_tz my mdy mdy_hm mdy_hms year month wday floor_date hours
#' @importFrom memoise memoise
#' @importFrom rvest html_nodes html_text html_attr read_html
#' @importFrom xml2 read_xml xml_children xml_name xml_text
#' @importFrom urltools domain url_decode
#' @importFrom anytime anytime anydate
#' @importFrom highcharter hchart hcaes hc_add_theme hc_theme_gridlight hc_theme_elementary hc_xAxis hc_yAxis hc_legend hc_credits hc_exporting hc_chart highchart
#' @importFrom ggplot2 ggplot aes geom_line scale_y_continuous facet_wrap scale_x_date labs
#' @importFrom ggthemes theme_fivethirtyeight
#' @importFrom trelliscopejs trelliscope img_panel cog_href
#' @importFrom wordcloud2 wordcloud2
#' @importFrom janitor clean_names make_clean_names
#' @importFrom readr parse_number read_csv read_tsv read_lines parse_date
#' @importFrom tibble rownames_to_column as_tibble tibble
#' @importFrom tidyr separate separate_longer_delim pivot_longer pivot_wider unite unnest
"_PACKAGE"

# --- Package-level cache environment ---
# Used instead of assign() to .GlobalEnv for caching URL dictionaries
.pkg_cache <- new.env(parent = emptyenv())

.cache_get <- function(key) {
  if (exists(key, envir = .pkg_cache)) {
    get(key, envir = .pkg_cache)
  } else {
    NULL
  }
}

.cache_set <- function(key, value) {
  assign(key, value, envir = .pkg_cache)
  invisible(value)
}

# --- Parallel Processing Infrastructure ---

#' Set up parallel processing
#'
#' Configures furrr/future for parallel execution with the specified number
#' of workers. If workers is NULL, uses half of available cores (minimum 2).
#'
#' @param workers Number of parallel workers. NULL for auto-detect.
#' @return Invisibly returns the number of workers configured.
#' @keywords internal
.setup_parallel <- function(workers = NULL) {
  if (is.null(workers)) {
    workers <- max(2L, parallel::detectCores() %/% 2L)
  }
  workers <- min(workers, parallel::detectCores())
  future::plan(future::multisession, workers = workers)
  cli::cli_alert_info("Parallel processing enabled with {workers} workers")
  invisible(workers)
}

#' Reset parallel processing to sequential
#'
#' @keywords internal
.reset_parallel <- function() {
  future::plan(future::sequential)
  invisible(NULL)
}

#' Parallel-aware map that returns a data frame
#'
#' Uses furrr::future_map_dfr when use_parallel is TRUE and there are
#' multiple items. Falls back to purrr::map_dfr for single items or
#' when parallel is disabled.
#'
#' @param .x A list or vector to iterate over.
#' @param .f A function to apply to each element.
#' @param use_parallel Logical. Whether to use parallel processing.
#' @param workers Number of parallel workers (NULL for auto-detect).
#' @param .progress Logical. Whether to show a progress bar.
#' @param ... Additional arguments passed to the mapping function.
#' @return A tibble (row-bound results).
#' @keywords internal
.safe_map_dfr <- function(.x, .f, use_parallel = FALSE, workers = NULL, .progress = TRUE, ...) {
  if (use_parallel && length(.x) > 1) {
    .setup_parallel(workers = workers)
    on.exit(.reset_parallel(), add = TRUE)
    result <- furrr::future_map_dfr(.x, .f, ..., .progress = .progress)
  } else {
    result <- purrr::map_dfr(.x, .f, ...)
  }
  result
}

#' Parallel-aware walk function
#'
#' Uses furrr::future_walk when use_parallel is TRUE.
#' Falls back to purrr::walk otherwise.
#'
#' @param .x A list or vector to iterate over.
#' @param .f A function to apply to each element.
#' @param use_parallel Logical. Whether to use parallel processing.
#' @param workers Number of parallel workers (NULL for auto-detect).
#' @param ... Additional arguments passed to the walk function.
#' @keywords internal
.safe_walk <- function(.x, .f, use_parallel = FALSE, workers = NULL, ...) {
  if (use_parallel && length(.x) > 1) {
    .setup_parallel(workers = workers)
    on.exit(.reset_parallel(), add = TRUE)
    furrr::future_walk(.x, .f, ...)
  } else {
    purrr::walk(.x, .f, ...)
  }
  invisible(.x)
}

# --- HTTP Retry Infrastructure ---

#' Safe HTTP GET with retry logic
#'
#' Wraps httr::GET with automatic retry on transient failures (5xx, timeouts).
#' Returns NULL on persistent failure instead of throwing an error.
#'
#' @param url Character string. URL to fetch.
#' @param max_retries Integer. Maximum number of retry attempts.
#' @param backoff_base Numeric. Base seconds for exponential backoff.
#' @param timeout_seconds Numeric. Request timeout in seconds.
#' @return An httr response object, or NULL on failure.
#' @keywords internal
.safe_http_get <- function(url, max_retries = 3L, backoff_base = 1, timeout_seconds = 60) {
  if (!is.character(url) || nchar(url) == 0) {
    cli::cli_alert_danger("Invalid URL provided")
    return(NULL)
  }

  for (attempt in seq_len(max_retries)) {
    resp <- tryCatch(
      httr::GET(url, httr::timeout(timeout_seconds)),
      error = function(e) {
        cli::cli_alert_warning("HTTP request failed (attempt {attempt}/{max_retries}): {e$message}")
        NULL
      }
    )

    if (!is.null(resp)) {
      status <- httr::status_code(resp)
      if (status < 400) {
        return(resp)
      }
      if (status == 429 && attempt < max_retries) {
        wait_time <- max(5, backoff_base * (2 ^ (attempt - 1)))
        # GDELT free tier is a shared global quota; honor Retry-After when sent (capped at 120s)
        retry_after <- suppressWarnings(as.numeric(httr::headers(resp)[["retry-after"]]))
        if (length(retry_after) == 1 && !is.na(retry_after)) {
          wait_time <- max(wait_time, min(retry_after, 120))
        }
        cli::cli_alert_warning("Rate limited (429), retrying in {wait_time}s (attempt {attempt}/{max_retries})")
        Sys.sleep(wait_time)
        next
      }
      if (status >= 500 && attempt < max_retries) {
        wait_time <- backoff_base * (2 ^ (attempt - 1))
        cli::cli_alert_warning("Server error {status}, retrying in {wait_time}s (attempt {attempt}/{max_retries})")
        Sys.sleep(wait_time)
        next
      }
      if (status >= 400) {
        cli::cli_alert_danger("HTTP {status} for {url}")
        return(NULL)
      }
    }

    if (attempt < max_retries) {
      wait_time <- backoff_base * (2 ^ (attempt - 1))
      Sys.sleep(wait_time)
    }
  }

  cli::cli_alert_danger("All {max_retries} attempts failed for {url}")
  NULL
}

#' Validate a URL string
#'
#' @param url Character string to validate.
#' @return Logical. TRUE if url appears to be a valid HTTP(S) URL.
#' @keywords internal
.is_valid_url <- function(url) {
  is.character(url) && length(url) == 1 && grepl("^https?://", url)
}

# --- Connection Pooling ---

# Shared curl handle pool for GDELT API endpoints
# Reuses connections to the same GDELT hosts across multiple requests,
# eliminating TLS handshake overhead for sequential API calls.
.gdelt_curl_pool <- NULL

.get_curl_handle <- function(base_url = "https://api.gdeltproject.org") {
  if (is.null(.gdelt_curl_pool)) {
    .gdelt_curl_pool <<- curl::new_pool(total_con = 10, host_con = 6)
  }
  curl::new_handle(url = base_url)
}

# --- .onLoad Cache Warming ---

.onLoad <- function(libname, pkgname) {
  # Pre-warm the memoise cache for frequently-used codebooks in background
  # This eliminates cold-start latency on first API call
  if (requireNamespace("callr", quietly = TRUE) && interactive()) {
    tryCatch({
      callr::r_bg(
        function() {
          # Trigger memoise cache population for CAMEO codes
          tryCatch({
            suppressMessages({
              gdeltr2::get_codes_cameo_country()
              gdeltr2::get_codes_cameo_events()
              gdeltr2::get_codes_gkg_themes()
            })
          }, error = function(e) invisible(NULL))
        },
        package = TRUE,
        supervise = TRUE
      )
    }, error = function(e) {
      # Silently fail -- cache warming is best-effort
      invisible(NULL)
    })
  }
}
