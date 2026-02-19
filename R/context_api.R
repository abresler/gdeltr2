# https://blog.gdeltproject.org/gdelt-context-2-0-api-debuts/


# utils -------------------------------------------------------------------

#' V2 Context API Modes
#'
#' @return A character vector of available V2 Context API mode names (only "ArtList")
#' @export
#'
#' @examples
#' \dontrun{
#' dictionary_v2_context_api_modes()
#' }
dictionary_v2_context_api_modes <-
  function() {
    modes <- c("ArtList")
    modes
  }


# context -----------------------------------------------------------------

# https://blog.gdeltproject.org/gdelt-context-2-0-api-debuts/
.generate_v2_context_url <-
  function(term = NULL,
           domain = NULL,
           domain_exact = NULL,
           source_language = "english",
           source_country = NULL,
           theme_gkg = NULL,
           near_term = NULL,
           near_length = 20,
           is_quote = NULL,
           search_language = NULL,
           timespan = "72H",
           maximum_records = 75,
           sort_variable = "DateDesc",
           format = "json",
           return_message = FALSE) {
    base_url <- "https://api.gdeltproject.org/api/v2/context/context?query="

    ## Query term
    if (length(term) > 0) {
      term_slug <- collapse_terms(terms = term, encode = TRUE)

      if (length(term) > 1) {
        term_slug <- glue::glue("({term_slug})")
      }
    } else {
      term_slug <- NULL
    }

    ## Domain
    if (length(domain) > 0) {
      domain_slug <-
        generate_gdelt_api_parameters_values(parameter = "domain", values = domain)
    } else {
      domain_slug <- NULL
    }

    ## Domain exact
    if (length(domain_exact) > 0) {
      domain_exact_slug <-
        generate_gdelt_api_parameters_values(parameter = "domainis", values = domain_exact)
    } else {
      domain_exact_slug <- NULL
    }

    ## Source language
    if (length(source_language) > 0) {
      source_language_slug <-
        generate_gdelt_api_parameters_values(parameter = "sourcelang", values = source_language)
    } else {
      source_language_slug <- NULL
    }

    ## Source country
    if (length(source_country) > 0) {
      source_clean <- source_country |> str_remove_all("\\ |\\+\\|-") |> str_to_lower()
      source_country_slug <-
        generate_gdelt_api_parameters_values(parameter = "sourcecountry", values = source_clean)
    } else {
      source_country_slug <- NULL
    }

    ## GKG theme
    if (length(theme_gkg) > 0) {
      theme_gkg_slug <-
        generate_gdelt_api_parameters_values(parameter = "theme", values = theme_gkg)
    } else {
      theme_gkg_slug <- NULL
    }

    ## Near term
    if (length(near_term) > 0) {
      near_param <- glue::glue("near{near_length}")
      near_term_quoted <- glue::glue("\"{near_term}\"")
      near_slug <-
        generate_gdelt_api_parameters_values(parameter = near_param, values = near_term_quoted)
    } else {
      near_slug <- NULL
    }

    ## Assemble query portion
    query_url <-
      str_c(
        base_url,
        term_slug,
        domain_slug,
        domain_exact_slug,
        source_language_slug,
        source_country_slug,
        theme_gkg_slug,
        near_slug,
        sep = "%20%"
      )

    query_params <-
      query_url |>
      str_remove("^https://api.gdeltproject.org/api/v2/context/context\\?query=") |>
      url_decode() |>
      str_remove_all("\\%") |>
      str_squish() |>
      str_split("\\&") |>
      flatten_chr()

    query_params <- query_params[[1]] |> str_replace_all("\\ ", "\\ - ")

    ## Mode (always ArtList for Context API)
    mode_slug <- "mode=ArtList"

    ## Format
    format_slug <- str_c("format=", format)

    ## Max records (cap at 200)
    maximum_records <- min(200, maximum_records)
    max_records_slug <- str_c("maxrecords=", maximum_records)

    ## Sort
    if (length(sort_variable) > 0) {
      sort_slug <- str_c("sort=", sort_variable)
    } else {
      sort_slug <- NULL
    }

    ## Timespan (cap at 72H)
    if (length(timespan) > 0) {
      timespan <- str_remove_all(timespan, "\\-|\\ ")
      timespan_slug <- str_c("timespan=", timespan)
    } else {
      timespan_slug <- NULL
    }

    ## isquote parameter
    if (!is.null(is_quote)) {
      is_quote_val <- if (isTRUE(is_quote)) 1L else 0L
      is_quote_slug <- str_c("isquote=", is_quote_val)
    } else {
      is_quote_slug <- NULL
    }

    ## searchlang parameter
    if (length(search_language) > 0) {
      search_language_slug <- str_c("searchlang=", search_language)
    } else {
      search_language_slug <- NULL
    }

    ## Assemble full URL
    url_gdelt_v2_context_api <- str_c(
      query_url,
      mode_slug,
      format_slug,
      max_records_slug,
      sort_slug,
      timespan_slug,
      is_quote_slug,
      search_language_slug,
      sep = "&"
    )

    if (return_message) {
      cli::cli_text("V2 Context API URL: {.url {url_gdelt_v2_context_api}}\n\n")
    }

    term_label <- glue::glue("{term |> str_c(collapse = '|')}") |> as.character()
    if (term_label == "") {
      term_label <- NULL
    }

    tibble::tibble(
      term = term_label,
      domain,
      domain_exact,
      source_language,
      source_country,
      theme_gkg,
      near_term,
      timespan,
      is_quote,
      search_language,
      format,
      query_params,
      url_gdelt_v2_context_api
    )
  }


.parse_v2_context_api_url <-
  function(url,
           return_message = TRUE,
           nest_data = FALSE) {
    url <- str_to_lower(url)

    if (return_message) {
      cli::cli_text("Parsing: {.url {url}}\n\n")
    }

    is_json <- url |> str_detect("format=json")
    is_csv  <- url |> str_detect("format=csv")

    if (is_json) {
      .read_gdelt_api_json_safe <-
        purrr::possibly(.read_gdelt_api_json, tibble::tibble())
      data <- .read_gdelt_api_json_safe(url, url_type = "url_gdelt_v2_context_api")
    }

    if (is_csv) {
      .read_gdelt_api_csv_safe <-
        purrr::possibly(.read_gdelt_api_csv, tibble::tibble())
      data <- .read_gdelt_api_csv_safe(url, url_type = "url_gdelt_v2_context_api")
    }

    if (data |> hasName("url")) {
      data <- data |> rename(url_article = url)
    }

    if (data |> hasName("url_mobile_link")) {
      data <- data |>
        mutate(url_mobile_link = case_when(
          url_mobile_link == "" ~ NA_character_,
          TRUE ~ url_mobile_link
        ))
    }

    if (data |> hasName("title_article")) {
      data <- data |>
        mutate(title_article = title_article |> str_squish())
    }

    if (data |> hasName("datetime_article")) {
      data <- data |>
        mutate(
          datetime_article = datetime_article |>
            lubridate::ymd_hms() |>
            lubridate::with_tz(Sys.timezone())
        )
    }

    data <- data |> mutate(url_gdelt_v2_context_api = url |> str_to_lower())

    if (nest_data) {
      data <- data |>
        nest(data_v2_context_api = -url_gdelt_v2_context_api)
    }

    data
  }


#' Parse V2 Context API URLs
#'
#' @param urls Vector of V2 Context API URLs to parse
#' @param nest_data If `TRUE` nests returned data per URL
#' @param return_message If `TRUE` prints progress messages
#' @param use_parallel If `TRUE` uses parallel processing
#' @param workers Number of parallel workers
#'
#' @return A `tibble` of parsed V2 Context API response data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_v2_context_api_urls(urls = "https://api.gdeltproject.org/api/v2/context/context?query=climate&mode=ArtList&format=json&maxrecords=75&timespan=24H")
#' }
parse_v2_context_api_urls <-
  function(urls = NULL,
           nest_data = FALSE,
           return_message = TRUE,
           use_parallel = FALSE,
           workers = NULL) {
    if (length(urls) == 0) {
      stop("Enter v2 Context API urls")
    }

    .parse_v2_context_api_url_safe <-
      purrr::possibly(.parse_v2_context_api_url, tibble::tibble())

    .safe_map_dfr(
      urls,
      function(url) {
        .parse_v2_context_api_url_safe(
          url = url,
          return_message = return_message,
          nest_data = nest_data
        )
      },
      use_parallel = use_parallel,
      workers = workers
    )
  }


#' GDELT V2 Context API
#'
#' Interact with the GDELT V2 Context API. Unlike the Document API which searches
#' full articles, the Context API searches individual sentences and returns the
#' matching sentence plus surrounding context. Only covers the past 72 hours.
#'
#' @param term Character vector of search terms. Supports keyword and keyphrase
#'   searches and OR statements.
#' @param domain Character vector of domains to filter results by (e.g., "cnn.com").
#' @param domain_exact Character vector of exact domains to match.
#' @param source_language Source language filter (default `"english"`). Use the
#'   language name or its three-character code.
#' @param source_country Character vector of source countries. For multi-word
#'   country names omit the spaces (e.g., `"unitedstates"`). Accepts 2-character
#'   FIPS codes.
#' @param theme_gkg Character vector of GDELT GKG themes to filter by (e.g.,
#'   `"TERROR"`).
#' @param near_term Character string specifying words that must appear within
#'   `near_length` words of each other.
#' @param near_length Integer proximity window for `near_term` (default `20`).
#' @param is_quote Logical. If `TRUE` returns only quoted sentences; if `FALSE`
#'   returns only non-quoted sentences. `NULL` (default) applies no filter.
#' @param search_language Language to search within (e.g., `"english"`). Controls
#'   the language of the sentence text searched, separate from `source_language`.
#' @param timespan Timespan to search backwards from now (default `"72H"`). Maximum
#'   is 72 hours. Accepts formats like `"24H"`, `"1440M"`, `"3D"`.
#' @param maximum_records Maximum number of sentence records to return (default
#'   `75`, maximum `200`).
#' @param sort_variable Sort order for results (default `"DateDesc"`).
#'   \itemize{
#'     \item `DateDesc` Most recent sentences first.
#'     \item `DateAsc` Oldest sentences first.
#'     \item `ToneDesc` Most positive tone first.
#'     \item `ToneAsc` Most negative tone first.
#'     \item `HybridRel` Relevance-weighted sort (default for textual searches).
#'   }
#' @param format Output format (default `"json"`). Supports `"json"` and `"csv"`.
#' @param parse_data If `TRUE` fetches and parses the API response (default `TRUE`).
#'   Set to `FALSE` to return the URL tibble only.
#' @param nest_data If `TRUE` nests parsed response data inside the URL tibble.
#' @param return_message If `TRUE` prints progress and URL messages via `cli`
#'   (default `TRUE`).
#' @param use_parallel If `TRUE` uses parallel processing for multiple URLs.
#' @param workers Number of parallel workers when `use_parallel = TRUE`.
#'
#' @return A `tibble` of V2 Context API sentence-level results. Each row represents
#'   one matching sentence with its surrounding context snippet and article metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' # Search for sentences mentioning climate change in the last 24 hours
#' context_v2_api(term = "climate change", timespan = "24H")
#'
#' # Retrieve only quoted sentences about a topic
#' context_v2_api(term = "Federal Reserve", is_quote = TRUE, maximum_records = 100)
#'
#' # Filter by domain
#' context_v2_api(term = "artificial intelligence", domain = "reuters.com")
#' }
context_v2_api <-
  function(term = NULL,
           domain = NULL,
           domain_exact = NULL,
           source_language = "english",
           source_country = NULL,
           theme_gkg = NULL,
           near_term = NULL,
           near_length = 20,
           is_quote = NULL,
           search_language = NULL,
           timespan = "72H",
           maximum_records = 75,
           sort_variable = "DateDesc",
           format = "json",
           parse_data = TRUE,
           nest_data = FALSE,
           return_message = TRUE,
           use_parallel = FALSE,
           workers = NULL) {
    tryCatch({
      if (length(term) == 0 &&
          length(domain) == 0 &&
          length(domain_exact) == 0 &&
          length(theme_gkg) == 0 &&
          length(near_term) == 0) {
        stop("Enter at least one search parameter: term, domain, domain_exact, theme_gkg, or near_term")
      }

      ## Cap timespan at 72H logic — pass through as-is; API enforces server-side
      ## Cap maximum_records at 200
      maximum_records <- min(200L, as.integer(maximum_records))

      all_data <- tibble::tibble()

      ## Build one URL per term (or one URL if no terms, using other filters)
      query_terms <- if (length(term) > 0) term else list(NULL)

      dat_urls <-
        seq_along(query_terms) |>
        purrr::map_dfr(function(i) {
          row_term <- query_terms[[i]]

          .generate_v2_context_url(
            term           = row_term,
            domain         = domain,
            domain_exact   = domain_exact,
            source_language = source_language,
            source_country = source_country,
            theme_gkg      = theme_gkg,
            near_term      = near_term,
            near_length    = near_length,
            is_quote       = is_quote,
            search_language = search_language,
            timespan       = timespan,
            maximum_records = maximum_records,
            sort_variable  = sort_variable,
            format         = format,
            return_message = return_message
          )
        })

      all_data <- all_data |> bind_rows(dat_urls)

      if (!parse_data) {
        return(all_data)
      }

      out <-
        all_data$url_gdelt_v2_context_api |>
        parse_v2_context_api_urls(
          return_message = return_message,
          nest_data      = TRUE,
          use_parallel   = use_parallel,
          workers        = workers
        )

      all_data <-
        all_data |>
        mutate(url_gdelt_v2_context_api = str_to_lower(url_gdelt_v2_context_api))

      if (nrow(out) > 0 && "url_gdelt_v2_context_api" %in% names(out)) {
        all_data <- all_data |> left_join(out, by = "url_gdelt_v2_context_api")
      }

      if (!nest_data) {
        all_data <- all_data |> unnest(cols = everything())
      }

      all_data
    }, error = function(e) {
      cli::cli_alert_danger("Failed: {e$message}")
      return(tibble::tibble())
    })
  }
