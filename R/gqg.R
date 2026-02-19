# Global Quotation Graph (GQG) ------------------------------------------
#
# Quoted statements from worldwide news
# URL pattern: http://data.gdeltproject.org/gdeltv3/gqg/YYYYMMDDHHMMSS.gqg.json.gz
# Updates every minute, available since 2020-01-01
# UTF8 JSONNL (JSON newline-delimited), gzipped

# utils -------------------------------------------------------------------

.extract_gqg_time <- function(url) {
  parts <- url |> stringr::str_split("/") |> purrr::flatten_chr()
  time <- parts[length(parts)] |>
    stringr::str_remove_all("\\.gqg.json.gz") |>
    lubridate::ymd_hms() |>
    lubridate::with_tz(Sys.timezone())
  time
}

# dict --------------------------------------------------------------------

#' Global Quotation Graph URL dictionary
#'
#' List available GQG file URLs for a specified date range.
#' Updates every minute, available since 2020-01-01.
#'
#' @param start_date start date in YMD format, default Sys.Date()-1
#' @param end_date end date in YMD format, default Sys.Date()
#'
#' @return A tibble with columns: yearData, monthData, dateData, datetimeData, urlGQG
#' @export
#'
#' @examples
#' \dontrun{
#' dictionary_gqg_urls()
#' }
dictionary_gqg_urls <- function(start_date = Sys.Date() - 1,
                                 end_date = Sys.Date()) {
  tryCatch({
    start_date <- start_date |>
      as.character() |>
      readr::parse_date()

    end_date <- end_date |>
      as.character() |>
      readr::parse_date()

    if (start_date < lubridate::ymd("2020-01-01")) {
      cli::cli_alert_warning("GQG data starts 2020-01-01")
    }

    # Generate timestamps for every minute in the range
    seq_times <- seq(
      from = as.POSIXct(paste(start_date, "00:00:00"), tz = Sys.timezone()),
      to = as.POSIXct(paste(end_date, "23:59:00"), tz = Sys.timezone()),
      by = "1 min"
    )

    data <- tibble::tibble(
      datetimeData = seq_times,
      dateData = as.Date(datetimeData),
      yearData = lubridate::year(dateData) |> as.numeric(),
      monthData = lubridate::month(dateData) |> as.numeric(),
      isoPeriod = datetimeData |> format("%Y%m%d%H%M%S"),
      urlGQG = glue::glue("http://data.gdeltproject.org/gdeltv3/gqg/{isoPeriod}.gqg.json.gz")
    ) |>
      dplyr::select(yearData, monthData, dateData, datetimeData, urlGQG)

    # Cache the result
    .cache_set("dictionary_gqg_urls", data)
    data
  }, error = function(e) {
    cli::cli_alert_danger("Failed: {e$message}")
    return(tibble::tibble())
  })
}

# parse -------------------------------------------------------------------

.parse_gqg_json_url <- function(url = "http://data.gdeltproject.org/gdeltv3/gqg/20200101000000.gqg.json.gz",
                                return_message = TRUE) {
  time <- .extract_gqg_time(url = url)

  if (return_message) {
    cli::cli_alert_info("Acquiring GQG data for {time}")
  }

  data <- tryCatch({
    curl::curl(url) |>
      gzcon() |>
      jsonlite::stream_in() |>
      tibble::as_tibble()
  }, error = function(e) {
    cli::cli_alert_warning("Failed to read {url}: {e$message}")
    return(tibble::tibble())
  })

  if (nrow(data) == 0) {
    return(tibble::tibble())
  }

  data <- data |>
    rlang::set_names(
      c(
        "datetimeArticle",
        "languageArticle",
        "urlArticle",
        "dataQuotations"
      )
    ) |>
    dplyr::mutate(
      datetimeArticle = lubridate::ymd_hms(datetimeArticle) |>
        lubridate::with_tz(Sys.timezone()),
      dateArticle = as.Date(datetimeArticle),
      yearArticle = lubridate::year(dateArticle) |> as.numeric(),
      monthArticle = lubridate::month(dateArticle) |> as.numeric(),
      domainArticle = urlArticle |> urltools::domain(),
      urlAPI = url
    ) |>
    dplyr::select(
      yearArticle,
      monthArticle,
      dateArticle,
      datetimeArticle,
      domainArticle,
      languageArticle,
      urlArticle,
      dataQuotations,
      urlAPI
    )

  data
}

.parse_gqg_json_url_m <- function(url, return_message = TRUE) {
  .parse_gqg_json_url_safe <- purrr::possibly(.parse_gqg_json_url, tibble::tibble())
  .tt <- memoise::memoise(.parse_gqg_json_url_safe)
  .tt(url = url, return_message = return_message)
}

#' Parse GQG URLs
#'
#' Parse vector of GQG URLs
#'
#' @param urls vector of GQG URLs
#' @param return_message if TRUE returns message
#' @param use_parallel if TRUE uses parallel processing
#' @param workers number of workers for parallel processing
#'
#' @return A tibble of GQG data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_gqg_urls()
#' }
parse_gqg_urls <- function(urls = c(
  "http://data.gdeltproject.org/gdeltv3/gqg/20200101000000.gqg.json.gz"
),
return_message = TRUE,
use_parallel = FALSE,
workers = NULL) {
  tryCatch({
    .parse_gqg_json_url_m_safe <-
      purrr::possibly(.parse_gqg_json_url_m, tibble::tibble())

    .safe_map_dfr(
      urls,
      function(url) {
        .parse_gqg_json_url_m_safe(url = url, return_message = return_message)
      },
      use_parallel = use_parallel,
      workers = workers
    )
  }, error = function(e) {
    cli::cli_alert_danger("Failed: {e$message}")
    return(tibble::tibble())
  })
}

#' Global Quotation Graph data
#'
#' Acquires quoted statements from worldwide news via the GDELT Global
#' Quotation Graph (GQG) for a specified date range.
#'
#' @param start_date start date in YMD format, default Sys.Date()-1
#' @param end_date end date in YMD format, default Sys.Date()
#' @param return_message if TRUE returns message
#' @param use_parallel if TRUE uses parallel processing
#' @param workers number of workers for parallel processing
#'
#' @return A tibble of GQG data with quoted statements
#' @export
#'
#' @examples
#' \dontrun{
#' get_data_gqg()
#' }
get_data_gqg <- function(start_date = Sys.Date() - 1,
                         end_date = Sys.Date(),
                         return_message = TRUE,
                         use_parallel = FALSE,
                         workers = NULL) {
  tryCatch({
    if (length(start_date) == 0) {
      stop("Enter start date")
    }

    start_date <- start_date |>
      as.character() |>
      readr::parse_date()

    if (start_date < lubridate::ymd("2020-01-01")) {
      cli::cli_alert_warning("GQG data starts 2020-01-01")
    }

    df_urls <- dictionary_gqg_urls(start_date = start_date, end_date = end_date)

    df_urls <- df_urls |>
      dplyr::filter(dateData >= start_date)

    if (length(end_date) > 0) {
      end_date <- end_date |>
        as.character() |>
        readr::parse_date()
      df_urls <- df_urls |>
        dplyr::filter(dateData <= end_date)
      end_slug <- glue::glue(" to {end_date}")
    } else {
      end_slug <- ""
    }

    if (return_message) {
      cli::cli_alert_info("Acquiring GQG data for {start_date}{end_slug}")
    }

    data <- parse_gqg_urls(
      urls = df_urls$urlGQG,
      return_message = return_message,
      use_parallel = use_parallel,
      workers = workers
    )

    data
  }, error = function(e) {
    cli::cli_alert_danger("Failed: {e$message}")
    return(tibble::tibble())
  })
}
