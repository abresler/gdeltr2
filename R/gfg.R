# https://blog.gdeltproject.org/announcing-gdelt-global-frontpage-graph-gfg/

# utils -------------------------------------------------------------------

.build_gfg_url <-
  function(datetime) {
    datetime_fmt <- datetime |>
      format("%Y%m%d%H0000")
    glue(
      "http://data.gdeltproject.org/gdeltv3/gfg/alpha/{datetime_fmt}.LINKS.TXT.gz"
    ) |>
      as.character()
  }

.extract_gfg_datetime <-
  function(url) {
    url |>
      str_extract("[0-9]{14}") |>
      lubridate::ymd_hms() |>
      lubridate::with_tz(Sys.timezone())
  }

# dict --------------------------------------------------------------------

#' Global Frontpage Graph (GFG) URL Dictionary
#'
#' Generate list of available GFG file URLs for a date/time range.
#' GFG files are hourly snapshots of news homepage links, updated every hour
#' with files ready approximately 30 minutes after each hour.
#'
#' @param start_date start datetime (defaults to 24 hours ago)
#' @param end_date end datetime (defaults to current time)
#'
#' @return A `tibble` of GFG URLs with datetime metadata
#' @export
#'
#' @examples
#' \dontrun{
#' dictionary_gfg_urls(
#'   start_date = Sys.time() - lubridate::hours(2),
#'   end_date = Sys.time()
#' )
#' }
dictionary_gfg_urls <-
  function(start_date = Sys.time() - lubridate::hours(24),
           end_date = Sys.time()) {
    tryCatch({
      start_date <- as.POSIXct(start_date, tz = "UTC")
      end_date <- as.POSIXct(end_date, tz = "UTC")
      # If dates were passed without time, set start to midnight and end to 23:00
      if (format(start_date, "%H%M%S") == "000000" & format(end_date, "%H%M%S") == "000000") {
        end_date <- end_date + lubridate::hours(23)
      }

      # Generate hourly sequence
      hours_seq <- seq(
        from = start_date,
        to = end_date,
        by = "1 hour"
      )

      data <- tibble(
        datetimeData = hours_seq,
        dateData = as.Date(hours_seq)
      ) |>
        mutate(
          urlGFG = datetimeData |> .build_gfg_url()
        ) |>
        select(dateData, datetimeData, urlGFG)

      data
    }, error = function(e) {
      cli::cli_alert_danger("Failed: {e$message}")
      return(tibble())
    })
  }

# parse -------------------------------------------------------------------

.parse_gfg_url <-
  function(url = "http://data.gdeltproject.org/gdeltv3/gfg/alpha/2021010100.LINKS.TXT.gz",
           return_message = TRUE) {
    datetime <- .extract_gfg_datetime(url = url)

    if (return_message) {
      cli::cli_alert_info("Acquiring GFG data for {datetime}")
    }

    data <-
      data.table::fread(
        url,
        sep = "\t",
        header = FALSE,
        col.names = c(
          "datetimeSnapshot",
          "urlFrontpage",
          "idLink",
          "pctLinkPosition",
          "urlDestination",
          "textLink"
        )
      ) |>
      as_tibble() |>
      mutate(
        datetimeSnapshot = datetimeSnapshot |>
          as.character() |>
          lubridate::ymd_hms() |>
          lubridate::with_tz(Sys.timezone()),
        idLink = as.numeric(idLink),
        pctLinkPosition = as.numeric(pctLinkPosition),
        urlGFG = url
      ) |>
      select(
        datetimeSnapshot,
        urlFrontpage,
        idLink,
        pctLinkPosition,
        urlDestination,
        textLink,
        everything()
      )

    data
  }

.parse_gfg_url_m <-
  function(url, return_message = TRUE) {
    .parse_gfg_url_safe <- possibly(.parse_gfg_url, tibble())
    .tt <- memoise(.parse_gfg_url_safe)
    .tt(url = url, return_message = return_message)
  }

#' Download GFG Data
#'
#' Acquires data from the GDELT Global Frontpage Graph for a date/time range.
#'
#' WARNING: Each hourly GFG file contains approximately 10 million+ rows.
#' Recommend using small date ranges (e.g., a few hours).
#'
#' @param start_date start datetime
#' @param end_date end datetime
#' @param return_message if `TRUE` returns informational messages
#' @param use_parallel if `TRUE` uses parallel processing
#' @param workers number of parallel workers (defaults to available cores)
#'
#' @return A `tibble` of GFG data
#' @export
#'
#' @examples
#' \dontrun{
#' get_data_gfg(
#'   start_date = Sys.time() - lubridate::hours(1),
#'   end_date = Sys.time()
#' )
#' }
get_data_gfg <-
  function(start_date = Sys.time() - lubridate::hours(1),
           end_date = Sys.time(),
           return_message = TRUE,
           use_parallel = FALSE,
           workers = NULL) {
    tryCatch({
      df_urls <- dictionary_gfg_urls(
        start_date = start_date,
        end_date = end_date
      )

      if (nrow(df_urls) == 0) {
        cli::cli_alert_warning("No GFG URLs generated for date range")
        return(tibble())
      }

      if (return_message) {
        cli::cli_alert_info(
          "Acquiring GFG data for {nrow(df_urls)} hour(s)"
        )
      }

      .parse_gfg_url_m_safe <-
        possibly(.parse_gfg_url_m, tibble())

      data <-
        .safe_map_dfr(
          df_urls$urlGFG,
          function(url) {
            .parse_gfg_url_m_safe(url = url, return_message = return_message)
          },
          use_parallel = use_parallel,
          workers = workers
        )

      data
    }, error = function(e) {
      cli::cli_alert_danger("Failed: {e$message}")
      return(tibble())
    })
  }

#' Download Most Recent GFG Data
#'
#' Acquires data from the most recent hour of the GDELT Global Frontpage Graph.
#'
#' @param return_message if `TRUE` returns informational messages
#'
#' @return A `tibble` of the most recent GFG data
#' @export
#'
#' @examples
#' \dontrun{
#' get_data_gfg_most_recent()
#' }
get_data_gfg_most_recent <-
  function(return_message = TRUE) {
    tryCatch({
      # Calculate current UTC hour
      current_time <- Sys.time() |>
        lubridate::with_tz("UTC")

      # Round down to the hour
      current_hour <- current_time |>
        lubridate::floor_date("1 hour")

      url <- current_hour |> .build_gfg_url()

      if (return_message) {
        cli::cli_alert_info("Acquiring most recent GFG data ({current_hour} UTC)")
      }

      data <- .parse_gfg_url(url = url, return_message = return_message)

      data
    }, error = function(e) {
      cli::cli_alert_danger("Failed: {e$message}")
      return(tibble())
    })
  }
