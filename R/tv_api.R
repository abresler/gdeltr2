# https://blog.gdeltproject.org/gdelt-2-0-television-api-debuts/


# api ---------------------------------------------------------------------

 # https://blog.gdeltproject.org/gdelt-2-0-television-api-debuts/



# dailies -----------------------------------------------------------------



# stations

#' GDELT TV Station Dictionary
#'
#' Returns information about the television
#' stations monitored by GDELT
#'
#' @return a \code{tibble}
#' @export
#' @import dplyr jsonlite lubridate purrr
#' @importFrom tidyr separate
#'
#' @examples
#' dictionary_gdelt_tv_stations()
dictionary_gdelt_tv_stations <-
  function() {
    json_data <-
      "https://api.gdeltproject.org/api/v2/tv/tv?mode=stationdetails&format=json" %>%
      jsonlite::fromJSON(simplifyVector = T)

    data <-
      json_data$station_details %>%
      as_tibble() %>%
      purrr::set_names(
        c(
          "idStation",
          "nameStation",
          "marketStation",
          "slugNetwork",
          "datetimeStartData",
          "datetimeEndData"
        )
      )

    data <-
      data %>%
      tidyr::separate(
        col = "nameStation",
        into = c("nameStation", "slugAffiliate"),
        sep = "\\("
      ) %>%
      mutate(slugAffiliate = slugAffiliate %>% str_replace_all("\\)", "")) %>%
      mutate_all(str_trim) %>%
      suppressWarnings() %>%
      suppressMessages()

    data <-
      data %>%
      mutate(
        datetimeStartData = datetimeStartData %>% lubridate::ymd_hms() %>% lubridate::with_tz(Sys.timezone()),
        datetimeEndData = datetimeEndData %>% lubridate::ymd_hms() %>% lubridate::with_tz(Sys.timezone())
      )

    data

  }


generate_tv_inventory_url <-  function(date = Sys.Date()) {
  if (date > Sys.Date()) {
    stop("Data cannot go into the future")
  }

  if (date < "2009-06-16") {
    stop("Sorry data starts on 2009-06-16")
  }

  date_slug <-
    date %>% lubridate::ymd() %>% str_replace_all("\\-", "")

  base <- "http://data.gdeltproject.org/gdeltv3/iatv/inventory"

  url <-
    glue::glue("{base}/{date_slug}.inventory.csv") %>% as.character()

  tibble(dateData = date, urlGDELTInventory = url)

}

generate_tv_inventory_urls <-
  function(date_start = c("2018-02-01"),
           date_end = NULL) {
    if (date_start %>% purrr::is_null()) {
      stop("Please enter a date")
    }

    if (date_end %>% purrr::is_null()) {
      data <- generate_tv_inventory_url(date = date_start)
      return(data)
    }

    dates <- seq(ymd(date_start), ymd(date_end), by = "days")
    generate_tv_inventory_url_safe <-
      purrr::possibly(generate_tv_inventory_url, tibble())

    data <-
      dates %>%
      future_map_dfr(function(date) {
        generate_tv_inventory_url_safe(date = date)
      })

    data


  }


parse_summary_inventoy_data_url <-
  function(url = "http://data.gdeltproject.org/gdeltv3/iatv/inventory/20180202.inventory.csv") {
    data <-
      url %>%
      read_csv() %>%
      suppressMessages()

    data <-
      data %>%
      purrr::set_names(
        c(
          "idShow",
          "urlArchive",
          "idStation",
          "nameShow",
          "countClips",
          "durationShowSeconds",
          "datetimeShowStart",
          "datetimeShowEnd"
        )
      ) %>%
      mutate(urlGDELTInventory = url)

    data
  }

parse_summary_inventory_data_urls <-
  function(urls = "http://data.gdeltproject.org/gdeltv3/iatv/inventory/20180202.inventory.csv",
           return_message = T) {
    df <-
      tibble()

    parse_summary_inventoy_data_url_safe <-
      purrr::possibly(parse_summary_inventoy_data_url, tibble())

    success <- function(res) {
      url <-
        res$url
      if (return_message) {
        glue::glue("Parsing {url}") %>% cat(fill = T)
      }

      data <-
        url %>%
        parse_summary_inventoy_data_url_safe()

      df <<-
        df %>%
        bind_rows(data)
    }
    failure <- function(msg) {
      tibble()
    }
    urls %>%
      walk(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }

#' GDELT TV Show Summary
#'
#' Acquires daily summary data for the television shows
#' that GDELT monitors
#'
#' @param date_start Start date of data, cannot be before June 16th, 2009
#' @param date_end End date of data, cannot exceed current date
#' @param return_message if \code{TRUE} returns a messag
#'
#' @return a \code{tibble}
#' @export
#' @importFrom glue glue
#' @importFrom readr read_csv
#' @import curl dplyr curl lubridate
#' @examples
#' get_data_tv_summaries(date_start = "2018-02-02", date_end = Sys.Date(), return_message = T)
get_data_tv_summaries <-
  function(date_start = "2018-02-02",
           date_end = Sys.Date(),
           return_message = T) {
    df_urls <-
      generate_tv_inventory_urls(date_start = date_start, date_end = date_end)

    all_data <-
      parse_summary_inventory_data_urls(urls = df_urls$urlGDELTInventory,
                                        return_message = return_message)

    all_data <-
      all_data %>%
      left_join(df_urls) %>%
      select(dateData, everything()) %>%
      arrange(dateData) %>%
      suppressMessages()

    all_data <-
      all_data %>%
      mutate(
        datetimeShowStart = datetimeShowStart %>% lubridate::with_tz(Sys.timezone()),
        datetimeShowEnd = datetimeShowEnd %>% lubridate::with_tz(Sys.timezone())
      )


    all_data

  }