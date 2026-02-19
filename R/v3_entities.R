
# utils -------------------------------------------------------------------



# dict --------------------------------------------------------------------

.extract_time <-
  function(url) {
    parts <- url |> str_split("/") |> flatten_chr()
    time <- parts[length(parts)] |> str_remove_all("\\.geg-g1.json.gz") |> lubridate::ymd_hms() |> lubridate::with_tz(Sys.timezone())
    time
  }

.recent_entities_time <- function() {
  read_lines("http://data.gdeltproject.org/gdeltv3/geg_g1/LASTUPDATE.TXT") |>
    .extract_time()
}

.dictionary_v3_entity_names <-
  function() {
    tibble(nameGDELT = c("urlArticle", "name", "type", "numMentions", "avgSalience",
                         "context", "mid", "wikipediaUrl"),
           nameActual = c("urlArticle", "nameEntity", "typeEntity", "countMentions", "meanSalience",
                          "textContext", "midGoogle", "urlWikipedia")

    )
  }

.munge_entity_names <-
  function(data) {
    names_dict <- names(data)

    dict <- .dictionary_v3_entity_names()
    actual_names <-
      names_dict |>
      map_chr(function(name) {
        df_row <-
          dict |> filter(nameGDELT == name)
        if (nrow(df_row) == 0) {
          cli::cli_alert_warning("Missing {name}")
          return(name)
        }

        df_row$nameActual
      })

    data |>
      set_names(actual_names)
  }

.dictionary_v3_entities <-
  function() {
    data <-
      "http://data.gdeltproject.org/gdeltv3/geg_gcnlapi/MASTERFILELIST.TXT" |>
      fread() |>
      as_tibble() |>
      setNames(c("urlAPI"))

    data |>
      mutate(
        datetimeData = urlAPI |> str_remove_all("http://data.gdeltproject.org/gdeltv3/geg_gcnlapi/") |>
          str_remove_all(".geg-gcnlapi.json.gz") |> as.numeric() |> lubridate::ymd_hms() |> lubridate::with_tz(Sys.timezone()),
        dateData = as.Date(datetimeData),
        yearData = year(dateData) |> as.numeric(),
        monthData = month(dateData) |> as.numeric()
      ) |>
      select(yearData, monthData, dateData, datetimeData, everything())
  }

#' V3 Entity API URL dictionary
#'
#' Dictionary of URLs from the V3 API
#' updated on 15 minute intervals
#'
#' @return A `tibble` of V3 entity API URLs with datetime metadata
#' @export
#' @examples
#' dictionary_v3_entity_urls()
dictionary_v3_entity_urls <-
  function(){
    tryCatch({
      .tt <- memoise(.dictionary_v3_entities)
      data <- .tt()
      most_recent_dict <-
        data |> select(datetimeData) |> pull() |> max()

      options(scipen = 99999)
      df_missing <-
        data |>
        mutate(
          isoPeriod = datetimeData |> format("%Y%m%d%H%M%S") |> as.numeric(),
          urlAPI = glue(
            "http://data.gdeltproject.org/gdeltv3/geg_g1/{isoPeriod}.geg-g1.json.gz"
          ) |> as.character(),
          dateData = as.Date(datetimeData),
          yearData = year(dateData) |> as.numeric(),
          monthData = month(dateData) |> as.numeric()
        ) |>
        select(-isoPeriod) |>
        select(yearData, monthData, dateData, datetimeData, everything())

      data
    }, error = function(e) {
      cli::cli_alert_danger("Failed: {e$message}")
      return(tibble())
    })
  }



# parse -------------------------------------------------------------------


.parse_json_entity_data <-
  function(data) {
    df_entities <-
      data |>
      select(urlArticle, dataEntities) |>
      unnest(cols = dataEntities)

    df_entities <- df_entities |> .munge_entity_names()

    df_entities <-
      df_entities |>
      mutate(countMentions = as.numeric(countMentions)) |>
      nest(dataEntities = -urlArticle) |>
      mutate(countEntitesArticle = dataEntities |> map_dbl(nrow)) |>
      select(urlArticle, countEntitesArticle, everything())

    data <- data |>
      select(-dataEntities) |>
      left_join(df_entities, by = "urlArticle")

    data
  }

.parse_entity_json_url <-
  function(url = "http://data.gdeltproject.org/gdeltv3/geg_gcnlapi/20170709211500.geg-gcnlapi.json.gz",
           return_message = TRUE) {
    time <- .extract_time(url = url)
    if (return_message) {

      cli::cli_alert_info("Acquiring V3 Entity API data for {time}")
    }

    data <-
      curl(url) |>
      gzcon() |>
      stream_in() |>
      as_tibble()

    data <-
      data |>
      setNames(
        c(
          "datetimeArticle",
          "urlArticle",
          "languageArticle",
          "polarity",
          "magnitude",
          "score",
          "dataEntities"
        )
      ) |>
      mutate(
        datetimeArticle = ymd_hms(datetimeArticle) |> lubridate::with_tz(Sys.timezone()),
        dateArticle = as.Date(datetimeArticle),
        yearArticle = year(dateArticle) |> as.numeric(),
        monthArticle = month(dateArticle) |> as.numeric(),
        domainArticle = urlArticle |> urltools::domain(),
        urlAPI = url
      ) |>
      select(
        yearArticle,
        monthArticle,
        dateArticle,
        datetimeArticle,
        domainArticle,
        everything()
      )

    data <-
      data |>
      .parse_json_entity_data()

    data
  }

.parse_entity_json_url_m <-
  function(url, return_message = TRUE) {
    .parse_entity_json_url_safe <- possibly(.parse_entity_json_url, tibble())
    .tt <- memoise(.parse_entity_json_url_safe)
    .tt(url = url, return_message = return_message)
  }

#' Parse V3 Entity API URLs
#'
#' Parse vector of V3 entity API urls
#'
#' @param urls vector of URLs
#' @param return_message if \code{TRUE} returns message
#'
#' @return \code{tibble}
#' @export
#'
#' @examples
#' \dontrun{
#' parse_v3_entity_api_urls()
#' }
parse_v3_entity_api_urls <-
  function(urls = c(
    "http://data.gdeltproject.org/gdeltv3/geg_gcnlapi/20190214011500.geg-gcnlapi.json.gz",
    "http://data.gdeltproject.org/gdeltv3/geg_gcnlapi/20190130204500.geg-gcnlapi.json.gz",
    "http://data.gdeltproject.org/gdeltv3/geg_gcnlapi/20190912134500.geg-gcnlapi.json.gz"
  ),
  return_message = TRUE,
  use_parallel = FALSE,
  workers = NULL) {
    tryCatch({
      .parse_entity_json_url_m_safe <-
        possibly(.parse_entity_json_url_m, tibble())

      .safe_map_dfr(
        urls,
        function(url) {
          .parse_entity_json_url_m_safe(url = url, return_message = return_message)
        },
        use_parallel = use_parallel,
        workers = workers
      )
    }, error = function(e) {
      cli::cli_alert_danger("Failed: {e$message}")
      return(tibble())
    })
  }


#' V3 Entity API data
#'
#' Acquires data from the GDELT
#' entity mention API for period of specified dates
#'
#'
#' @param start_date start date in YMD format
#' @param end_date if not \code{NULL} end date
#' @param return_message if \code{TRUE} return message
#'
#' @return A `tibble` of V3 entity data for the specified date range
#' @export
#'
#' @examples
#' \dontrun{
#' v3_entity_api()
#' }
v3_entity_api <-
  function(start_date = Sys.Date()-1,
           end_date =Sys.Date(),
           return_message = TRUE) {
    tryCatch({
      if (length(start_date) == 0) {
        stop("Enter start date")
      }

      start_date <-
        start_date |>
        as.character() |>
        parse_date()

      if (start_date < ymd("2016-07-17")){
        cli::cli_alert_warning("Entity API Data starts 2016-07-17")
      }

      df_urls <- dictionary_v3_entity_urls()

      df_urls <-
        df_urls |>
        filter(dateData >= start_date)

      if (length(end_date) > 0) {
        end_date <-
          end_date |>
          as.character() |>
          parse_date()
        df_urls <-
          df_urls |>
          filter(dateData <= end_date)
        end_slug <-
          glue(" to {end_date}")
      } else {
        end_slug <- ""
      }

      if (return_message) {
        cli::cli_alert_info("Acquiring V3 entity API data for {start_date}{end_slug}")
      }


      data <-
        parse_v3_entity_api_urls(urls = df_urls$urlAPI, return_message = return_message)

      data
    }, error = function(e) {
      cli::cli_alert_danger("Failed: {e$message}")
      return(tibble())
    })
  }
