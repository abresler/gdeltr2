# utils -------------------------------------------------------------------

.extract_ngram_inventory_date <-
  function(url) {
    parts <- url |> stringr::str_split("/") |> purrr::flatten_chr()
    datestring <- parts[length(parts)] |> stringr::str_remove_all("\\.txt")
    lubridate::ymd(datestring)
  }

.parse_tv_ngram_file <-
  function(url, return_message = TRUE) {
    time <- .extract_ngram_inventory_date(url)
    if (return_message) {
      cli::cli_alert_info("Acquiring TV ngram inventory for {time}")
    }

    # Inventory file is one URL per line
    data <-
      readr::read_lines(url) |>
      tibble::tibble(urlNgram = _) |>
      dplyr::filter(nchar(urlNgram) > 0) |>
      dplyr::mutate(
        # Extract station, date, and ngram type from URL filename
        # e.g., ALJAZ_20240115_000000_NEWS_1gram.txt.gz
        filename = urlNgram |> stringr::str_extract("[^/]+$"),
        nameStation = filename |> stringr::str_extract("^[A-Z0-9]+"),
        typeNgram = filename |> stringr::str_extract("[12345]gram"),
        dateData = time
      ) |>
      dplyr::select(dateData, nameStation, typeNgram, urlNgram)

    data
  }

.parse_tv_ngram_file_m <-
  function(url, return_message = TRUE) {
    .parse_tv_ngram_file_safe <- purrr::possibly(.parse_tv_ngram_file, tibble::tibble())
    .tt <- memoise::memoise(.parse_tv_ngram_file_safe)
    .tt(url = url, return_message = return_message)
  }

.parse_tv_ngram_data <-
  function(url, return_message = TRUE) {
    if (return_message) {
      cli::cli_alert_info("Acquiring TV ngram data from {url}")
    }

    data <-
      data.table::fread(url, sep = "\t", header = FALSE) |>
      tibble::as_tibble() |>
      purrr::set_names(c("dateData", "idStation", "hourBroadcast", "textWord", "countMentions"))

    data |>
      dplyr::mutate(
        dateData = lubridate::ymd(dateData),
        hourBroadcast = as.numeric(hourBroadcast),
        countMentions = as.numeric(countMentions)
      )
  }

.parse_tv_ngram_data_m <-
  function(url, return_message = TRUE) {
    .parse_tv_ngram_data_safe <- purrr::possibly(.parse_tv_ngram_data, tibble::tibble())
    .tt <- memoise::memoise(.parse_tv_ngram_data_safe)
    .tt(url = url, return_message = return_message)
  }

.parse_web_ngram_data <-
  function(url, return_message = TRUE) {
    if (return_message) {
      cli::cli_alert_info("Acquiring web ngram data from {url}")
    }

    data <-
      curl::curl(url) |>
      gzcon() |>
      data.table::fread(sep = "\t", header = FALSE) |>
      tibble::as_tibble() |>
      purrr::set_names(c("textTerm", "countMentions", "languageArticle"))

    data |>
      dplyr::mutate(
        countMentions = as.numeric(countMentions)
      )
  }

.parse_web_ngram_data_m <-
  function(url, return_message = TRUE) {
    .parse_web_ngram_data_safe <- purrr::possibly(.parse_web_ngram_data, tibble::tibble())
    .tt <- memoise::memoise(.parse_web_ngram_data_safe)
    .tt(url = url, return_message = return_message)
  }


# dict --------------------------------------------------------------------

#' TV Ngram File Inventory
#'
#' Get available TV ngram files for a specified date
#'
#' @param date date in YMD format. Defaults to 2 days ago.
#'
#' @return A `tibble` of available TV ngram files with columns:
#'   dateData (date), nameStation (character), typeNgram (character), urlNgram (character)
#' @export
#'
#' @examples
#' \dontrun{
#' dictionary_tv_ngram_inventory()
#' }
dictionary_tv_ngram_inventory <-
  function(date = Sys.Date() - 2) {
    tryCatch({
      date <- as.Date(date)

      datestring <- date |> format("%Y%m%d")
      url <- glue::glue("http://data.gdeltproject.org/gdeltv3/iatv/ngramsv2/{datestring}.txt")

      data <- .parse_tv_ngram_file_m(url = url, return_message = FALSE)

      data
    }, error = function(e) {
      cli::cli_alert_danger("Failed: {e$message}")
      return(tibble::tibble())
    })
  }


# get_data ----------------------------------------------------------------

#' Get TV Ngram Data
#'
#' Download TV ngram data for a specified date
#'
#' @param date date in YMD format. Defaults to 2 days ago.
#' @param stations vector of station names to filter. If NULL, retrieves all stations.
#' @param ngram_type ngram type to retrieve. Options: "1gram" or "2gram". Defaults to "1gram".
#' @param return_message if TRUE returns message
#' @param use_parallel if TRUE uses parallel processing
#' @param workers number of parallel workers
#'
#' @return A `tibble` of TV ngram data with columns:
#'   dateData (date), idStation (character), hourBroadcast (numeric),
#'   textWord (character), countMentions (numeric)
#' @export
#'
#' @examples
#' \dontrun{
#' get_data_tv_ngrams()
#' }
get_data_tv_ngrams <-
  function(date = Sys.Date() - 2,
           stations = NULL,
           ngram_type = "1gram",
           return_message = TRUE,
           use_parallel = FALSE,
           workers = NULL) {
    tryCatch({
      date <- as.Date(date)

      valid_types <- c("1gram", "2gram", "3gram", "4gram", "5gram")
      if (!(ngram_type %in% valid_types)) {
        stop("ngram_type must be one of: ", paste(valid_types, collapse = ", "))
      }

      # Get inventory
      inventory <- dictionary_tv_ngram_inventory(date = date)

      if (nrow(inventory) == 0) {
        cli::cli_alert_warning("No TV ngram inventory found for {date}")
        return(tibble::tibble())
      }

      # Filter by ngram type
      inventory <-
        inventory |>
        dplyr::filter(typeNgram == ngram_type)

      # Filter by stations if provided
      if (!is.null(stations)) {
        inventory <-
          inventory |>
          dplyr::filter(nameStation %in% stations)
      }

      if (nrow(inventory) == 0) {
        cli::cli_alert_warning("No matching ngram files found")
        return(tibble::tibble())
      }

      if (return_message) {
        cli::cli_alert_info("Acquiring TV ngram data for {date} ({nrow(inventory)} file{?s})")
      }

      # Download ngram files
      urls <- inventory$urlNgram

      if (use_parallel) {
        data <-
          furrr::future_map_dfr(
            urls,
            function(url) {
              .parse_tv_ngram_data_m(url = url, return_message = FALSE)
            },
            .options = furrr::furrr_options(seed = TRUE)
          )
      } else {
        data <-
          purrr::map_dfr(
            urls,
            function(url) {
              .parse_tv_ngram_data_m(url = url, return_message = FALSE)
            }
          )
      }

      data |>
        dplyr::select(dateData, idStation, hourBroadcast, textWord, countMentions)

    }, error = function(e) {
      cli::cli_alert_danger("Failed: {e$message}")
      return(tibble::tibble())
    })
  }


#' Get Web Ngram Data
#'
#' Download web ngram data for a specified datetime
#'
#' @param datetime datetime in POSIXct format. Defaults to 1 hour ago.
#' @param ngram_type ngram type to retrieve. Options: "1gram" or "2gram". Defaults to "1gram".
#' @param return_message if TRUE returns message
#'
#' @return A `tibble` of web ngram data with columns:
#'   textTerm (character), countMentions (numeric), languageArticle (character)
#' @export
#'
#' @examples
#' \dontrun{
#' get_data_web_ngrams()
#' }
get_data_web_ngrams <-
  function(datetime = Sys.time() - 3600,
           ngram_type = "1gram",
           return_message = TRUE) {
    tryCatch({
      valid_types <- c("1gram", "2gram", "3gram", "4gram", "5gram")
      if (!(ngram_type %in% valid_types)) {
        stop("ngram_type must be one of: ", paste(valid_types, collapse = ", "))
      }

      # Format datetime for URL
      datetimestring <- datetime |> format("%Y%m%d%H%M%S")

      url <-
        glue::glue("http://data.gdeltproject.org/gdeltv3/web/ngrams/{datetimestring}.{ngram_type}.txt.gz")

      if (return_message) {
        cli::cli_alert_info("Acquiring web ngram data for {datetime}")
      }

      data <- .parse_web_ngram_data_m(url = url, return_message = FALSE)

      data |>
        dplyr::select(textTerm, countMentions, languageArticle)

    }, error = function(e) {
      cli::cli_alert_danger("Failed: {e$message}")
      return(tibble::tibble())
    })
  }
