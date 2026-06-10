

# utilities ---------------------------------------------------------------
#' Filters a gdelt data frame to specified sources
#'
#' @param data gkg or vgkg data frame
#' @param sources vector of sources
#'
#' @return A filtered `tibble` containing only rows matching the specified sources
#' @export
#'
#' @examples
#' \dontrun{
#' filter_sources()
#' }
filter_sources <-
  function(data,
           sources = c(
             'netsdaily',
             'realdeal',
             'curbed',
             'law360',
             'dailymail',
             'wsj.com',
             'law360',
             'pehub',
             'techcrunch',
             'washingtonpost',
             'bloomberg',
             'archdaily',
             'espn.com',
             'venturebeat'
           )) {
    sources <-
      sources |>
      str_to_lower() |>
      paste0(collapse = '|')
    if (!'documentSource' %in% names(data)) {
      rlang::abort("Data must contain the `documentSource` column", class = "gdeltr2_input_error")
    }
    data <-
      data |>
      filter(documentSource |> str_detect(sources))

    return(data)
  }

.resolve_long_names <-
  function(data) {
    data <-
      data |>
      mutate(
        numberItem = ifelse(
          item |> str_detect("idADM"),
          item |> str_replace_all("idADM1", '') |> as.character() |> parse_number(),
          item |>
            as.character() |>
            parse_number()
        ),
        numberItem = ifelse(numberItem |> is.na(), 0 , numberItem),
        item = item |> str_replace_all("^\\d+|\\d+$", '')
      ) |>
      distinct() |>
      suppressWarnings()

    data <-
      data |>
      tidyr::pivot_wider(names_from = item, values_from = value) |>
      suppressWarnings()

    data <-
      data |>
      dplyr::mutate(dplyr::across(
        dplyr::matches(
          "^score|^count|^amount|^value|^face|^angle|^latitude|^longitude|^day|^month|^year|^idTypeLocation"
        ),
        ~parse_number(as.character(.x))))

    return(data)
  }

#' Loads needed packages
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Package dependencies are now managed
#' automatically through the NAMESPACE file. Use `library()` directly
#' if you need to load packages interactively.
#'
#' @param required_packages Character vector of package names to load
#'
#' @return Invisible NULL
#' @export
#'
#' @examples
#' \dontrun{
#' load_needed_packages(c("dplyr", "stringr"))
#' }
load_needed_packages <- function(required_packages = c('dplyr')) {
  .Deprecated(msg = "load_needed_packages() is deprecated. Use library() directly.")
  loaded_packages <- gsub("package:", "", search())
  package_to_load <- required_packages[!required_packages %in%
                                         loaded_packages]
  if (length(package_to_load) > 0) {
    lapply(package_to_load, library, character.only = TRUE)
  }
  invisible(NULL)
}


# logs --------------------------------------------------------------------



#' Loads gdelt v2 Global Knowledge Graph master log data, updated every 15 minutes
#'
#' @return A `tibble` of GKG 15-minute log URLs with date and file metadata
#' @export
#'
#' @examples
#' get_urls_gkg_15_minute_log()
get_urls_gkg_15_minute_log <- memoise::memoise(function() {
  tryCatch({
    url <-
      'http://data.gdeltproject.org/gdeltv2/masterfilelist.txt'

    log_df <-
      url |>
      read_tsv(col_names = FALSE, show_col_types = FALSE) |>
      separate(
        col = X1,
        into = c('idFile', 'idHash', 'urlData'),
        sep = '\\ ',
        extra = "merge",
        fill = "right"
      )

    log_df <-
      log_df |>
      dplyr::mutate(dateTimeFile = urlData |> str_replace_all('http://data.gdeltproject.org/gdeltv2/', '')) |>
      separate(dateTimeFile,
                      into = c('timestamp', 'nameFile', 'typeFile', 'isZip'),
                      extra = "merge", fill = "right")

    log_df <-
      log_df |>
      dplyr::mutate(
        dateTimeData = timestamp |> as.numeric() |> lubridate::ymd_hms() |> lubridate::with_tz(Sys.timezone()),
        dateData = dateTimeData |> as.Date(),
        typeFile = typeFile |> str_to_lower(),
        idFile = idFile |> as.integer()
      ) |>
      dplyr::mutate(dplyr::across(c('idHash', 'nameFile', 'urlData'), str_trim))

    return(log_df)
  }, error = function(e) {
    cli::cli_alert_danger("Failed: {e$message}")
    return(tibble())
  })
})

#' Gets GDELT Event data, by year from 1979-2005, by year month 2006 - 2013, then by dat
#'
#' @param return_message Logical. If \code{TRUE}, prints progress messages to console.
#'
#' @return A `tibble` of GDELT event log URLs with date and file metadata
#'
#' @export
#'
#' @examples
#' get_urls_gdelt_event_log()

get_urls_gdelt_event_log <- memoise::memoise(function(return_message = TRUE) {
  tryCatch({
  url <-
    'http://data.gdeltproject.org/events/md5sums'

  urlData <-
    url |>
    read_tsv(col_names = FALSE, show_col_types = FALSE) |>
    separate(col = X1,
                    into = c('idHash', 'stemData'),
                    sep = '\\  ',
                    extra = "merge", fill = "right") |>
    dplyr::mutate(
      urlData = 'http://data.gdeltproject.org/events/' |> paste0(stemData),
      slugDatabaseGDELT = 'EVENTS',
      isZipFile = ifelse(stemData |> str_detect(".zip"), TRUE, FALSE)
    )

  urlData <-
    urlData |>
    separate(
      col = stemData,
      into = c('periodData', 'nameFile', 'typeFile', 'zip_file'),
      sep = '\\.',
      extra = "merge", fill = "right"
    ) |>
    dplyr::select(-zip_file) |>
    dplyr::mutate(
      periodData = ifelse(periodData == 'GDELT', typeFile, periodData),
      isDaysData = ifelse(nchar(periodData) == 8, TRUE, FALSE)
    ) |>
    dplyr::select(-c(nameFile, typeFile))

  urlData <-
    urlData |>
    dplyr::filter(isDaysData == FALSE) |>
    dplyr::mutate(dateData = NA) |>
    bind_rows(
      urlData |>
        dplyr::filter(isDaysData == TRUE) |>
        dplyr::mutate(dateData = periodData |> lubridate::ymd() |> as.Date())
    ) |>
    dplyr::select(idHash,
                  dateData,
                  isZipFile,
                  isDaysData,
                  urlData,
                  everything())

  if (return_message) {
    count.files <-
      nrow(urlData)

    min.date <-
      min(urlData$dateData, na.rm = TRUE)

    max.date <-
      max(urlData$dateData, na.rm = TRUE)

    cli::cli_alert_info("You got {count.files} GDELT Global Knowledge Graph URLS from {min.date} to {max.date}")
  }

  return(urlData)
  }, error = function(e) {
    cli::cli_alert_danger("Failed: {e$message}")
    return(tibble())
  })
})

#' Gets most recent GKG log URLs
#'
#' @return A `tibble` of the most recent GKG log URLs
#'
#' @export
#'
#' @examples
#' get_urls_gkg_most_recent_log()
get_urls_gkg_most_recent_log <- function() {
  tryCatch({
    log_df <-
      'http://data.gdeltproject.org/gdeltv2/lastupdate.txt' |>
      read_tsv(col_names = FALSE, show_col_types = FALSE) |>
      separate(
        col = X1,
        into = c('idFile', 'idHash', 'urlData'),
        sep = '\\ ',
        extra = "merge", fill = "right"
      )

    log_df <-
      log_df |>
      dplyr::mutate(dateTimeFile = urlData |> str_replace_all('http://data.gdeltproject.org/gdeltv2/', '')) |>
      separate(dateTimeFile,
                      into = c('timestamp', 'nameFile', 'typeFile', 'isZip'),
                      extra = "merge", fill = "right") |>
      dplyr::mutate(
        typeFile = typeFile |> str_to_lower(),
        isZip = ifelse(isZip |> str_detect("ZIP|zip"), TRUE, FALSE),
        idFile = idFile |> as.integer()
      ) |>
      dplyr::mutate(dplyr::across(c('idHash', 'nameFile', 'urlData'), str_trim))

    return(log_df)
  }, error = function(e) {
    cli::cli_alert_danger("Failed: {e$message}")
    return(tibble())
  })
}


#' Gets Global Knowledge Graph summary files by day since April 2013
#'
#' @param remove_count_files Logical. If \code{TRUE}, removes count files after processing.
#' @param return_message Logical. If \code{TRUE}, prints progress messages to console.
#'
#' @return A `tibble` of GKG daily summary URLs with date metadata
#' @export
#'
#' @examples
#' get_urls_gkg_daily_summaries(remove_count_files = TRUE)
get_urls_gkg_daily_summaries <-
  function(remove_count_files = FALSE,
           return_message = TRUE) {
    tryCatch({
    url <-
      'http://data.gdeltproject.org/gkg/md5sums'

    urlData <-
      url |>
      read_tsv(col_names = FALSE, show_col_types = FALSE) |>
      separate(col = X1,
                      into = c('idHash', 'stemData'),
                      sep = '\\  ',
                      extra = "merge", fill = "right") |>
      dplyr::mutate(urlData = 'http://data.gdeltproject.org/gkg/' |> paste0(stemData),
                    slugDatabaseGDELT = 'gkg') |>
      separate(
        col = stemData,
        into = c('dateData', 'nameFile', 'typeFile', 'isZipFile'),
        sep = '\\.',
        extra = "merge", fill = "right"
      ) |>
      dplyr::mutate(
        isZipFile = ifelse(isZipFile == "zip", TRUE, FALSE),
        isCountFile = ifelse(nameFile == 'gkgcounts', TRUE, FALSE),
        dateData = dateData |> lubridate::ymd() |> as.Date()
      ) |>
      dplyr::select(-c(nameFile, typeFile)) |>
      dplyr::select(idHash,
                    dateData,
                    isZipFile,
                    isCountFile,
                    urlData,
                    everything())

    if (remove_count_files) {
      urlData <-
        urlData |>
        dplyr::filter(isCountFile == FALSE)
    }

    if (return_message) {
      count.files <-
        nrow(urlData)

      min.date <-
        min(urlData$dateData, na.rm = TRUE)

      max.date <-
        max(urlData$dateData, na.rm = TRUE)

      cli::cli_alert_info("You got {count.files} GDELT Global Knowledge Graph URLS from {min.date} to {max.date}")
    }

    return(urlData)
    }, error = function(e) {
      cli::cli_alert_danger("Failed: {e$message}")
      return(tibble())
    })
  }

#' Retrives most recent GDELT Global Content Analysis Measures (GCAM) code book
#'
#'
#' @return A `tibble` of GCAM codebook entries with dimension and variable metadata
#' @export
#'
#' @examples
#' get_codes_gcam()
get_codes_gcam <- memoise::memoise(function() {
  tryCatch({
    url <-
      'http://data.gdeltproject.org/documentation/GCAM-MASTER-CODEBOOK.TXT'
    gcam_data <-
      url |>
      read_tsv() |>
      suppressWarnings() |>
      suppressMessages()

    names(gcam_data) <-
      c(
        'idGCAM',
        'idDictionary',
        'idDimension',
        'typeDictionary',
        'codeLanguage',
        'dictionaryHumanName',
        'dimensionHumanName',
        'dictionaryCitation'
      )

    gcam_data
  }, error = function(e) {
    cli::cli_alert_danger("Failed: {e$message}")
    return(tibble())
  })
})

#' Retrives GDELT CAMEO religion code book
#'
#'
#' @return A `tibble` of CAMEO religion codes and descriptions
#' @export
#'
#' @examples
#' get_codes_cameo_religion()
get_codes_cameo_religion <- memoise::memoise(function() {
  tryCatch({
    url <-
      'http://gdeltproject.org/data/lookups/CAMEO.religion.txt'
    code_df <-
      url |>
      read_tsv() |>
      suppressWarnings() |>
      suppressMessages()

    names(code_df) <-
      c('codeCAMEOReligion', 'nameCAMEOReligion')

    return(code_df)
  }, error = function(e) {
    cli::cli_alert_danger("Failed: {e$message}")
    return(tibble())
  })
})

#' Retrieves GDELT CAMEO country code book
#'
#'
#' @return A `tibble` of CAMEO country codes and names
#'
#' @export
#'
#' @examples
#' get_codes_cameo_country()
get_codes_cameo_country <- memoise::memoise(function() {
  tryCatch({
    url <-
      'http://gdeltproject.org/data/lookups/CAMEO.country.txt'
    code_df <-
      url |>
      read_tsv() |>
      suppressWarnings() |>
      suppressMessages()

    names(code_df) <-
      c('codeISO', 'nameCountry')

    code_df
  }, error = function(e) {
    cli::cli_alert_danger("Failed: {e$message}")
    return(tibble())
  })
})

#' Retrieves GDELT CAMEO type code book
#'
#' @return A `tibble` of CAMEO actor type codes and descriptions
#'
#' @export
#'
#' @examples
#' get_codes_cameo_type()
get_codes_cameo_type <- memoise::memoise(function() {
  tryCatch({
    url <- 'http://gdeltproject.org/data/lookups/CAMEO.type.txt'
    code_df <-
      url |>
      read_tsv() |>
      suppressWarnings() |>
      suppressMessages()

    names(code_df) <-
      c('codeCAMEOType', 'nameCAMEOType')

    code_df
  }, error = function(e) {
    cli::cli_alert_danger("Failed: {e$message}")
    return(tibble())
  })
})

#' Retrieves CAMEO CAMEO event code book
#'
#'
#' @return A `tibble` of CAMEO event codes with descriptions and categories
#' @export
#' @examples
#' get_codes_cameo_events()

get_codes_cameo_events <- memoise::memoise(function() {
  tryCatch({
    url <-
      'http://gdeltproject.org/data/lookups/CAMEO.eventcodes.txt'
    code_df <-
      url |>
      read_tsv() |>
      suppressWarnings() |>
      suppressMessages()

    names(code_df) <-
      c('idCAMEOEvent', 'descriptionCAMEOEvent')

    code_df <-
      code_df |>
      dplyr::mutate(
        isParentCode = ifelse(idCAMEOEvent |> nchar() == 2, TRUE, FALSE),
        idParentCode = idCAMEOEvent |> substr(1, 2)
      ) |>
      dplyr::select(idParentCode, everything()) |>
      dplyr::mutate(dplyr::across(c("idParentCode", "idCAMEOEvent"), as.numeric))

    return(code_df)
  }, error = function(e) {
    cli::cli_alert_danger("Failed: {e$message}")
    return(tibble())
  })
})

#' Retrieves GDELT CAMEO known group code book
#'
#' @return A `tibble` of CAMEO known group codes and names
#'
#' @export
#' @examples
#' get_codes_cameo_known_groups()
get_codes_cameo_known_groups <- memoise::memoise(function() {
  tryCatch({
    url <-
      'http://gdeltproject.org/data/lookups/CAMEO.knowngroup.txt'
    code_df <-
      url |>
      read_tsv() |>
      suppressWarnings() |>
      suppressMessages()

    names(code_df) <-
      c('codeCAMEOGroup', 'nameCAMEOGroup')

    return(code_df)
  }, error = function(e) {
    cli::cli_alert_danger("Failed: {e$message}")
    return(tibble())
  })
})

#' Retrieves GDELT CAMEO ethnic code book
#'
#' @return A `tibble` of CAMEO ethnic group codes and names
#' @export
#' @examples
#' get_codes_cameo_ethnic()
get_codes_cameo_ethnic <- memoise::memoise(function() {
  tryCatch({
    url <-
      'http://gdeltproject.org/data/lookups/CAMEO.ethnic.txt'
    code_df <-
      url |>
      read_tsv() |>
      suppressWarnings() |>
      suppressMessages() |>
      set_names(c('codeCAMEOEthnicity', 'nameCAMEOEthnicity')) |>
      mutate(codeCAMEOEthnicity = codeCAMEOEthnicity |> str_to_upper())

    return(code_df)
  }, error = function(e) {
    cli::cli_alert_danger("Failed: {e$message}")
    return(tibble())
  })
})

#' Retrieves GKG theme code book
#'
#' @return A `tibble` of GKG theme codes and descriptions
#' @export
#' @examples
#' get_codes_gkg_themes()

get_codes_gkg_themes <- memoise::memoise(function() {
  tryCatch({
  url <-
    'http://data.gdeltproject.org/api/v2/guides/LOOKUP-GKGTHEMES.TXT'

  code_df <-
    url |>
    read_tsv(col_names = FALSE) |>
    setNames(c("codeGKGTheme", "countEvents"))

  code_df <-
    code_df |>
    mutate(
      code2 = codeGKGTheme |> str_to_lower(),
      isWBCode = ifelse(code2 |> str_detect("wb_"), TRUE, FALSE),
      isEconomicEvent = ifelse(code2 |> str_detect("econ_"), TRUE, FALSE),
      isTaxEvent = ifelse(code2 |> str_detect("tax_"), TRUE, FALSE),
      isSocialEvent = ifelse(code2 |> str_detect("soc_"), TRUE, FALSE),
      isMilitaryEvent = ifelse(code2 |> str_detect("military|mil_"), TRUE, FALSE),
      isGovernmentEvent = ifelse(code2 |> str_detect("gov_|government"), TRUE, FALSE),
      isMedicalEvent = ifelse(code2 |> str_detect("med_|medical"), TRUE, FALSE),
      isAgressionAct = ifelse(code2 |> str_detect("act_"), TRUE, FALSE),
      isMediaEvent = ifelse(code2 |> str_detect("media_|_media"), TRUE, FALSE),
      isEmergencyEvent = ifelse(code2 |> str_detect("emerg_"), TRUE, FALSE),
      isMovement = ifelse(code2 |> str_detect("movement_"), TRUE, FALSE),
      isCriminalEvent = ifelse(code2 |> str_detect("crime|crm_"), TRUE, FALSE)
    ) |>
    dplyr::select(-code2)

  wb_codes <-
    code_df |>
    dplyr::filter(isWBCode)

  wb_codes <-
    wb_codes |>
    mutate(codeGKGTheme = sub('\\_', '\\.', codeGKGTheme)) |>
    separate(
      codeGKGTheme,
      into = c('idDictionary', 'nameWBCode'),
      remove = FALSE,
      sep = '\\.'
    ) |>
    mutate(nameWBCode = sub('\\_', '\\.', nameWBCode)) |>
    separate(
      nameWBCode,
      into = c('idWBCode', 'nameWBCode'),
      remove = TRUE,
      sep = '\\.'
    ) |>
    mutate(
      idWBCode = as.numeric(idWBCode),
      nameWBCode = nameWBCode |> str_replace_all('\\_', ' ') |> str_to_lower()
    ) |>
    dplyr::select(-idDictionary)

  non_wb <-
    code_df |>
    dplyr::filter(!isWBCode)

  code_df <-
    non_wb |>
    bind_rows(wb_codes)

  code_df <-
    code_df |>
    mutate(codeGKGTheme = codeGKGTheme |> str_replace_all('WB.', '\\WB_')) |>
    dplyr::select(isWBCode, codeGKGTheme, idWBCode, nameWBCode, everything())

  code_df
  }, error = function(e) {
    cli::cli_alert_danger("Failed: {e$message}")
    return(tibble())
  })
})

#' Retrieves GDELT event summary by period
#'
#' @param period can be \code{c("yearly", "daily", "monthly")}
#' @param by_country is data by country
#' can be \code{c(TRUE, FALSE)}
#' @param return_message returns message
#' @return A `tibble` of GDELT event totals for the specified period
#' @export
#'
#' @examples
#' get_data_gdelt_period_event_totals(period = 'monthly', by_country = TRUE)

get_data_gdelt_period_event_totals <-
  function(period = 'yearly',
           by_country = TRUE,
           return_message = TRUE) {
    tryCatch({
    periods <-
      c('daily', 'monthly', 'yearly')
    if (!period %in% periods) {
      "Sorry periods can only be:\n" |>
        stop(paste0(paste0(periods, collapse = '\n')))
    }

    if (by_country) {
      period_slug <-
        period |>
        paste0('_country.csv')
    } else {
      period_slug <-
        period |>
        paste0('.csv')
    }
    base <-
      'http://data.gdeltproject.org/normfiles/'

    url_data <-
      base |>
      paste0(period_slug)

    period_data <-
      url_data |>
      read_csv(col_names = FALSE) |>
      suppressWarnings() |>
      suppressMessages()

    if (by_country) {
      names(period_data) <-
        c('idDate', 'idCountry', 'countEvents')
    } else {
      names(period_data) <-
        c('idDate', 'countEvents')
    }

    period_data <-
      period_data |>
      mutate(periodData = period, isByCountry = by_country)

    if (period == 'daily') {
      period_data <-
        period_data |>
        mutate(dateData = idDate |> lubridate::ymd() |> as.Date()) |>
        dplyr::select(periodData, isByCountry, dateData, everything())
    }

    if (period == 'monthly') {
      period_data <-
        period_data |>
        mutate(yearMonth = idDate) |>
        dplyr::select(periodData, isByCountry, yearMonth, everything()) |>
        mutate(
          yearData = yearMonth |> substr(1, 4) |> as.numeric(),
          monthData = yearMonth |> substr(5, 6) |> as.numeric()
        ) |>
        select(periodData, isByCountry, yearData, monthData, everything())
    }

    if (period == 'yearly') {
      period_data <-
        period_data |>
        mutate(yearData = idDate) |>
        dplyr::select(periodData, isByCountry, everything())
    }

    if (return_message)  {
      from_date <-
        period_data$idDate |> min()

      to_date <-
        period_data$idDate |> max()

      total_events <-
        period_data$countEvents |> sum() / 1000000
      events_slug <-
        total_events |> paste0(" million GDELT events from ")
      cli::cli_alert_info("There have been {events_slug}{from_date} to {to_date}")
    }
    return(period_data)
    }, error = function(e) {
      cli::cli_alert_danger("Failed: {e$message}")
      return(tibble())
    })
  }


#' Retrieves GDELT event database schema
#'
#' @return A `tibble` of GDELT event column names and their descriptions
#' @examples
#' get_schema_gdelt_events

get_schema_gdelt_events <- function() {
  gdelt_events_schema <-
    tibble(
      nameGDELT = c(
        "GLOBALEVENTID",
        "SQLDATE",
        "MonthYear",
        "Year",
        "FractionDate",
        "Actor1Code",
        "Actor1Name",
        "Actor1CountryCode",
        "Actor1KnownGroupCode",
        "Actor1EthnicCode",
        "Actor1Religion1Code",
        "Actor1Religion2Code",
        "Actor1Type1Code",
        "Actor1Type2Code",
        "Actor1Type3Code",
        "Actor2Code",
        "Actor2Name",
        "Actor2CountryCode",
        "Actor2KnownGroupCode",
        "Actor2EthnicCode",
        "Actor2Religion1Code",
        "Actor2Religion2Code",
        "Actor2Type1Code",
        "Actor2Type2Code",
        "Actor2Type3Code",
        "IsRootEvent",
        "EventCode",
        "EventBaseCode",
        "EventRootCode",
        "QuadClass",
        "GoldsteinScale",
        "NumMentions",
        "NumSources",
        "NumArticles",
        "AvgTone",
        "Actor1Geo_Type",
        "Actor1Geo_FullName",
        "Actor1Geo_CountryCode",
        "Actor1Geo_ADM1Code",
        "Actor1Geo_ADM2Code",
        "Actor1Geo_Lat",
        "Actor1Geo_Long",
        "Actor1Geo_FeatureID",
        "Actor2Geo_Type",
        "Actor2Geo_FullName",
        "Actor2Geo_CountryCode",
        "Actor2Geo_ADM1Code",
        "Actor2Geo_ADM2Code",
        "Actor2Geo_Lat",
        "Actor2Geo_Long",
        "Actor2Geo_FeatureID",
        "ActionGeo_Type",
        "ActionGeo_FullName",
        "ActionGeo_CountryCode",
        "ActionGeo_ADM1Code",
        "ActionGeo_ADM2Code",
        "ActionGeo_Lat",
        "ActionGeo_Long",
        "ActionGeo_FeatureID",
        "DATEADDED",
        "SOURCEURL"
      ),
      nameActual = c(
        "idGlobalEvent",
        "dateEvent",
        "monthYearEvent",
        "yearEvent",
        "dateFraction",
        "codeActor1",
        "nameActor1",
        "codeISOActor1",
        "codeCAMEOGroupActor1",
        "codeCAMEOEthnicityActor1",
        "codeCAMEOReligionActor1",
        "codeCAMEOReligion2Actor1",
        "codeCAMEOTypeActor1",
        "codeCAMEOType2Actor1",
        "codeCAMEOType3Actor1",
        "codeActor2",
        "nameActor2",
        "codeISOActor2",
        "codeCAMEOGroupActor2",
        "codeCAMEOEthnicityActor2",
        "codeCAMEOReligionActor2",
        "codeCAMEOReligion2Actor2",
        "codeCAMEOTypeActor2",
        "codeCAMEOType2Actor2",
        "codeCAMEOType3Actor.3",
        "isRootEvent",
        "idCAMEOEvent",
        "idCAMEOEventBase",
        "idCAMEOEventRoot",
        "classQuad",
        "scoreGoldstein",
        "countMentions",
        "countSources",
        "countArticles",
        "avgTone",
        "idTypeLocationActor1",
        "locationActor1",
        "idCountryActor1",
        "idADM1CodeActor1",
        "idADM2CodeActor1",
        "latitudeActor1",
        "longitudeActor1",
        "idFeatureActor1",
        "idTypeLocationActor2",
        "locationActor2",
        "idCountryActor2",
        "idADM1CodeActor2",
        "idADM2CodeActor2",
        "latitudeActor2",
        "longitudeActor2",
        "idFeatureActor2",
        "idTypeLocationAction",
        "locationAction",
        "idCountryAction",
        "idADM1CodeAction",
        "idADM2CodeAction",
        "latitudeAction",
        "longitudeAction",
        "idFeatureAction",
        "dateTimeDataAdded",
        "urlSource"
      )
    )
  return(gdelt_events_schema)
}

#' Gets gkg general schema
#'
#' @return A `tibble` of GKG general column names and descriptions
#'
#' @examples
#' get_schema_gkg_general()
get_schema_gkg_general <- function() {
  schema_df <-
    tibble(
      nameGDELT = c(
        "GKGRECORDID",
        "DATE",
        "SourceCollectionIdentifier",
        "SourceCommonName",
        "DocumentIdentifier",
        "Counts",
        "V2Counts",
        "Themes",
        "V2Themes",
        "Locations",
        "V2Locations",
        "Persons",
        "V2Persons",
        "Organizations",
        "V2Organizations",
        "V2Tone",
        "Dates",
        "GCAM",
        "SharingImage",
        "RelatedImages",
        "SocialImageEmbeds",
        "SocialVideoEmbeds",
        "Quotations",
        "AllNames",
        "Amounts",
        "TranslationInfo",
        "Extras",
        "NUMARTS",
        "COUNTS",
        "THEMES",
        "LOCATIONS",
        "PERSONS",
        "ORGANIZATIONS",
        "TONE",
        "CAMEOEVENTIDS",
        "SOURCES",
        "SOURCEURLS"

      ),
      nameActual = c(
        "idGKG",
        "dateDocument",
        "idSourceCollectionIdentifier",
        "nameSource",
        "documentSource",
        "counts",
        "countsCharLoc",
        "themes",
        "themesCharLoc",
        "locations",
        "locationsCharLoc",
        "persons",
        "personsCharLoc",
        "organizations",
        "organizationsCharLoc",
        "tone",
        "dates",
        "gcam",
        "urlImage",
        "urlImageRelated",
        "urlSocialMediaImageEmbeds",
        "urlSocialMediaVideoEmbeds",
        "quotations",
        "mentionedNamesCounts",
        "mentionedNumericsCounts",
        "translationInfo",
        "xmlExtras",
        "countArticles",
        "counts",
        "themes",
        "locations",
        "persons",
        "organizations",
        "tone",
        "idCAMEOEvents",
        "sources",
        "urlSources"
      )
    )
  return(schema_df)
}


#' Gets gkg count schema
#'
#' @return A `tibble` of GKG count column names and descriptions
#'
#' @examples
#' get_schema_gkg_counts()

get_schema_gkg_counts <- function() {
  counts_schema <-
    tibble(
      nameGDELT = c(
        "DATE",
        "NUMARTS",
        "COUNTTYPE",
        "NUMBER",
        "OBJECTTYPE",
        "GEO_TYPE",
        "GEO_FULLNAME",
        "GEO_COUNTRYCODE",
        "GEO_ADM1CODE",
        "GEO_LAT",
        "GEO_LONG",
        "GEO_FEATUREID",
        "CAMEOEVENTIDS",
        "SOURCES",
        "SOURCEURLS"
      ),
      nameActual = c(
        "dateEvent",
        "countArticles",
        "typeEvent",
        "countObject",
        "typeObject",
        "idTypeLocation",
        "location",
        "idCountry",
        "idADM1CodeAction",
        "latitude",
        "longitude",
        "idFeature",
        "idCAMEOEvents",
        "sources",
        "urlSources"
      )
    )
  return(counts_schema)
}

#' Gets gkg mention schema
#'
#' @return A `tibble` of GKG mention column names and descriptions
#'
#' @examples
#' get_schema_gkg_mentions()
get_schema_gkg_mentions <- function() {
  mentions_schema <-
    tibble(
      nameGDELT =
        c(
          "GLOBALEVENTID",
          "EventTimeDate",
          "MentionTimeDate",
          "MentionType",
          "MentionSourceName",
          "MentionIdentifier",
          "SentenceID",
          "Actor1CharOffset",
          "Actor2CharOffset",
          "ActionCharOffset",
          "InRawText",
          "Confidence",
          "MentionDocLen",
          "MentionDocTone",
          "MentionDocTranslationInfo",
          "Extras"
        ),
      nameActual =
        c(
          "idGlobalEvent",
          "dateEvent",
          "dateMention",
          "idMentionType",
          "nameSource",
          "documentSource",
          "idSentence",
          "charLocActor1",
          "charLocActor2",
          "charLocAction",
          "isRawText",
          "scoreGoldsteinConfidence",
          "lengthMentionedDocument",
          "toneMentionedDocument",
          "translationMentionedDocument",
          "extra"
        )
    )

  return(mentions_schema)
}

#' Get GDELT url data
#'
#' @param url Character string. URL to fetch data from.
#' @param file_directory Character string. Path to directory for file storage.
#' @param folder_name Character string. Name of the folder for file storage.
#' @param remove_existing_folder Logical. If \code{TRUE}, removes existing folder before download.
#' @param remove_files Logical. If \code{TRUE}, removes temporary files after processing.
#' @param empty_trash Logical. If \code{TRUE}, empties trash after file removal.
#' @param return_message Logical. If \code{TRUE}, prints progress messages to console.
#'
#' @return A `tibble` of parsed GDELT data from the specified URL
#' @export
#' @examples
#' \dontrun{
#' get_gdelt_url_data()
#' }
get_gdelt_url_data <-
  function(url = "http://data.gdeltproject.org/gdeltv2/20160531000000.gkg.csv.zip",
           file_directory = NULL,
           folder_name = 'gdelt_data',
           remove_existing_folder = TRUE,
           remove_files = TRUE,
           empty_trash = TRUE,
           return_message = TRUE) {
    tryCatch({
    use_tmp_file <-
      file_directory |>
      is_null()

    if (use_tmp_file) {
      tmp <-
        tempfile()

      url |>
        curl_download(url = ., tmp)

      con <-
        unzip(tmp)

      gdelt_cols <-
        con |>
        read_tsv(col_names = FALSE,
                 n_max = 1) |> ncol() |>
        suppressMessages() |>
        suppressWarnings()

      if (gdelt_cols == 16) {
        gdelt_data <-
          con |>
          read_tsv(col_names = FALSE) |>
          suppressWarnings() |>
          suppressMessages()

        names(gdelt_data) <-
          get_schema_gkg_mentions() |> dplyr::pull(nameActual)

        gdelt_data <-
          gdelt_data |>
          mutate(
            dateTimeEvent = dateEvent |> ymd_hms() |> lubridate::with_tz(Sys.timezone()),
            dateEvent = dateTimeEvent |> as.Date(),
            dateTimeMention = dateMention |> ymd_hms() |> lubridate::with_tz(Sys.timezone()),
            dateMention = dateTimeMention |> as.Date()
          ) |>
          dplyr::select(idGlobalEvent,
                        dateTimeEvent,
                        dateTimeMention,
                        everything()) |>
          dplyr::left_join(tibble(
            idMentionType = 1:6,
            typeMention = c('web', 'citation', 'core', 'dtic', 'jstor', 'nontext')
          )) |>
          dplyr::select(idGlobalEvent:idMentionType,
                        typeMention,
                        everything()) |>
          suppressMessages()

        gdelt_data <-
          gdelt_data |>
          dplyr::mutate(dplyr::across(dplyr::matches("is"), as.logical))

      }

      if (gdelt_cols == 15) {
        gdelt_data <-
          con |>
          read_tsv(col_names = TRUE) |>
          suppressWarnings() |>
          suppressMessages()

        names(gdelt_data) <-
          get_schema_gkg_counts() |> dplyr::pull(nameActual)

        gdelt_data <-
          gdelt_data |>
          dplyr::left_join(tibble(
            idTypeLocation = 1:5,
            typeLocation = c(
              'country',
              'usState',
              'usCity',
              'worldCity',
              'worldState'
            )
          )) |>
          suppressMessages() |>
          dplyr::mutate(
            idRecord = 1:n(),
            idGKG = dateEvent |> paste0('.', idRecord),
            urlSources = urlSources |> str_replace_all("<UDIV>", ';')
          ) |>
          dplyr::mutate(dateEvent = dateEvent |> ymd()) |>
          dplyr::select(dateEvent:idTypeLocation, typeLocation, everything()) |>
          dplyr::select(idRecord, idGKG, everything())
      }

      if (gdelt_cols == 61) {
        gdelt_data <-
          con |>
          read_tsv(col_names = FALSE) |>
          suppressWarnings() |>
          suppressMessages()

        names(gdelt_data) <-
          get_schema_gdelt_events() |> dplyr::pull(nameActual)

        gdelt_data <-
          gdelt_data |>
          dplyr::rename(dateTimeDocument = dateTimeDataAdded) |>
          dplyr::mutate(
            dateEvent = lubridate::ymd(dateEvent),
            dateTimeDocument = dateTimeDocument |> ymd_hms() |> lubridate::with_tz(Sys.timezone()),
            nameSource = urlSource |> urltools::domain() |> str_replace_all("www.", '')
          )

        gdelt_data <-
          gdelt_data |>
          dplyr::mutate(dplyr::across(dplyr::matches("is"), as.logical))
        gdelt_data <-
          gdelt_data |>
          dplyr::left_join(tibble(
            classQuad =  1:4,
            nameQuad =  c(
              'Verbal Cooperation',
              'Material Cooperation',
              'Verbal Conflict',
              'Material Conflict'
            )
          )) |>
          suppressMessages()
      }

      if (gdelt_cols == 57) {
        gdelt_data <-
          con |>
          read_tsv(col_names = FALSE) |>
          suppressWarnings() |>
          suppressMessages()

        names(gdelt_data) <-
          c(
            "idGlobalEvent",
            "dateEvent",
            "monthYearEvent",
            "yearEvent",
            "dateFraction",
            "codeActor1",
            "nameActor1",
            "codeISOActor1",
            "codeCAMEOGroupActor1",
            "codeCAMEOEthnicityActor1",
            "codeCAMEOReligionActor1",
            "codeCAMEOReligion2Actor1",
            "codeCAMEOTypeActor1",
            "codeCAMEOType2Actor1",
            "codeCAMEOType3Actor1",
            "codeActor2",
            "nameActor2",
            "codeISOActor2",
            "codeCAMEOGroupActor2",
            "codeCAMEOEthnicityActor2",
            "codeCAMEOReligionActor2",
            "codeCAMEOReligion2Actor2",
            "codeCAMEOTypeActor2",
            "codeCAMEOType2Actor2",
            "codeCAMEOType3Actor.3",
            "isRootEvent",
            "idCAMEOEvent",
            "idCAMEOEventBase",
            "idCAMEOEventRoot",
            "classQuad",
            "scoreGoldstein",
            "countMentions",
            "countSources",
            "countArticles",
            "avgTone",
            "idTypeLocationActor1",
            "locationActor1",
            "idCountryActor1",
            "idADM1CodeActor1",
            "latitudeActor1",
            "longitudeActor1",
            "idFeatureActor1",
            "idTypeLocationActor2",
            "locationActor2",
            "idCountryActor2",
            "idADM1CodeActor2",
            "latitudeActor2",
            "longitudeActor2",
            "idFeatureActor2",
            "idTypeLocationAction",
            "locationAction",
            "idCountryAction",
            "idADM1CodeAction",
            "latitudeAction",
            "longitudeAction",
            "idFeatureAction",
            "dateAdded"
          )

        gdelt_data <-
          gdelt_data |>
          dplyr::rename(dateDocument = dateAdded) |>
          dplyr::mutate(
            dateEvent = lubridate::ymd(dateEvent),
            dateDocument = lubridate::ymd(dateDocument)
          ) |>
          suppressWarnings()

        gdelt_data <-
          gdelt_data |>
          dplyr::mutate(dplyr::across(dplyr::matches("is"), as.logical))

        gdelt_data <-
          gdelt_data |>
          dplyr::left_join(tibble(
            classQuad =  1:4,
            nameQuad =  c(
              'Verbal Cooperation',
              'Material Cooperation',
              'Verbal Conflict',
              'Material Conflict'
            )
          )) |>
          suppressMessages()
      }

      if (gdelt_cols == 58) {
        gdelt_data <-
          con |>
          read_tsv(col_names = FALSE) |>
          suppressWarnings() |>
          suppressMessages()

        names(gdelt_data) <-
          c(
            "idGlobalEvent",
            "dateEvent",
            "monthYearEvent",
            "yearEvent",
            "dateFraction",
            "codeActor1",
            "nameActor1",
            "codeISOActor1",
            "codeCAMEOGroupActor1",
            "codeCAMEOEthnicityActor1",
            "codeCAMEOReligionActor1",
            "codeCAMEOReligion2Actor1",
            "codeCAMEOTypeActor1",
            "codeCAMEOType2Actor1",
            "codeCAMEOType3Actor1",
            "codeActor2",
            "nameActor2",
            "codeISOActor2",
            "codeCAMEOGroupActor2",
            "codeCAMEOEthnicityActor2",
            "codeCAMEOReligionActor2",
            "codeCAMEOReligion2Actor2",
            "codeCAMEOTypeActor2",
            "codeCAMEOType2Actor2",
            "codeCAMEOType3Actor.3",
            "isRootEvent",
            "idCAMEOEvent",
            "idCAMEOEventBase",
            "idCAMEOEventRoot",
            "classQuad",
            "scoreGoldstein",
            "countMentions",
            "countSources",
            "countArticles",
            "avgTone",
            "idTypeLocationActor1",
            "locationActor1",
            "idCountryActor1",
            "idADM1CodeActor1",
            "latitudeActor1",
            "longitudeActor1",
            "idFeatureActor1",
            "idTypeLocationActor2",
            "locationActor2",
            "idCountryActor2",
            "idADM1CodeActor2",
            "latitudeActor2",
            "longitudeActor2",
            "idFeatureActor2",
            "idTypeLocationAction",
            "locationAction",
            "idCountryAction",
            "idADM1CodeAction",
            "latitudeAction",
            "longitudeAction",
            "idFeatureAction",
            "dateAdded",
            "urlSource"
          )

        gdelt_data <-
          gdelt_data |>
          dplyr::rename(dateTimeDocument = dateAdded) |>
          dplyr::mutate(
            dateEvent = dateEvent |> lubridate::ymd(),
            nameSource = urlSource |> urltools::domain() |> str_replace_all("www.", '')
          ) |>
          suppressWarnings()

        gdelt_data <-
          gdelt_data |>
          dplyr::mutate(dplyr::across(dplyr::matches("is"), as.logical))

        gdelt_data <-
          gdelt_data |>
          dplyr::left_join(tibble(
            classQuad =  1:4,
            nameQuad =  c(
              'Verbal Cooperation',
              'Material Cooperation',
              'Verbal Conflict',
              'Material Conflict.'
            )
          )) |>
          suppressMessages()

      }

      if (gdelt_cols == 11) {
        gdelt_data <-
          con |>
          read_tsv(col_names = TRUE) |>
          suppressWarnings() |>
          suppressMessages()

        schema_df <-
          get_schema_gkg_general()

        names(gdelt_data) <-
          schema_df$nameActual[names(gdelt_data) |> match(schema_df$nameGDELT)]

        names(gdelt_data)[1] <-
          c('date')

        gdelt_data <-
          gdelt_data |>
          dplyr::mutate(
            idRecord = 1:n(),
            idGKG = date |> paste0('.', idRecord),
            date = date |> lubridate::ymd(),
            urlSources = urlSources |> str_replace_all("<UDIV>", ';')
          ) |>
          dplyr::select(idRecord, idGKG, everything())

      }

      if (gdelt_cols == 27) {
        gdelt_data <-
          con |>
          read_tsv(col_names = FALSE) |>
          suppressWarnings() |>
          suppressMessages()

        schema_df <-
          get_schema_gkg_general()

        names(gdelt_data) <-
          schema_df$nameActual[1:27]

        gdelt_data <-
          gdelt_data |>
          dplyr::mutate(
            idSourceCollectionIdentifier = idSourceCollectionIdentifier |> as.numeric(),
            isDocumentURL = ifelse(documentSource |> str_detect('http'), TRUE, FALSE)
          ) |>
          dplyr::select(idGKG:idSourceCollectionIdentifier,
                        isDocumentURL,
                        everything()) |>
          dplyr::rename(dateTimeDocument = dateDocument) |>
          dplyr::mutate(
            dateTimeDocument = dateTimeDocument |> lubridate::ymd_hms() |> lubridate::with_tz(Sys.timezone())
          ) |>
          separate(
            idGKG,
            into = c('dateTime', 'idDateTimeArticle'),
            sep = '\\-',
            remove = FALSE
          ) |>
          dplyr::select(-c(dateTime, translationInfo)) |>
          dplyr::mutate(
            idDateTimeArticle = idDateTimeArticle |> as.numeric(),
            domainSource = if_else(
              isDocumentURL == TRUE,
              documentSource |> urltools::domain(),
              nameSource
            )
          ) |>
          dplyr::select(idGKG:documentSource, domainSource, everything()) |>
          suppressMessages() |>
          suppressWarnings()

      }
      con |>
        unlink()
    }  else {
      only_folder <-
        !folder_name |> is_null() &
        file_directory |> is_null()
      if (only_folder) {
        file_directory <-
          getwd()
      }
      file_directory <-
        file_directory |>
        paste0('/', folder_name)

      file <-
        url |> basename()

      temp.dir <-
        file_directory

      file_path <-
        temp.dir |> str_split('/') |> list_c() |> paste0(collapse = '/')
      if (remove_existing_folder) {
        if (dir.exists(paths = file_path)) {
          "rm -R " |>
            paste0(temp.dir) |>
            system()
          if (empty_trash) {
            system('rm -rf ~/.Trash/*')
          }
        }
      }

      if (!dir.exists(paths = file_path)) {
        dir.create(temp.dir)
      }

      file <-
        temp.dir |>
        paste0('/', file)

      url |>
        curl_download(url = ., destfile = file)

      file |>
        unzip(exdir = paste0(temp.dir, '/'))

      dir_files <-
        temp.dir |>
        list.files()

      file_name <-
        dir_files[str_detect(dir_files, 'CSV|csv|TXT|txt|XLS|XLSX|xlsx|xls')]

      csv_file_loc <-
        paste0(file_directory, '/', file_name[!file_name |> str_detect("zip")])

      gdelt_cols <-
        csv_file_loc |>
        read_tsv(col_names = FALSE,
                 n_max = 1) |> ncol() |> suppressWarnings() |>
        as.character() |>
        parse_number() |>
        suppressMessages()

      if (gdelt_cols == 16) {
        gdelt_data <-
          csv_file_loc |>
          read_tsv(col_names = FALSE) |>
          suppressWarnings() |>
          suppressMessages()

        names(gdelt_data) <-
          get_schema_gkg_mentions() |> dplyr::pull(nameActual)

        gdelt_data <-
          gdelt_data |>
          mutate(
            dateTimeEvent = dateEvent |> ymd_hms() |> with_tz(Sys.timezone()),
            dateEvent = dateTimeEvent |> as.Date(),
            dateTimeMention = dateMention |> ymd_hms() |> with_tz(Sys.timezone()),
            dateMention = dateTimeMention |> as.Date()
          ) |>
          dplyr::select(idGlobalEvent,
                        dateTimeEvent,
                        dateTimeMention,
                        everything()) |>
          dplyr::left_join(tibble(
            idMentionType = 1:6,
            mention_type = c('web', 'citation', 'core', 'dtic', 'jstor', 'nontext')
          )) |>
          dplyr::select(idGlobalEvent:idMentionType,
                        mention_type,
                        everything()) |>
          suppressMessages()

        gdelt_data <-
          gdelt_data |>
          dplyr::mutate(dplyr::across(dplyr::matches("is"), as.logical))
      }

      if (gdelt_cols == 15) {
        gdelt_data <-
          csv_file_loc |>
          read_tsv(col_names = TRUE) |>
          suppressWarnings() |>
          suppressMessages()

        names(gdelt_data) <-
          get_schema_gkg_counts() |> dplyr::pull(nameActual)

        gdelt_data <-
          gdelt_data |>
          dplyr::left_join(tibble(
            idTypeLocation = 1:5,
            typeLocation = c(
              'country',
              'usState',
              'usCity',
              'worldCity',
              'worldState'
            )
          )) |>
          suppressMessages() |>
          dplyr::mutate(
            idRecord = 1:n(),
            idGKG = dateEvent |> paste0('.', idRecord),
            urlSources = urlSources |> str_replace_all("<UDIV>", ';')
          ) |>
          dplyr::mutate(dateEvent = dateEvent |> ymd()) |>
          dplyr::select(dateEvent:idTypeLocation, typeLocation, everything()) |>
          dplyr::select(idRecord, idGKG, everything())
      }

      if (gdelt_cols == 61) {
        gdelt_data <-
          csv_file_loc |>
          read_tsv(col_names = FALSE) |>
          suppressWarnings() |>
          suppressMessages()

        names(gdelt_data) <-
          get_schema_gdelt_events() |> dplyr::pull(nameActual)

        gdelt_data <-
          gdelt_data |>
          dplyr::rename(dateTimeDocument = dateTimeDataAdded) |>
          dplyr::mutate(
            dateEvent = lubridate::ymd(dateEvent),
            dateTimeDocument = dateTimeDocument |> ymd_hms() |> with_tz(Sys.timezone()),
            nameSource = urlSource |> domain() |> str_replace_all("www.", '')
          )

        gdelt_data <-
          gdelt_data |>
          dplyr::mutate(dplyr::across(dplyr::matches("is"), as.logical))

        gdelt_data <-
          gdelt_data |>
          dplyr::left_join(tibble(
            classQuad =  1:4,
            nameQuad =  c(
              'Verbal Cooperation',
              'Material Cooperation',
              'Verbal Conflict',
              'Material Conflict'
            )
          )) |>
          suppressMessages()
      }

      if (gdelt_cols == 57) {
        gdelt_data <-
          csv_file_loc |>
          read_tsv(col_names = FALSE) |>
          suppressWarnings() |>
          suppressMessages()

        names(gdelt_data) <-
          c(
            "idGlobalEvent",
            "dateEvent",
            "monthYearEvent",
            "yearEvent",
            "dateFraction",
            "codeActor1",
            "nameActor1",
            "codeISOActor1",
            "codeCAMEOGroupActor1",
            "codeCAMEOEthnicityActor1",
            "codeCAMEOReligionActor1",
            "codeCAMEOReligion2Actor1",
            "codeCAMEOTypeActor1",
            "codeCAMEOType2Actor1",
            "codeCAMEOType3Actor1",
            "codeActor2",
            "nameActor2",
            "codeISOActor2",
            "codeCAMEOGroupActor2",
            "codeCAMEOEthnicityActor2",
            "codeCAMEOReligionActor2",
            "codeCAMEOReligion2Actor2",
            "codeCAMEOTypeActor2",
            "codeCAMEOType2Actor2",
            "codeCAMEOType3Actor.3",
            "isRootEvent",
            "idCAMEOEvent",
            "idCAMEOEventBase",
            "idCAMEOEventRoot",
            "classQuad",
            "scoreGoldstein",
            "countMentions",
            "countSources",
            "countArticles",
            "avgTone",
            "idTypeLocationActor1",
            "locationActor1",
            "idCountryActor1",
            "idADM1CodeActor1",
            "latitudeActor1",
            "longitudeActor1",
            "idFeatureActor1",
            "idTypeLocationActor2",
            "locationActor2",
            "idCountryActor2",
            "idADM1CodeActor2",
            "latitudeActor2",
            "longitudeActor2",
            "idFeatureActor2",
            "idTypeLocationAction",
            "locationAction",
            "idCountryAction",
            "idADM1CodeAction",
            "latitudeAction",
            "longitudeAction",
            "idFeatureAction",
            "dateAdded"
          )

        gdelt_data <-
          gdelt_data |>
          dplyr::rename(dateDocument = dateAdded) |>
          dplyr::mutate(
            dateEvent = lubridate::ymd(dateEvent),
            dateDocument = lubridate::ymd(dateDocument)
          ) |>
          suppressWarnings()

        gdelt_data <-
          gdelt_data |>
          dplyr::mutate(dplyr::across(dplyr::matches("is"), as.logical))

        gdelt_data <-
          gdelt_data |>
          dplyr::left_join(tibble(
            classQuad =  1:4,
            nameQuad =  c(
              'Verbal Cooperation',
              'Material Cooperation',
              'Verbal Conflict',
              'Material Conflict'
            )
          )) |>
          suppressMessages()
      }

      if (gdelt_cols == 58) {
        load_needed_packages(c('urltools'))
        gdelt_data <-
          csv_file_loc |>
          read_tsv(col_names = FALSE) |>
          suppressWarnings() |>
          suppressMessages()

        names(gdelt_data) <-
          c(
            "idGlobalEvent",
            "dateEvent",
            "monthYearEvent",
            "yearEvent",
            "dateFraction",
            "codeActor1",
            "nameActor1",
            "codeISOActor1",
            "codeCAMEOGroupActor1",
            "codeCAMEOEthnicityActor1",
            "codeCAMEOReligionActor1",
            "codeCAMEOReligion2Actor1",
            "codeCAMEOTypeActor1",
            "codeCAMEOType2Actor1",
            "codeCAMEOType3Actor1",
            "codeActor2",
            "nameActor2",
            "codeISOActor2",
            "codeCAMEOGroupActor2",
            "codeCAMEOEthnicityActor2",
            "codeCAMEOReligionActor2",
            "codeCAMEOReligion2Actor2",
            "codeCAMEOTypeActor2",
            "codeCAMEOType2Actor2",
            "codeCAMEOType3Actor.3",
            "isRootEvent",
            "idCAMEOEvent",
            "idCAMEOEventBase",
            "idCAMEOEventRoot",
            "classQuad",
            "scoreGoldstein",
            "countMentions",
            "countSources",
            "countArticles",
            "avgTone",
            "idTypeLocationActor1",
            "locationActor1",
            "idCountryActor1",
            "idADM1CodeActor1",
            "latitudeActor1",
            "longitudeActor1",
            "idFeatureActor1",
            "idTypeLocationActor2",
            "locationActor2",
            "idCountryActor2",
            "idADM1CodeActor2",
            "latitudeActor2",
            "longitudeActor2",
            "idFeatureActor2",
            "idTypeLocationAction",
            "locationAction",
            "idCountryAction",
            "idADM1CodeAction",
            "latitudeAction",
            "longitudeAction",
            "idFeatureAction",
            "dateAdded",
            "urlSource"
          )

        gdelt_data <-
          gdelt_data |>
          dplyr::rename(dateTimeDocument = dateAdded) |>
          dplyr::mutate(
            dateEvent = lubridate::ymd(dateEvent),
            dateTimeDocument = dateTimeDocument |> lubridate::ymd_hms() |> with_tz(Sys.timezone()),
            dateDocument = dateTimeDocument |> as.Date(),
            nameSource = urlSource |> domain() |> str_replace_all("www.", '')
          ) |>
          suppressWarnings()

        gdelt_data <-
          gdelt_data |>
          dplyr::mutate(dplyr::across(dplyr::matches("is"), as.logical))

        gdelt_data <-
          gdelt_data |>
          dplyr::left_join(tibble(
            classQuad =  1:4,
            nameQuad =  c(
              'Verbal Cooperation',
              'Material Cooperation',
              'Verbal Conflict',
              'Material Conflict.'
            )
          )) |>
          suppressMessages()

      }

      if (gdelt_cols == 11) {
        gdelt_data <-
          csv_file_loc |>
          read_tsv(col_names = TRUE) |>
          suppressWarnings() |>
          suppressMessages()

        schema_df <-
          get_schema_gkg_general()

        names(gdelt_data) <-
          schema_df$nameActual[names(gdelt_data) |> match(schema_df$nameGDELT)]

        names(gdelt_data)[1] <-
          c('date')

        gdelt_data <-
          gdelt_data |>
          dplyr::mutate(
            idRecord = 1:n(),
            idGKG = date |> paste0('.', idRecord),
            date = date |> lubridate::ymd(),
            urlSources = urlSources |> str_replace_all("<UDIV>", ';')
          ) |>
          dplyr::select(idRecord, idGKG, everything())

      }

      if (gdelt_cols == 27) {
        gdelt_data <-
          csv_file_loc |>
          read_tsv(col_names = FALSE) |>
          suppressWarnings() |>
          suppressMessages()

        schema_df <-
          get_schema_gkg_general()

        names(gdelt_data) <-
          schema_df$nameActual[1:27]

        gdelt_data <-
          gdelt_data |>
          dplyr::mutate(
            idSourceCollectionIdentifier = idSourceCollectionIdentifier |> as.numeric(),
            isDocumentURL = ifelse(documentSource |> str_detect('http'), TRUE, FALSE)
          ) |>
          dplyr::select(idGKG:idSourceCollectionIdentifier,
                        isDocumentURL,
                        everything()) |>
          dplyr::rename(dateTimeDocument = dateDocument) |>
          dplyr::mutate(dateTimeDocument = dateTimeDocument |> lubridate::ymd_hms() |> with_tz(Sys.timezone())) |>
          separate(
            idGKG,
            into = c('dateTime', 'idDateTimeArticle'),
            sep = '\\-',
            remove = FALSE
          ) |>
          dplyr::select(-c(dateTime, translationInfo)) |>
          dplyr::mutate(
            idDateTimeArticle = idDateTimeArticle |> as.numeric(),
            domainSource = if_else(
              isDocumentURL == TRUE,
              documentSource |> urltools::domain(),
              nameSource
            )
          ) |>
          dplyr::select(idGKG:documentSource, domainSource, everything()) |>
          suppressMessages() |>
          suppressWarnings()

      }

      if (remove_files) {
        "rm -R " |>
          paste0(temp.dir) |>
          system()
        if (empty_trash) {
          system('rm -rf ~/.Trash/*')
        }
      }


    }
    if ('idADM1CodeActor1' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data |>
        mutate(idADM1CodeActor1 = idADM1CodeActor1 |> as.character())

    }

    if ('idADM1CodeActor2' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data |>
        mutate(idADM1CodeActor2 = idADM1CodeActor2 |> as.character())

    }

    if ('idADM2CodeActor1' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data |>
        mutate(idADM2CodeActor1 = idADM2CodeActor1 |> as.character())

    }

    if ('idADM2CodeActor2' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data |>
        mutate(idADM2CodeActor2 = idADM2CodeActor2 |> as.character())

    }

    if ('idADM1CodeAction' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data |>
        mutate(idADM1CodeAction = idADM1CodeAction |> as.character())

    }

    if ('idADM2CodeAction' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data |>
        mutate(idADM2CodeAction = idADM2CodeAction |> as.character())

    }

    if ('idCAMEOEvent' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data |>
        dplyr::mutate(dplyr::across(dplyr::matches('idCAMEOEvent'), ~as.numeric(as.character(.x))))

    }

    if ('idFeatureActor' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data |>
        dplyr::mutate(dplyr::across(dplyr::matches('idFeatureActor'), as.character))

    }

    if (return_message) {
      cli::cli_alert_info("Downloaded, parsed and imported {url}")
    }
    return(gdelt_data)
    }, error = function(e) {
      cli::cli_alert_danger("Failed: {e$message}")
      return(tibble())
    })
  }

#' Gets clean count data
#'
#' @param all_counts A \code{tibble} of count data to clean.
#' @param count_col Character string. Name of the column containing counts.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#'
#' @return A `tibble` of cleaned count data in long or wide format
#'
#' @examples
#' \dontrun{
#' get_clean_count_data()
#' }
get_clean_count_data <-
  function(all_counts,
           extra_key = NA,
           count_col = 'idArticle.tone',
           return_wide = FALSE) {
    if (!extra_key |> is.na()) {
      clean_data <-
        all_counts |>
        dplyr::rename(count = !!rlang::sym(count_col),
                      ek = !!rlang::sym(extra_key)) |>
        group_by(idGKG) |>
        mutate(count = 1:n()) |>
        ungroup() |>
        mutate(count = count - 1) |>
        tidyr::pivot_longer(cols = -c(idGKG, count, ek), names_to = "item", values_to = "value", values_drop_na = TRUE) |>
        mutate(item = ifelse(count == 0, item, item |> paste0(count)),
               value = value |> str_trim()) |>
        separate(
          idGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = FALSE
        ) |>
        mutate(dateTime = dateTime |> as.numeric()) |>
        arrange(dateTime, count) |>
        dplyr::select(-c(dateTime, GKG))
    } else {
      clean_data <-
        all_counts |>
        dplyr::rename(count = !!rlang::sym(count_col)) |>
        group_by(idGKG) |>
        mutate(count = 1:n()) |>
        ungroup() |>
        mutate(count = count - 1)
      if ('charLoc' %in% names(clean_data)) {
        clean_data <-
          clean_data |>
          tidyr::pivot_longer(cols = -c(idGKG, count, charLoc), names_to = "item", values_to = "value", values_drop_na = TRUE) |>
          mutate(
            item = ifelse(count == 0, item, item |> paste0(count)),
            value = value |> str_trim()
          ) |>
          distinct() |>
          separate(
            idGKG,
            into = c('GKG', 'dateTime'),
            sep = '\\-',
            remove = FALSE
          ) |>
          mutate(dateTime = dateTime |> as.numeric()) |>
          arrange(dateTime, count) |>
          dplyr::select(-c(dateTime, GKG))
      } else {
        clean_data <-
          clean_data |>
          tidyr::pivot_longer(cols = -c(idGKG, count), names_to = "item", values_to = "value", values_drop_na = TRUE) |>
          mutate(
            item = ifelse(count == 0, item, item |> paste0(count)),
            value = value |> str_trim()
          ) |>
          distinct() |>
          separate(
            idGKG,
            into = c('GKG', 'dateTime'),
            sep = '\\-',
            remove = FALSE
          ) |>
          mutate(dateTime = dateTime |> as.numeric()) |>
          arrange(dateTime, count) |>
          dplyr::select(-c(dateTime, GKG))
      }
    }

    if (return_wide) {
      if (!extra_key |> is.na()) {
        names_order <-
          c('idGKG', 'ek',  clean_data$item |> unique())
      } else {
        names_order <-
          c('idGKG',  clean_data$item |> unique())
      }

      clean_data <-
        clean_data |>
        dplyr::select(-count) |>
        tidyr::pivot_wider(names_from = item, values_from = value)

      keywords <-
        c(
          'amountValue',
          'latitude',
          'scoreGoldstein',
          'articleWordCount',
          'longitude',
          'countEvent',
          'idTypeLocation',
          'charLoc',
          'length',
          'count',
          'month',
          'day',
          'year',
          'score'
        )

      length_nums <-
        clean_data |>
        dplyr::select(dplyr::matches(keywords |> paste0(collapse = '|'))) |>
        dplyr::select(-dplyr::matches('namePerson|idCountry')) |>
        names() |> length()

      if (length_nums > 0) {
        numeric_vars <-
          clean_data |>
          dplyr::select(dplyr::matches(paste0(keywords, collapse = '|'))) |>
          dplyr::select(-dplyr::matches('namePerson|idCountry')) |>
          names()

        clean_data <-
          clean_data |>
          dplyr::mutate(dplyr::across(dplyr::all_of(numeric_vars), as.numeric))

      }

      clean_data <-
        clean_data |>
        dplyr::select(dplyr::all_of(names_order)) |>
        separate(
          idGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = FALSE
        ) |>
        mutate(dateTime = dateTime |> as.numeric()) |>
        arrange(dateTime) |>
        dplyr::select(-c(GKG, dateTime))

      if (!extra_key |> is.na()) {
        names(clean_data)[2] <-
          extra_key
      }
    } else {
      clean_data <-
        clean_data |>
        dplyr::select(-count) |>
        separate(
          idGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = FALSE
        ) |>
        mutate(dateTime = dateTime |> as.numeric()) |>
        arrange(dateTime) |>
        dplyr::select(-c(GKG, dateTime))

    }

    return(clean_data)
  }

#' Returns long or wide mentioned numerics from a GKG data frame
#'
#' @param gdelt_data A \code{tibble} of GDELT data to parse.
#' @param filter_na Logical. If \code{TRUE}, removes rows with \code{NA} values.
#' @param include_char_locg Logical. If \code{TRUE}, includes character location data.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#' @return A `tibble` of parsed mentioned numerics from GKG data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_gkg_mentioned_numerics()
#' }
parse_gkg_mentioned_numerics <- function(gdelt_data,
                                         filter_na = TRUE,
                                         include_char_loc = TRUE,
                                         return_wide = TRUE) {
  parse_mentioned_numerics <-
    function(field = "170,Scotland Road,1600;170,Scotland Road,2475;",
             return_wide = FALSE) {
      options(scipen = 99999)
      if (field |> is.na()) {
        if (return_wide) {
          field_data <-
            tibble(
              amountValue1 = NA,
              amountTerm1 = NA,
              charLoc = NA
            )
        } else {
          field_data <-
            tibble(
              amountValue = NA,
              amountTerm = NA,
              charLoc = NA,
              idArticleNumericItem = 1
            )
        }
      } else {
        fields <-
          field |>
          str_split('\\;') |>
          list_c() |>
          (\(x) x[x != ''])()

        fields_df <-
          tibble(field = fields) |>
          dplyr::mutate(idArticleNumericItem = 1:n()) |>
          separate(
            field,
            into = c('amountValue', 'amountTerm', 'charLoc'),
            sep = '\\,'
          ) |>
          dplyr::mutate(amountTerm = amountTerm |> str_trim()) |>
          suppressMessages() |>
          suppressWarnings()

        if (return_wide) {
          fields_df <-
            fields_df |>
            tidyr::pivot_longer(cols = -idArticleNumericItem, names_to = "item", values_to = "value") |>
            arrange(idArticleNumericItem) |>
            unite(item, item, idArticleNumericItem, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df |>
            tidyr::pivot_wider(names_from = item, values_from = value) |>
            dplyr::select(dplyr::all_of(order_fields))

          field_data <-
            field_data |>
            dplyr::mutate(dplyr::across(dplyr::matches('amountValue|charLoc'), ~parse_number(as.character(.x))))

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data |>
            dplyr::select(idArticleNumericItem,
                          amountValue,
                          amountTerm,
                          charLoc) |>
            dplyr::mutate(charLoc = charLoc |> as.numeric(),
                          amountValue = amountValue |> as.numeric())
        }
      }

      return(field_data)
    }

  if (!'mentionedNumericsCounts' %in% names(gdelt_data)) {
    stop("Sorry missing numeric count column")
  }
  counts_data <-
    gdelt_data |>
    dplyr::select(idGKG, mentionedNumericsCounts)

  if (filter_na) {
    counts_data <-
      counts_data |>
      dplyr::filter(!mentionedNumericsCounts |> is.na())
  }

  all_counts <-
    seq_along(counts_data$mentionedNumericsCounts) |>
    map_dfr(function(x) {
      parse_mentioned_numerics(field = counts_data$mentionedNumericsCounts[x],
                               return_wide = FALSE) |>
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) |>
    dplyr::select(idGKG, everything())

  if (include_char_loc == FALSE) {
    all_counts <-
      all_counts |>
      dplyr::select(-charLoc)
  }

  all_counts <-
    all_counts |>
    get_clean_count_data(count_col = 'idArticleNumericItem', return_wide = return_wide) |>
    separate(
      idGKG,
      into = c('GKG', 'dateTime'),
      sep = '\\-',
      remove = FALSE
    ) |>
    mutate(dateTime = dateTime |> as.numeric()) |>
    arrange(dateTime) |>
    dplyr::select(-c(dateTime, GKG)) |>
    suppressWarnings()

  if (!return_wide) {
    all_counts <-
      all_counts |>
      .resolve_long_names()
  }

  return(all_counts)
}

#' Returns long or wide mentioned people from a GKG data frame
#'
#' @param gdelt_data A \code{tibble} of GDELT data to parse.
#' @param people_column options \code{c('person', 'persons', 'persons.count', 'personsCharLoc', 'charLoc'))}
#' @param filter_na Logical. If \code{TRUE}, removes rows with \code{NA} values.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#'
#' @return A `tibble` of parsed mentioned people from GKG data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_gkg_mentioned_people()
#' }
parse_gkg_mentioned_people <- function(gdelt_data,
                                       people_column = 'personsCharLoc',
                                       filter_na = TRUE,
                                       return_wide = TRUE) {
  people_count_cols <-
    c('person',
      'persons',
      'persons.count',
      'personsCharLoc',
      'charLoc')

  if (!people_column %in% people_count_cols) {
    stop("Sorry people column can only be\n" |>
           paste0(paste0(people_count_cols, collapse = '\n')))
  }

  if (people_column %in% c('person', 'persons')) {
    people_column <-
      'persons'
  }

  if (people_column %in% c('persons.count', 'personsCharLoc', 'charLoc')) {
    people_column <-
      'personsCharLoc'
  }

  parse_mentioned_people_counts <-
    function(field = "Chaudhry Nisar Ali Khan,63",
             return_wide = FALSE) {
      options(scipen = 99999)
      if (field |> is.na()) {
        if (return_wide) {
          field_data <-
            tibble(namePerson1 = NA, charLoc1 = NA)
        } else {
          field_data <-
            tibble(
              namePerson = NA,
              charLoc = NA,
              idArticlePerson = 1
            )
        }
      } else {
        fields <-
          field |>
          str_split('\\;') |>
          list_c() |>
          (\(x) x[x != ''])()

        fields_df <-
          tibble(field = fields) |>
          dplyr::mutate(idArticlePerson = 1:n()) |>
          separate(field,
                   into = c('namePerson', 'charLoc'),
                   sep = '\\,') |>
          suppressWarnings() |>
          suppressMessages()

        if (return_wide) {
          fields_df <-
            fields_df |>
            tidyr::pivot_longer(cols = -idArticlePerson, names_to = "item", values_to = "value") |>
            arrange(idArticlePerson) |>
            unite(item, item, idArticlePerson, sep = '.') |>
            suppressWarnings()

          order_fields <-
            fields_df$item

          field_data <-
            fields_df |>
            tidyr::pivot_wider(names_from = item, values_from = value) |>
            dplyr::select(dplyr::all_of(order_fields))

          field_data <-
            field_data |>
            dplyr::mutate(dplyr::across(dplyr::matches('charLoc'), ~parse_number(as.character(.x))))

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data |>
            dplyr::select(idArticlePerson, namePerson, charLoc) |>
            dplyr::mutate(charLoc = charLoc |> as.numeric())
        }
      }

      return(field_data)
    }

  if (!people_column %in% names(gdelt_data)) {
    stop("Sorry missing people column")
  }

  col_names <-
    c('idGKG', people_column)

  counts_data <-
    gdelt_data |>
    dplyr::select(dplyr::all_of(col_names))

  names(counts_data)[2] <-
    'people_col'

  if (filter_na) {
    counts_data <-
      counts_data |>
      dplyr::filter(!people_col |> is.na())
  }

  all_counts <-
    seq_along(counts_data$people_col) |>
    map_dfr(function(x) {
      parse_mentioned_people_counts(field = counts_data$people_col[x],
                                    return_wide = FALSE) |>
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) |>
    dplyr::select(idGKG, everything())

  if (people_column == 'persons') {
    all_counts <-
      all_counts |>
      dplyr::select(-charLoc)
  }

  all_counts <-
    all_counts |>
    get_clean_count_data(count_col = 'idArticlePerson', return_wide = return_wide) |>
    separate(
      idGKG,
      into = c('GKG', 'dateTime'),
      sep = '\\-',
      remove = FALSE
    ) |>
    mutate(dateTime = dateTime |> as.numeric()) |>
    arrange(dateTime) |>
    dplyr::select(-c(dateTime, GKG)) |>
    suppressWarnings()

  if (!return_wide) {
    all_counts <-
      all_counts |>
      .resolve_long_names()
  }

  return(all_counts)
}

#' Returns long or wide mentioned organizations from a GKG data frame
#'
#' @param gdelt_data A \code{tibble} of GDELT data to parse.
#' @param organization_column options \code{c('organization', 'organizations', 'organizations.count', 'organizationsCharLoc', 'charLoc'))}
#' @param filter_na Logical. If \code{TRUE}, removes rows with \code{NA} values.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#'
#' @return A `tibble` of parsed mentioned organizations from GKG data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_gkg_mentioned_organizations()
#' }
parse_gkg_mentioned_organizations <- function(gdelt_data,
                                              organization_column = 'organizations',
                                              filter_na = TRUE,
                                              return_wide = TRUE) {
  organization_count_cols <-
    c(
      'organization',
      'organizations',
      'organizations.count',
      'organizationsCharLoc',
      'charLoc'
    )

  if (!organization_column %in% organization_count_cols) {
    stop("Sorry people column can only be\n" |>
           paste0(paste0(organization_count_cols, collapse = '\n')))
  }

  if (organization_column %in% c('organization', 'organizations')) {
    organization_column <-
      'organizations'
  }

  if (organization_column %in% c('organizations.count',
                                 'organizationsCharLoc',
                                 'charLoc')) {
    organization_column <-
      'personsCharLoc'
  }

  parse_mentioned_organization_counts <-
    function(field = "Twitter,2912;Pegasystems,169;Pegasystems,1238;Pegasystems,1829;Pegasystems,2079;Pegasystems,2888;Nasdaq,193;Nasdaq,2086;Ilena Ryan Pegasystems Inc,2892;Pegasystems Inc,173;Pegasystems Inc,2892",
             return_wide = FALSE) {
      options(scipen = 99999)
      if (field |> is.na()) {
        if (return_wide) {
          field_data <-
            tibble(nameOrganization1 = NA,
                       charLoc1 = NA)
        } else {
          field_data <-
            tibble(
              nameOrganization = NA,
              charLoc = NA,
              idArticle.organization = 1
            )
        }
      } else {
        fields <-
          field |>
          str_split('\\;') |>
          list_c() |>
          (\(x) x[x != ''])()

        fields_df <-
          tibble(field = fields) |>
          dplyr::mutate(idArticle.organization = 1:n()) |>
          separate(field,
                   into = c('nameOrganization', 'charLoc'),
                   sep = '\\,') |>
          suppressMessages() |>
          suppressWarnings()

        if (return_wide) {
          fields_df <-
            fields_df |>
            tidyr::pivot_longer(cols = -idArticle.organization, names_to = "item", values_to = "value") |>
            arrange(idArticle.organization) |>
            unite(item, item, idArticle.organization, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df |>
            tidyr::pivot_wider(names_from = item, values_from = value) |>
            dplyr::select(dplyr::all_of(order_fields))

          field_data <-
            field_data |>
            dplyr::mutate(dplyr::across(dplyr::matches('charLoc'), ~parse_number(as.character(.x))))
        } else {
          field_data <-
            fields_df

          field_data <-
            field_data |>
            dplyr::select(idArticle.organization, nameOrganization, charLoc) |>
            dplyr::mutate(charLoc = charLoc |> as.numeric())
        }
      }

      return(field_data)
    }

  if (!organization_column %in% names(gdelt_data)) {
    stop("Sorry missing organization column")
  }

  col_names <-
    c('idGKG', organization_column)

  counts_data <-
    gdelt_data |>
    dplyr::select(dplyr::all_of(col_names))

  names(counts_data)[2] <-
    'org_col'

  if (filter_na) {
    counts_data <-
      counts_data |>
      dplyr::filter(!org_col |> is.na())
  }

  all_counts <-
    seq_along(counts_data$org_col) |>
    map_dfr(function(x) {
      parse_mentioned_organization_counts(field = counts_data$org_col[x],
                                          return_wide = FALSE) |>
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) |>
    dplyr::select(idGKG, everything())

  if (organization_column == 'organizations') {
    all_counts <-
      all_counts |>
      dplyr::select(-charLoc)
  }

  all_counts <-
    all_counts |>
    get_clean_count_data(count_col = 'idArticle.organization', return_wide = return_wide) |>
    separate(
      idGKG,
      into = c('GKG', 'dateTime'),
      sep = '\\-',
      remove = FALSE
    ) |>
    mutate(dateTime = dateTime |> as.numeric()) |>
    arrange(dateTime) |>
    dplyr::select(-c(dateTime, GKG)) |>
    suppressWarnings()

  if (!return_wide) {
    all_counts <-
      all_counts |>
      .resolve_long_names()
  }


  return(all_counts)
}

#' Returns mentioned names from a GKG data frame.
#'
#' @param gdelt_data A \code{tibble} of GDELT data to parse.
#' @param filter_na Logical. If \code{TRUE}, removes rows with \code{NA} values.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#'
#' @return A `tibble` of parsed mentioned names from GKG data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_gkg_mentioned_names()
#' }
parse_gkg_mentioned_names <- function(gdelt_data,
                                      filter_na = TRUE,
                                      return_wide = TRUE) {
  parse_mentioned_names_counts <-
    function(field = "Interior Minister Chaudhry Nisar Ali Khan,47;Mullah Mansour,87;Afghan Taliban,180;Mullah Mansour,382;Mullah Mansor,753;Mullah Mansour,815;Mullah Mansour,1025",
             return_wide = return_wide) {
      options(scipen = 99999)
      if (field |> is.na()) {
        if (return_wide) {
          field_data <-
            tibble(nameMentionedName1 = NA,
                       charLoc1 = NA)
        } else {
          field_data <-
            tibble(
              nameMentionedName = NA,
              charLoc = NA,
              idArticleMentionedName = 1
            )
        }
      } else {
        fields <-
          field |>
          str_split('\\;') |>
          list_c() |>
          (\(x) x[x != ''])()

        fields_df <-
          tibble(field = fields) |>
          dplyr::mutate(idArticleMentionedName = 1:n()) |>
          separate(field,
                   into = c('nameMentionedName', 'charLoc'),
                   sep = '\\,') |>
          mutate(charLoc = charLoc |> as.numeric()) |>
          suppressMessages() |>
          suppressWarnings()

        if (return_wide) {
          fields_df <-
            fields_df |>
            tidyr::pivot_longer(cols = -c(idArticleMentionedName, charLoc), names_to = "item", values_to = "value") |>
            arrange(idArticleMentionedName) |>
            unite(item, item, idArticleMentionedName, sep = '')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df |>
            dplyr::select(-dplyr::matches("charLoc")) |>
            tidyr::pivot_wider(names_from = item, values_from = value) |>
            dplyr::select(dplyr::all_of(order_fields))

          field_data <-
            field_data |>
            dplyr::mutate(dplyr::across(dplyr::matches('charLoc'), ~parse_number(as.character(.x))))
        } else {
          field_data <-
            fields_df

          field_data <-
            field_data |>
            dplyr::select(idArticleMentionedName,
                          charLoc,
                          nameMentionedName)

        }
      }

      return(field_data)
    }

  if (!'mentionedNamesCounts' %in% names(gdelt_data)) {
    stop("Sorry missing metioned name column")
  }
  counts_data <-
    gdelt_data |>
    dplyr::select(idGKG, mentionedNamesCounts)

  all_counts <-
    seq_along(counts_data$mentionedNamesCounts) |>
    map_dfr(function(x) {
      parse_mentioned_names_counts(field = counts_data$mentionedNamesCounts[x],
                                   return_wide = FALSE) |>
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) |>
    dplyr::select(idGKG, everything())

  if (filter_na) {
    if ('nameMentionedName' %in% names(all_counts)) {
      all_counts <-
        all_counts |>
        dplyr::filter(!nameMentionedName |> is.na())
    }
  }

  all_counts <-
    all_counts |>
    get_clean_count_data(count_col = 'idArticleMentionedName',
                         return_wide = FALSE) |>
    separate(
      idGKG,
      into = c('GKG', 'dateTime'),
      sep = '\\-',
      remove = FALSE
    ) |>
    mutate(dateTime = dateTime |> as.numeric()) |>
    select(-dplyr::matches("charLoc")) |>
    arrange(dateTime) |>
    dplyr::select(-c(dateTime, GKG)) |>
    suppressWarnings()

  if (return_wide) {
    all_counts <-
      all_counts |>
      tidyr::pivot_wider(names_from = item, values_from = value)
  }

  if (!return_wide) {
    all_counts <-
      all_counts |>
      .resolve_long_names()
  }

  return(all_counts)
}

#' Returns mentioned themes from a gkg data frame
#'
#' @param gdelt_data A \code{tibble} of GDELT data to parse.
#' @param filter_na Logical. If \code{TRUE}, removes rows with \code{NA} values.
#' @param organization_column options \code{c('theme', 'themes', 'countThemes', 'themesCharLoc', 'charLoc'))}
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#' @return A `tibble` of parsed mentioned themes from GKG data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_gkg_mentioned_themes()
#' }

parse_gkg_mentioned_themes <- function(gdelt_data,
                                       filter_na = TRUE,
                                       theme_column = 'themes',
                                       return_wide = TRUE) {
  theme_count_cols <-
    c('theme',
      'themes',
      'countThemes',
      'themesCharLoc',
      'charLoc')

  if (!theme_column %in% theme_count_cols) {
    stop("Sorry theme column can only be\n" |>
           paste0(paste0(theme_count_cols, collapse = '\n')))
  }

  if (theme_column %in% c('theme', 'themes')) {
    theme_column <-
      'themes'
  }

  if (theme_column %in% c('countThemes', 'themesCharLoc', 'charLoc')) {
    theme_column <-
      'themesCharLoc'
  }

  parse_mentioned_names_themes <-
    function(field = "https://youtube.com/esctodaytv;https://youtube.com/embed/5ymFX91HwM0?wmode=transparent&#038;modestbranding=1&#038;autohide=1&#038;showinfo=0&#038;rel=0;",
             return_wide = FALSE) {
      options(scipen = 99999)
      if (field |> is.na()) {
        if (return_wide) {
          field_data <-
            tibble(codeTheme1 = NA, charLoc1 = NA)
        } else {
          field_data <-
            tibble(
              codeTheme = NA,
              charLoc = NA,
              idArticleGKGTheme = 1
            )
        }
      } else {
        fields <-
          field |>
          str_split('\\;') |>
          list_c() |>
          (\(x) x[x != ''])()

        fields_df <-
          tibble(field = fields) |>
          dplyr::mutate(idArticleGKGTheme = 1:n()) |>
          separate(field,
                   into = c('codeTheme', 'charLoc'),
                   sep = '\\,') |>
          suppressWarnings()

        if (return_wide) {
          fields_df <-
            fields_df |>
            tidyr::pivot_longer(cols = -idArticleGKGTheme, names_to = "item", values_to = "value") |>
            arrange(idArticleGKGTheme) |>
            unite(item, item, idArticleGKGTheme, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df |>
            tidyr::pivot_wider(names_from = item, values_from = value) |>
            dplyr::select(dplyr::all_of(order_fields))

          field_data <-
            field_data |>
            dplyr::mutate(dplyr::across(dplyr::matches('charLoc'), ~parse_number(as.character(.x))))
        } else {
          field_data <-
            fields_df

          field_data <-
            field_data |>
            dplyr::select(idArticleGKGTheme, codeTheme, charLoc) |>
            dplyr::mutate(charLoc = charLoc |> as.numeric())
        }
      }

      return(field_data)
    }

  if (!theme_column %in% names(gdelt_data)) {
    stop("Sorry missing organization column")
  }

  col_names <-
    c('idGKG', theme_column)

  counts_data <-
    gdelt_data |>
    dplyr::select(dplyr::all_of(col_names))
  names(counts_data)[2] <-
    'theme_col'

  if (filter_na) {
    counts_data <-
      counts_data |>
      dplyr::filter(!theme_col |> is.na())
  }

  all_counts <-
    seq_along(counts_data$theme_col) |>
    map_dfr(function(x) {
      parse_mentioned_names_themes(field = counts_data$theme_col[x],
                                   return_wide = FALSE) |>
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) |>
    dplyr::select(idGKG, everything())

  if (theme_column == 'themes') {
    all_counts <-
      all_counts |>
      dplyr::select(-charLoc)
  }

  all_counts <-
    all_counts |>
    get_clean_count_data(count_col = 'idArticleGKGTheme', return_wide = return_wide) |>
    separate(
      idGKG,
      into = c('GKG', 'dateTime'),
      sep = '\\-',
      remove = FALSE
    ) |>
    mutate(dateTime = dateTime |> as.numeric()) |>
    arrange(dateTime) |>
    dplyr::select(-c(dateTime, GKG)) |>
    suppressWarnings()

  if (!return_wide) {
    all_counts <-
      all_counts |>
      .resolve_long_names()
  }

  return(all_counts)
}

#' Returns social embed information from a gkg data frame
#'
#' @param gdelt_data A \code{tibble} of GDELT data to parse.
#' @param social_embed_column options \code{c('urlSocialMediaImageEmbeds', 'images', 'urlSocialMediaVideoEmbeds', 'video', 'videos'))}
#' @param filter_na Logical. If \code{TRUE}, removes rows with \code{NA} values.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#'
#' @return A `tibble` of parsed social media embed URLs from GKG data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_gkg_mentioned_social_embeds()
#' }

parse_gkg_mentioned_social_embeds <-
  function(gdelt_data,
           social_embed_column = 'urlSocialMediaImageEmbeds',
           filter_na = TRUE,
           return_wide = TRUE) {
    image_video_cols <-
      c(
        'urlSocialMediaImageEmbeds',
        'images',
        'image',
        'urlSocialMediaVideoEmbeds',
        'video',
        'videos'
      )
    if (!social_embed_column %in% image_video_cols) {
      stop("Social embed column can only be\n" |> paste0(paste0(image_video_cols, collapse = '\n')))
    }

    if (social_embed_column %in% c("image", "images")) {
      social_embed_column <-
        'urlSocialMediaImageEmbeds'
    }

    if (social_embed_column  %in% c("video", "videos")) {
      social_embed_column <-
        'urlSocialMediaVideoEmbeds'
    }
    parse_embeds <-
      function(field = "http://instagram.com/p/9YfHJtMx0N;http://instagram.com/p/BFz1t7Tsx8t;http://instagram.com/p/BEZrBSKsx8U;http://instagram.com/p/BEw5_T-Mx3B;",
               return_wide = FALSE) {
        options(scipen = 99999)
        if (field |> is.na()) {
          if (return_wide) {
            field_data <-
              tibble(urlSocialMediaImageEmbed = NA)
          } else {
            field_data <-
              tibble(
                urlSocialMediaImageEmbed = NA,
                idArticleSocialMediaImageEmbed = 1
              )
          }
        }  else {
          fields <-
            field |>
            str_split('\\;') |>
            list_c() |>
            (\(x) x[x != ''])()

          fields_df <-
            tibble(urlSocialMediaImageEmbed = fields) |>
            dplyr::mutate(
              idArticleSocialMediaImageEmbed = 1:n(),
              domainSocialMediaImageEmbed = urlSocialMediaImageEmbed |> urltools::domain()
            )
          if (return_wide) {
            fields_df <-
              fields_df |>
              tidyr::pivot_longer(cols = -idArticleSocialMediaImageEmbed, names_to = "item", values_to = "value") |>
              arrange(idArticleSocialMediaImageEmbed) |>
              unite(item, item, idArticleSocialMediaImageEmbed, sep = '.')

            order_fields <-
              fields_df$item

            field_data <-
              fields_df |>
              tidyr::pivot_wider(names_from = item, values_from = value) |>
              dplyr::select(dplyr::all_of(order_fields))

          } else {
            field_data <-
              fields_df

            field_data <-
              field_data |>
              dplyr::select(
                idArticleSocialMediaImageEmbed,
                domainSocialMediaImageEmbed,
                urlSocialMediaImageEmbed
              )
          }
        }

        return(field_data)
      }

    if (!social_embed_column %in% names(gdelt_data)) {
      stop("Sorry missing source embed column")
    }

    col_names <-
      c('idGKG', social_embed_column)

    counts_data <-
      gdelt_data |>
      dplyr::select(dplyr::all_of(col_names))

    names(counts_data)[2] <-
      'source_col'

    if (filter_na) {
      counts_data <-
        counts_data |>
        dplyr::filter(!source_col |> is.na())
    }

    all_counts <-
      seq_along(counts_data$source_col) |>
      map_dfr(function(x) {
        parse_embeds(field = counts_data$source_col[x],
                     return_wide = FALSE) |>
          dplyr::mutate(idGKG = counts_data$idGKG[x])
      })

    names(all_counts)[1] <-
      'idColumn'

    if (social_embed_column == 'urlSocialMediaImageEmbeds') {
      names(all_counts)[3] <-
        c('urlSocialMediaImageEmbed')

      names(all_counts)[1] <-
        c('idArticleSocialMediaImageEmbed')
    }

    if (social_embed_column == 'urlSocialMediaVideoEmbeds') {
      names(all_counts)[3] <-
        c('urlSocialMediaVideo.embed')
      names(all_counts)[1] <-
        c('idArticle.social_media.video_embed')
    }

    all_counts <-
      all_counts |>
      dplyr::select(dplyr::matches("id^[A-Z]"), everything())

    id_col <-
      all_counts |> select(dplyr::matches("^id[A-Z]")) |> names() |> (\(x) x[[1]])() |> suppressWarnings()
    all_counts <-
      all_counts |>
      get_clean_count_data(count_col = id_col, return_wide = return_wide) |>
      separate(
        idGKG,
        into = c('GKG', 'dateTime'),
        sep = '\\-',
        remove = FALSE
      ) |>
      mutate(dateTime = dateTime |> as.numeric()) |>
      arrange(dateTime) |>
      dplyr::select(-c(dateTime, GKG)) |>
      suppressWarnings()

    if (!return_wide) {
      all_counts <-
        all_counts |>
        tidyr::pivot_longer(cols = -idGKG, names_to = "item", values_to = "value", values_drop_na = TRUE) |>
        .resolve_long_names()
    }

    return(all_counts)
  }

#' Returns article tones from a gkg data frame
#'
#' @param gdelt_data A \code{tibble} of GDELT data to parse.
#' @param filter_na Logical. If \code{TRUE}, removes rows with \code{NA} values.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#'
#' @return A `tibble` of parsed article tone scores from GKG data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_gkg_mentioned_article_tone()
#' }
parse_gkg_mentioned_article_tone <-
  function(gdelt_data,
           filter_na = TRUE,
           return_wide = TRUE) {
    parse_article_tones <-
      function(field = "-4.65116279069767,1.55038759689922,62015503875969,7.75193798449612,13.1782945736434,0,134",
               return_wide = FALSE) {
        options(scipen = 99999, digits = 5)
        if (field |> is.na()) {
          if (return_wide) {
            field_data <-
              tibble(amount.tone = NA)
          } else {
            field_data <-
              tibble(amount.tone = NA, idArticle.tone = 1)
          }
        }  else {
          fields <-
            field |>
            str_split('\\,') |>
            list_c() |>
            (\(x) x[x != ''])() |>
            as.numeric()

          fields_df <-
            tibble(amount.tone = fields) |>
            dplyr::mutate(idArticle.tone = 1:n())
          if (return_wide) {
            fields_df <-
              fields_df |>
              tidyr::pivot_longer(cols = -idArticle.tone, names_to = "item", values_to = "value") |>
              arrange(idArticle.tone) |>
              unite(item, item, idArticle.tone, sep = '.')

            order_fields <-
              fields_df$item

            field_data <-
              fields_df |>
              tidyr::pivot_wider(names_from = item, values_from = value) |>
              dplyr::select(dplyr::all_of(order_fields))

          } else {
            field_data <-
              fields_df

            field_data <-
              field_data |>
              dplyr::select(idArticle.tone, amount.tone)
          }
        }

        return(field_data)
      }

    if (!'tone' %in% names(gdelt_data)) {
      stop("Sorry missing tone column")
    }
    counts_data <-
      gdelt_data |>
      dplyr::select(idGKG, tone)

    all_counts <-
      seq_along(counts_data$tone) |>
      map_dfr(function(x) {
        parse_article_tones(field = counts_data$tone[x], return_wide = FALSE) |>
          dplyr::mutate(idGKG = counts_data$idGKG[x])
      }) |>
      dplyr::select(idGKG, everything()) |>
      dplyr::rename(scoreTone = amount.tone)

    if (filter_na) {
      all_counts <-
        all_counts |>
        dplyr::filter(!scoreTone |> is.na())
    }

    all_counts <-
      all_counts |>
      get_clean_count_data(count_col = 'idArticle.tone',
                           extra_key = NA,
                           return_wide = return_wide) |>
      arrange(idGKG) |>
      separate(
        idGKG,
        into = c('GKG', 'dateTime'),
        sep = '\\-',
        remove = FALSE
      ) |>
      mutate(dateTime = dateTime |> as.numeric()) |>
      arrange(dateTime) |>
      dplyr::select(-c(dateTime, GKG)) |>
      suppressWarnings()

    if (!return_wide) {
      all_counts <-
        all_counts |>
        .resolve_long_names()
    }

    return(all_counts)
  }

#' Returns mentioned CAMEO event count from a gkg data frame
#'
#' @param gdelt_data A \code{tibble} of GDELT data to parse.
#' @param count_column options \code{c('count', 'counts'))}
#' @param filter_na Logical. If \code{TRUE}, removes rows with \code{NA} values.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#'
#' @return A `tibble` of parsed CAMEO event counts from GKG data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_gkg_mentioned_event_counts()
#' }
parse_gkg_mentioned_event_counts <- function(gdelt_data,
                                             count_column = 'counts',
                                             filter_na = TRUE,
                                             return_wide = TRUE) {
  count_cols <-
    c('counts',
      'count',
      'countsCharLoc',
      'countCharLoc',
      'charLoc')

  if (!count_column %in% count_cols) {
    stop("Sorry count column can only be\n" |>
           paste0(paste0(count_cols, collapse = '\n')))
  }

  if (count_column %in% c('counts', 'count')) {
    count_column <-
      'counts'
  }

  if (count_column %in% c('countsCharLoc', 'countCharLoc', 'charLoc')) {
    count_column <-
      'countsCharLoc'
  }

  parse_field_count <-
    function(field = "KIDNAP#60##4#Beirut, Beyrouth, Lebanon#LE#LE04#33.8719#35.5097#-801546;KIDNAP#2##1#Lebanon#LE#LE#33.8333#35.8333#LE;KIDNAP#4##1#Australia#AS#AS#-27#133#AS;",
             return_wide = FALSE) {
      options(scipen = 99999, digits = 5)
      if (field |> is.na()) {
        if (return_wide) {
          field_data <-
            tibble(codeGKGTheme = NA)
        } else {
          field_data <-
            tibble(codeGKGTheme = NA,
                       idArticle.field = 1)
        }
      }  else {
        fields <-
          field |>
          str_split('\\;') |>
          list_c() |>
          (\(x) x[x != ''])()

        fields_df <-
          tibble(field_item = fields) |>
          dplyr::mutate(idArticle.field = 1:n()) |>
          separate(
            col = field_item,
            sep = '\\#',
            into = c(
              'codeGKGTheme',
              'countEvent',
              'entityEvent',
              'idTypeLocation',
              'location',
              'idCountry',
              'idADM1Code',
              'latitude',
              'longitude',
              'idFeature',
              'charLoc'
            )
          ) |>
          suppressMessages() |>
          suppressWarnings()

        fields_df <-
          fields_df |>
          dplyr::mutate(dplyr::across(dplyr::matches("count|charLoc|idTypeLocation") & !dplyr::matches("idCountry"), as.numeric)) |>
          dplyr::mutate(dplyr::across(dplyr::matches("latitude|longitude"), ~round(as.numeric(.x), digits = 5)))

        fields_df$entityEvent[fields_df$entityEvent == ''] <-
          NA

        fields_df$location[fields_df$location == ''] <-
          NA

        fields_df$idCountry[fields_df$idCountry == ''] <-
          NA

        fields_df$idADM1Code[fields_df$idADM1Code == ''] <-
          NA

        fields_df$idFeature[fields_df$idFeature == ''] <-
          NA

        fields_df <-
          fields_df |>
          dplyr::left_join(tibble(
            idTypeLocation = 1:5,
            typeLocation = c(
              'country',
              'usState',
              'usCity',
              'worldCity',
              'worldState'
            )
          )) |>
          dplyr::select(codeGKGTheme:idTypeLocation,
                        typeLocation,
                        everything()) |>
          suppressMessages()

        if (return_wide) {
          fields_df <-
            fields_df |>
            tidyr::pivot_longer(cols = -idArticle.field, names_to = "item", values_to = "value") |>
            arrange(idArticle.field) |>
            unite(item, item, idArticle.field, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df |>
            tidyr::pivot_wider(names_from = item, values_from = value) |>
            dplyr::select(dplyr::all_of(order_fields))

          field_data <-
            field_data |>
            dplyr::mutate(dplyr::across(dplyr::matches("count|charLoc|idTypeLocation") & !dplyr::matches("idCountry"), as.numeric)) |>
            dplyr::mutate(dplyr::across(dplyr::matches("latitude|longitude"), ~round(as.numeric(.x), digits = 5)))

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data |>
            dplyr::select(idArticle.field, everything())
        }
      }

      return(field_data)
    }

  if (!count_column %in% names(gdelt_data)) {
    stop("Sorry missing count column")
  }

  col_names <-
    c('idGKG', count_column)

  counts_data <-
    gdelt_data |>
    dplyr::select(dplyr::all_of(col_names))

  names(counts_data)[2] <-
    'count_col'

  if (filter_na) {
    counts_data <-
      counts_data |>
      dplyr::filter(!count_col |> is.na())
  }

  all_counts <-
    seq_along(counts_data$count_col) |>
    map_dfr(function(x) {
      parse_field_count(field = counts_data$count_col[x],
                        return_wide = FALSE) |>
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) |>
    dplyr::select(idGKG, everything()) |>
    dplyr::select(where(~!all(is.na(.x))))

  all_counts <-
    all_counts |>
    get_clean_count_data(count_col = 'idArticle.field', return_wide = return_wide) |>
    separate(
      idGKG,
      into = c('GKG', 'dateTime'),
      sep = '\\-',
      remove = FALSE
    ) |>
    mutate(dateTime = dateTime |> as.numeric()) |>
    arrange(dateTime) |>
    dplyr::select(-c(dateTime, GKG)) |>
    suppressWarnings()

  if (!return_wide) {
    all_counts <-
      all_counts |>
      .resolve_long_names()
  }

  return(all_counts)
}

#' Returns mentioned locations from a gkg data frame
#'
#' @param gdelt_data A \code{tibble} of GDELT data to parse.
#' @param location_column options \code{c('location', 'locations', 'locationsCharLoc', 'locationCharLoc', 'charLoc'))}
#' @param isCharLoc Logical. If \code{TRUE}, uses character location variant.
#' @param filter_na Logical. If \code{TRUE}, removes rows with \code{NA} values.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#'
#' @return A `tibble` of parsed mentioned locations from GKG data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_gkg_mentioned_locations()
#' }
parse_gkg_mentioned_locations <-
  function(gdelt_data,
           location_column = 'locations',
           isCharLoc = FALSE,
           filter_na = TRUE,
           return_wide = TRUE) {
    location_cols <-
      c('location',
        'locations',
        'locationsCharLoc',
        'locationCharLoc',
        'charLoc')

    if (!location_column %in% location_cols) {
      stop("Sorry location column can only be\n" |>
             paste0(paste0(location_cols, collapse = '\n')))
    }

    if (location_column %in% c('location', 'locations')) {
      location_column <-
        'locations'
    }

    if (location_column %in% c('locationsCharLoc', 'locationCharLoc', 'charLoc')) {
      location_column <-
        'locationsCharLoc'
    }
    parse_location_count <-
      function(field = "4#Leichhardt, New South Wales, Australia#AS#AS02#4944#-33.8833#151.15#-1583352#203;4#Daintree, Queensland, Australia#AS#AS04#40202#-1625#145.317#-1568710#421;4#Daintree, Queensland, Australia#AS#AS04#40202#-1625#145.317#-1568710#2224",
               return_wide = FALSE) {
        options(scipen = 99999, digits = 5)
        if (field |> is.na()) {
          if (return_wide) {
            field_data <-
              tibble(location = NA)
          } else {
            field_data <-
              tibble(location = NA, idArticle.location = 1)
          }
        }  else {
          fields <-
            field |>
            str_split('\\;') |>
            list_c() |>
            (\(x) x[x != ''])()

          fields_df <-
            tibble(field_item = fields) |>
            dplyr::mutate(idArticle.location = 1:n())


          if (isCharLoc) {
            fields_df <-
              fields_df |>
              separate(
                col = field_item,
                sep = '\\#',
                into = c(
                  'idTypeLocation',
                  'location',
                  'idCountry',
                  'idADM1Code',
                  'idADM2Code',
                  'latitude',
                  'longitude',
                  'idFeature',
                  'charLoc'
                )
              ) |>
              suppressMessages() |>
              suppressWarnings()

          } else {
            fields_df <-
              fields_df |>
              separate(
                col = field_item,
                sep = '\\#',
                into = c(
                  'idTypeLocation',
                  'location',
                  'idCountry',
                  'idADM1Code',
                  'latitude',
                  'longitude',
                  'idFeature'
                )
              ) |>
              suppressMessages() |>
              suppressWarnings()
          }

          fields_df <-
            fields_df |>
            dplyr::mutate(dplyr::across(dplyr::matches("idTypeLocation|charLoc"), as.numeric)) |>
            dplyr::mutate(dplyr::across(dplyr::matches("latitude|longitude"), ~round(as.numeric(.x), digits = 5))) |>
            dplyr::left_join(tibble(
              idTypeLocation = 1:5,
              typeLocation = c(
                'country',
                'usState',
                'usCity',
                'worldCity',
                'worldState'
              )
            )) |>
            suppressMessages() |>
            dplyr::select(idTypeLocation, typeLocation, everything()) |>
            suppressWarnings()


          fields_df$location[fields_df$location == ''] <-
            NA

          fields_df$idCountry[fields_df$idCountry == ''] <-
            NA

          fields_df$idADM1Code[fields_df$idADM1Code == ''] <-
            NA

          fields_df$idFeature[fields_df$idFeature == ''] <-
            NA

          if (return_wide) {
            fields_df <-
              fields_df |>
              tidyr::pivot_longer(cols = -idArticle.location, names_to = "item", values_to = "value") |>
              arrange(idArticle.location) |>
              unite(item, item, idArticle.location, sep = '.')

            order_fields <-
              fields_df$item

            field_data <-
              fields_df |>
              tidyr::pivot_wider(names_from = item, values_from = value) |>
              dplyr::select(dplyr::all_of(order_fields))

            field_data <-
              field_data |>
              dplyr::mutate(dplyr::across(dplyr::matches("idTypeLocation|charLoc"), as.numeric)) |>
              dplyr::mutate(dplyr::across(dplyr::matches("latitude|longitude"), ~round(as.numeric(.x), digits = 5)))
          } else {
            field_data <-
              fields_df

            field_data <-
              field_data |>
              dplyr::select(idArticle.location, everything())
          }
        }

        return(field_data)
      }

    if (!location_column %in% names(gdelt_data)) {
      stop("Sorry missing location column")
    }

    col_names <-
      c('idGKG', location_column)

    counts_data <-
      gdelt_data |>
      dplyr::select(dplyr::all_of(col_names))

    names(counts_data)[2] <-
      'loc_col'

    if (filter_na) {
      counts_data <-
        counts_data |>
        dplyr::filter(!loc_col |> is.na())
    }

    all_counts <-
      seq_along(counts_data$loc_col) |>
      map_dfr(function(x) {
        parse_location_count(field = counts_data$loc_col[x],
                             return_wide = FALSE) |>
          dplyr::mutate(idGKG = counts_data$idGKG[x])
      }) |>
      dplyr::select(idGKG, everything())

    if (filter_na) {
      all_counts <-
        all_counts |>
        dplyr::filter(!location |> is.na())
    }
    all_counts <-
      all_counts |>
      get_clean_count_data(count_col = 'idArticle.location', return_wide = return_wide) |>
      separate(
        idGKG,
        into = c('GKG', 'dateTime'),
        sep = '\\-',
        remove = FALSE
      ) |>
      mutate(dateTime = dateTime |> as.numeric()) |>
      arrange(dateTime) |>
      dplyr::select(-c(dateTime, GKG)) |>
      suppressWarnings()

    if (!return_wide) {
      all_counts <-
        all_counts |>
        .resolve_long_names()
    }


    return(all_counts)
  }

#' Returns mentioned dates from gkg data frame
#'
#' @param gdelt_data A \code{tibble} of GDELT data to parse.
#' @param filter_na Logical. If \code{TRUE}, removes rows with \code{NA} values.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#'
#' @return A `tibble` of parsed mentioned dates from GKG data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_gkg_mentioned_dates()
#' }
parse_gkg_mentioned_dates <- function(gdelt_data,
                                      filter_na = TRUE,
                                      return_wide = TRUE) {
  parse_dates <-
    function(field = "4#6#16#0#734;4#4#26#0#2258",
             return_wide = FALSE) {
      options(scipen = 99999, digits = 5)
      if (field |> is.na()) {
        if (return_wide) {
          field_data <-
            tibble(idDateResolution = NA)
        } else {
          field_data <-
            tibble(idDateResolution = NA,
                       idDateArticle = NA)
        }
      }  else {
        fields <-
          field |>
          str_split('\\;') |>
          list_c() |>
          (\(x) x[x != ''])()

        fields_df <-
          tibble(field_item = fields) |>
          dplyr::mutate(idDateArticle = 1:n()) |>
          separate(
            col = field_item,
            sep = '\\#',
            into = c('idDateResolution', 'month', 'day', 'year', 'charLoc')
          ) |>
          suppressMessages() |>
          suppressWarnings()

        fields_df <-
          fields_df |>
          dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) |>
          dplyr::left_join(tibble(
            idDateResolution = 1:4,
            dateResolution = c(
              'ex_mon_date',
              'year_only',
              'month_date' ,
              'fully_resolved'
            )
          )) |>
          suppressMessages() |>
          dplyr::select(idDateResolution, dateResolution, everything())

        if (return_wide) {
          fields_df <-
            fields_df |>
            tidyr::pivot_longer(cols = -idDateArticle, names_to = "item", values_to = "value") |>
            arrange(idDateArticle) |>
            unite(item, item, idDateArticle, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df |>
            tidyr::pivot_wider(names_from = item, values_from = value) |>
            dplyr::select(dplyr::all_of(order_fields))

          field_data <-
            field_data |>
            dplyr::mutate(dplyr::across(dplyr::matches("idDateResolution|month|day|year|charLoc"), as.numeric))

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data |>
            dplyr::select(idDateArticle, everything())
        }
      }

      return(field_data)
    }

  if (!'dates' %in% names(gdelt_data)) {
    stop("Sorry missing date column")
  }
  counts_data <-
    gdelt_data |>
    dplyr::select(idGKG, dates)

  if (filter_na) {
    counts_data <-
      counts_data |>
      dplyr::filter(!dates |> is.na())
  }

  all_counts <-
    seq_along(counts_data$dates) |>
    map_dfr(function(x) {
      parse_dates(field = counts_data$dates[x], return_wide = FALSE) |>
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) |>
    dplyr::select(idGKG, everything()) |>
    dplyr::select(-idDateResolution)

  all_counts <-
    all_counts |>
    get_clean_count_data(count_col = 'idDateArticle', return_wide = return_wide) |>
    separate(
      idGKG,
      into = c('GKG', 'dateTime'),
      sep = '\\-',
      remove = FALSE
    ) |>
    mutate(dateTime = dateTime |> as.numeric()) |>
    arrange(dateTime) |>
    dplyr::select(-c(dateTime, GKG)) |>
    suppressWarnings()

  if (!return_wide) {
    all_counts <-
      all_counts |>
      .resolve_long_names()
  }

  return(all_counts)
}

#' Returns mentioned quotes from a gkg data frame
#'
#' @param gdelt_data A \code{tibble} of GDELT data to parse.
#' @param filter_na Logical. If \code{TRUE}, removes rows with \code{NA} values.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#'
#' @return A `tibble` of parsed mentioned quotes from GKG data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_gkg_mentioned_quotes()
#' }
parse_gkg_mentioned_quotes <-
  function(gdelt_data,
           filter_na = TRUE,
           return_wide = TRUE) {
    parse_quotes <-
      function(field = "495|51||knowingly aided and abetted an international kidnap#865|50||nothing less than an international child abduction#2764|49|| staff member should be singled out for dismissal#3373|48||make any serious attempt to independently verify#4059|46||wants to go through every single little detail#4802|156||And xC2 ; xA0 ; you're keeping all of xC2 ; xA0 ; them xC2 ; xA0 ; - xC2 ; xA0 ; except one sacrificial lamb - xC2 ; xA0 ; to run the show?#4879|28||How do you think that looks?#6093|60||an extraordinary conspiracy to remove the children illegally#6150|50||nothing less than an international child abduction#6828|408||I xE2 ; x80 ; xA6 ; have found nothing that supports a finding that any Australian Government official somehow knowingly assisted the mother to do something that was wrong xE2 ; x80 ; xA6 ; I do not find xE2 ; x80 ; xA6 ; that any Australian Embassy officials who helped the mother did so knowing that the mother did not have the father consent to remove the girls permanently from Italy",
               return_wide = FALSE) {
        options(scipen = 99999, digits = 5)
        if (field |> is.na()) {
          if (return_wide) {
            field_data <-
              tibble(idArticleQuote = NA)
          } else {
            field_data <-
              tibble(quote = NA,
                         idArticleQuote = NA)
          }
        }  else {
          fields <-
            field |>
            str_split('\\#') |>
            list_c() |>
            (\(x) x[x != ''])()

          fields_df <-
            tibble(quote_items = fields) |>
            dplyr::mutate(idArticleQuote = 1:n()) |>
            separate(
              col = quote_items,
              sep = '\\|',
              into = c('charLoc', 'lengthQuote', 'verbIntro', 'textQuote')
            ) |>
            suppressMessages() |>
            suppressWarnings()

          fields_df <-
            fields_df |>
            dplyr::mutate(textQuote = textQuote |> str_trim()) |>
            dplyr::mutate(dplyr::across(c('charLoc', 'lengthQuote'), as.numeric)) |>
            dplyr::select(idArticleQuote, everything())

          fields_df$verbIntro[fields_df$verbIntro == ''] <-
            NA

          if (return_wide) {
            fields_df <-
              fields_df |>
              tidyr::pivot_longer(cols = -idArticleQuote, names_to = "item", values_to = "value") |>
              arrange(idArticleQuote) |>
              unite(item, item, idArticleQuote, sep = '.')

            order_fields <-
              fields_df$item

            field_data <-
              fields_df |>
              tidyr::pivot_wider(names_from = item, values_from = value) |>
              dplyr::select(dplyr::all_of(order_fields))

            field_data <-
              field_data |>
              dplyr::mutate(dplyr::across(dplyr::matches("charLoc|lengthQuote"), as.numeric))

          } else {
            field_data <-
              fields_df

            field_data <-
              field_data |>
              dplyr::select(idArticleQuote, everything())
          }
        }

        return(field_data)
      }

    if (!'quotations' %in% names(gdelt_data)) {
      stop("Sorry missing quotations column")
    }

    counts_data <-
      gdelt_data |>
      dplyr::select(idGKG, quotations)

    if (filter_na) {
      counts_data <-
        counts_data |>
        dplyr::filter(!quotations |> is.na())
    }

    all_counts <-
      seq_along(counts_data$quotations) |>
      map_dfr(function(x) {
        parse_quotes(field = counts_data$quotations[x], return_wide = FALSE) |>
          dplyr::mutate(idGKG = counts_data$idGKG[x])
      }) |>
      dplyr::select(idGKG, everything())

    if (sum(as.numeric(is.na(all_counts$verbIntro))) == nrow(all_counts)) {
      all_counts <-
        all_counts |>
        dplyr::select(-verbIntro)
    }

    all_counts <-
      all_counts |>
      get_clean_count_data(count_col = 'idArticleQuote', return_wide = TRUE) |>
      separate(
        idGKG,
        into = c('GKG', 'dateTime'),
        sep = '\\-',
        remove = FALSE
      ) |>
      mutate(dateTime = dateTime |> as.numeric()) |>
      arrange(dateTime) |>
      dplyr::select(-c(dateTime, GKG)) |>
      suppressWarnings()

    if (!return_wide) {
      all_counts <-
        all_counts |>
        tidyr::pivot_longer(cols = -idGKG, names_to = "item", values_to = "value", values_drop_na = TRUE) |>
        .resolve_long_names()
    }

    return(all_counts)
  }

#' Returns GCAM codes from a gkg data frame
#'
#' @param gdelt_data A \code{tibble} of GDELT data to parse.
#' @param merge_gcam_codes Logical. If \code{TRUE}, merges GCAM code descriptions.
#' @param filter_na Logical. If \code{TRUE}, removes rows with \code{NA} values.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#'
#' @return A `tibble` of parsed GCAM scores from GKG data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_gkg_mentioned_gcams()
#' }
parse_gkg_mentioned_gcams <- function(gdelt_data,
                                      merge_gcam_codes = FALSE,
                                      filter_na = TRUE,
                                      return_wide = TRUE) {
  parse_gcam_data <-
    function(field = "wc:284,c121:5",
             return_wide = FALSE) {
      options(scipen = 99999)
      if (field |> is.na()) {
        if (return_wide) {
          field_data <-
            tibble(idGCAM = NA)
        } else {
          field_data <-
            tibble(idGCAM = NA,
                       idArticleGCAM = 1)
        }
      }  else {
        fields <-
          field |>
          str_split('\\,') |>
          list_c() |>
          (\(x) x[x != ''])()

        articleWordCount <-
          fields[1] |>
          as.character() |>
          parse_number()

        fields_df <-
          tibble(articleWordCount,
                     idGCAM = fields[2:length(fields)]) |>
          separate(idGCAM,
                   into = c('idGCAM', 'scoreGoldsteinWords'),
                   sep = '\\:') |>
          dplyr::mutate(
            idArticleGCAM = 1:n(),
            scoreGoldsteinWords = scoreGoldsteinWords |> as.numeric() |> round(digits = 4)
          )


        if (return_wide) {
          fields_df <-
            fields_df |>
            tidyr::pivot_longer(cols = -c(articleWordCount, idArticleGCAM), names_to = "item", values_to = "value") |>
            arrange(idArticleGCAM) |>
            unite(item, item, idArticleGCAM, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df |>
            tidyr::pivot_wider(names_from = item, values_from = value) |>
            dplyr::select(dplyr::all_of(order_fields))

          field_data <-
            field_data |>
            dplyr::mutate(dplyr::across(dplyr::matches("scoreGoldsteinWords"), as.numeric))

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data |>
            dplyr::select(idArticleGCAM, everything())
        }
      }

      return(field_data)
    }

  if (!'gcam' %in% names(gdelt_data)) {
    stop("Sorry missing video embed column")
  }

  counts_data <-
    gdelt_data |>
    dplyr::select(idGKG, gcam)

  if (filter_na) {
    counts_data <-
      counts_data |>
      dplyr::filter(!gcam |> is.na())
  }

  all_counts <-
    seq_along(counts_data$gcam) |>
    map_dfr(function(x) {
      parse_gcam_data(field = counts_data$gcam[x],
                      return_wide = FALSE) |>
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) |>
    dplyr::select(idGKG, everything())

  if (merge_gcam_codes) {
    all_counts <-
      all_counts |>
      dplyr::left_join(
        get_codes_gcam() |>
          dplyr::select(
            idGCAM,
            typeDictionary,
            dictionaryHumanName,
            dimensionHumanName
          )
      )
  }

  all_counts <-
    all_counts |>
    get_clean_count_data(count_col = 'idArticleGCAM',
                         extra_key = 'articleWordCount',
                         return_wide = return_wide) |>
    separate(
      idGKG,
      into = c('GKG', 'dateTime'),
      sep = '\\-',
      remove = FALSE
    ) |>
    mutate(dateTime = dateTime |> as.numeric()) |>
    arrange(dateTime) |>
    dplyr::select(-c(dateTime, GKG)) |>
    suppressWarnings()

  if (!return_wide) {
    all_counts <-
      all_counts |>
      .resolve_long_names()
  }

  return(all_counts)
}


#' Returns source name or source url from a gkg data frame
#'
#' @param gdelt_data A \code{tibble} of GDELT data to parse.
#' @param source_column options \code{c('sources', 'source', 'sources.url', 'source.url'))}
#' @param filter_na Logical. If \code{TRUE}, removes rows with \code{NA} values.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#'
#' @return A `tibble` of parsed source URLs and domains from GKG data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_gkg_mentioned_source_data()
#' }
parse_gkg_mentioned_source_data <-
  function(gdelt_data,
           source_column = 'sources',
           filter_na = TRUE,
           return_wide = FALSE) {
    source_options <-
      c('sources', 'source', 'urlSources', 'urlSource')
    if (!source_column %in% source_options) {
      stop("Sorry source column can only be\n" |>
             paste0(paste0(source_options, collapse = '\n')))
    }

    if (source_column %in% c('sources', 'source')) {
      source_column <-
        'sources'
    }

    if (source_column %in% c('urlSources', 'urlSource')) {
      source_column <-
        'urlSources'
    }
    parse_source_name <-
      function(field = "businesstimes.com.sg;businesstimes.com.sg",
               return_wide = FALSE) {
        options(scipen = 99999)
        if (field |> is.na()) {
          if (return_wide) {
            field_data <-
              tibble(nameSource = NA)
          } else {
            field_data <-
              tibble(nameSource = NA,
                         idArticle.source1 = 1)
          }
        }  else {
          fields <-
            field |>
            str_split('\\;') |>
            list_c() |>
            (\(x) x[x != ''])() |>
            unique()

          fields_df <-
            tibble(nameSource = fields) |>
            dplyr::mutate(idArticle.source = 1:n())
          if (return_wide) {
            fields_df <-
              fields_df |>
              tidyr::pivot_longer(cols = -idArticle.source, names_to = "item", values_to = "value") |>
              arrange(idArticle.source) |>
              unite(item, item, idArticle.source, sep = '.')

            order_fields <-
              fields_df$item

            field_data <-
              fields_df |>
              tidyr::pivot_wider(names_from = item, values_from = value) |>
              dplyr::select(dplyr::all_of(order_fields))

          } else {
            field_data <-
              fields_df

            field_data <-
              field_data |>
              dplyr::select(idArticle.source,
                            everything())
          }
        }

        return(field_data)
      }

    if (!source_column %in% names(gdelt_data)) {
      stop("Sorry missing source column")
    }

    col_names <-
      c('idGKG', source_column)

    counts_data <-
      gdelt_data |>
      dplyr::select(dplyr::all_of(col_names))

    names(counts_data)[2] <-
      'source_col'

    if (filter_na) {
      counts_data <-
        counts_data |>
        dplyr::filter(!source_col |> is.na())
    }

    all_counts <-
      seq_along(counts_data$source_col) |>
      map_dfr(function(x) {
        parse_source_name(field = counts_data$source_col[x],
                          return_wide = return_wide) |>
          dplyr::mutate(idGKG = counts_data$idGKG[x])
      })

    if (source_column == 'urlSources') {
      names(all_counts)[2] <-
        c('urlSource')
    }

    if (source_column == 'sources') {
      names(all_counts)[2] <-
        c('nameSource')
    }

    all_counts <-
      all_counts |>
      dplyr::select(idGKG, everything())

    if (!return_wide) {
      all_counts <-
        all_counts |>
        .resolve_long_names()
    }

    return(all_counts)
  }


# data_ac -----------------------------------------------------------------


.get_data_gkg_day_detailed <-
  function(date_data = "2016-06-01",
           table_name = "gkg",
           file_directory =  NULL,
           folder_name = 'gdelt_data',
           only_most_recent = FALSE,
           remove_files = TRUE,
           empty_trash = TRUE,
           return_message = TRUE) {
    if (only_most_recent) {
      date_data <-
        Sys.Date()
    }

    if (!date_data |> substr(5, 5) == "-") {
      stop("Sorry data must be in YMD format, ie, 2016-06-01")
    }
    tables <-
      c('gkg', 'export', 'mentions')
    if (!table_name %in% tables) {
      stop("Sorry tables can only be:\n" |> paste0(paste0(tables, collapse = '\n')))
    }

    date_data <-
      date_data |>
      ymd() |> as.Date()


    if (date_data < "2015-02-18") {
      stop("Sorry data starts on February 18th, 2015")
    }

    if (date_data > Sys.Date()) {
      stop("Sorry data can't go into the future")
    }

    if (only_most_recent) {
      gdelt_detailed_logs <-
        get_urls_gkg_most_recent_log()
      urls <-
        gdelt_detailed_logs |>
        dplyr::filter(nameFile == table_name) |>
        dplyr::pull(urlData)
    } else {
      gdelt_detailed_logs <- .cache_get('gdelt_detailed_logs')
      if (is.null(gdelt_detailed_logs)) {
        gdelt_detailed_logs <-
          get_urls_gkg_15_minute_log()
        .cache_set('gdelt_detailed_logs', gdelt_detailed_logs)
      }

      urls <-
        gdelt_detailed_logs |>
        dplyr::filter(dateData == date_data) |>
        dplyr::filter(nameFile == table_name) |>
        dplyr::pull(urlData)
    }

    get_gdelt_url_data_safe <-
      possibly(get_gdelt_url_data, tibble())

    all_data <-
      urls |>
      map_dfr(function(x) {
        data <-
          get_gdelt_url_data_safe(
            url = x,
            remove_files = remove_files,
            file_directory = file_directory,
            folder_name = folder_name,
            return_message = return_message,
            empty_trash = empty_trash
          )
        data |>
          dplyr::mutate(dplyr::across(dplyr::matches('idFeatureActor|idFeatureAction'), as.character))
      }) |>
      distinct() |>
      suppressWarnings()

    if ('domainSource' %in% names(all_data)) {
      all_data <-
        all_data |>
        mutate(domainSource = documentSource |> urltools::domain())
    }

    if ('urlSource' %in% names(all_data)) {
      all_data <-
        all_data |>
        mutate(domainSource = urlSource |> urltools::domain())
    }

    if (return_message) {
      cli::cli_alert_info("You retrieved {nrow(all_data)} gkg detailed events for {date_data}")
    }

    return(all_data)
  }


#' Get dates detailed data from a specified table
#'
#' @param date_data must be a date in Year - Month - Day format
#' @param table_name the name of the table
#' options \code{c('gkg', 'export', 'mentions'))}
#' @param file_directory where are the files to be saved
#' @param remove_files Do you want to remove the files
#' \code{TRUE, FALSE}
#' @param empty_trash Do You want to empy the trash
#' \code{TRUE, FALSE}
#' @param return_message Do you want to return a message
#' \code{TRUE, FALSE}
#' @return A `tibble` of detailed GKG data for the specified dates
#' @export
#'
#' @examples
#' \dontrun{
#' get_data_gkg_days_detailed()
#' }

get_data_gkg_days_detailed <-
  function(dates = c("2016-07-19"),
           table_name = c("gkg"),
           file_directory = NULL,
           folder_name = 'gdelt_data',
           only_most_recent = FALSE,
           remove_files = TRUE,
           empty_trash = TRUE,
           return_message = TRUE,
           use_parallel = FALSE,
           workers = NULL) {
    tryCatch({
    .get_data_gkg_day_detailed_safe <-
    possibly(.get_data_gkg_day_detailed, tibble())

  var_matrix <-
    expand.grid(date = dates,
                table_name = table_name) |>
    as_tibble()

  all_data <-
    .safe_map_dfr(
      seq_len(var_matrix |> nrow()),
      function(x)
        .get_data_gkg_day_detailed_safe(
          date_data = var_matrix$date[x],
          table_name = var_matrix$table_name[x],
          only_most_recent = only_most_recent,
          file_directory = file_directory,
          folder_name = folder_name,
          remove_files = remove_files,
          empty_trash = empty_trash,
          return_message = return_message
        ),
      use_parallel = use_parallel,
      workers = workers
    ) |>
    suppressWarnings()

  if ('idDateTime' %in% names(all_data)) {
    all_data <-
      all_data |>
      dplyr::rename(idDateTime = idDateTimeArticle) |>
      dplyr::mutate(idDateTime = 1:n()) |>
      separate(idGKG, sep = '\\-', c('dateCode', 'remove')) |>
      unite(idGKG,
            dateCode,
            idDateTime,
            sep = '-',
            remove = FALSE) |>
      dplyr::select(-c(dateCode, remove))
  }

  return(all_data)
    }, error = function(e) {
      cli::cli_alert_danger("Failed: {e$message}")
      return(tibble())
    })
}

#' Retrieves gkg summary file for a given day
#'
#' @param date_data Date of the data, must be in year, month day form.
#' @param file_directory location of where the file is to be saved
#' @param is_count_file options
#' \code{c(TRUE, FALSE)}
#' @param remove_files Logical. If \code{TRUE}, removes temporary files after processing.
#' \code{c(TRUE, FALSE)}
#' @param empty_trash Empty trash
#' \code{c(TRUE, FALSE)}
#' @param return_message Logical. If \code{TRUE}, prints progress messages to console.
#' \code{c(TRUE, FALSE)}
#' @return A `tibble` of GKG daily summary data for the specified date
#'
#' @examples
#' \dontrun{
#' get_data_gkg_day_summary()
#' }

get_data_gkg_day_summary <- function(date_data = "2016-06-01",
                                     file_directory = NULL,
                                     folder_name = 'gdelt_data',
                                     is_count_file = FALSE,
                                     remove_files = TRUE,
                                     empty_trash = TRUE,
                                     return_message = TRUE) {
  options(scipen = 99999)
  if (!date_data |> substr(5, 5) == "-") {
    stop("Sorry data must be in YMD format, ie, 2016-06-01")
  }

  date_data <-
    date_data |>
    ymd() |> as.Date()


  if (date_data < "2013-04-01") {
    stop("Sorry data starts on April 1st, 2013")
  }

  if (date_data > Sys.Date()) {
    stop("Sorry data can't go into the future")
  }
  summary_data_urls <- .cache_get('summary_data_urls')
  if (is.null(summary_data_urls)) {
    summary_data_urls <-
      get_urls_gkg_daily_summaries(return_message = return_message)
    .cache_set('summary_data_urls', summary_data_urls)
  }

  if (is_count_file) {
    summary_data_urls <-
      summary_data_urls |>
      dplyr::filter(isCountFile == TRUE)
  } else {
    summary_data_urls <-
      summary_data_urls |>
      dplyr::filter(isCountFile == FALSE)
  }

  urls <-
    summary_data_urls |>
    dplyr::filter(dateData == date_data) |>
    dplyr::pull(urlData)

  get_gdelt_url_data_safe <-
    possibly(get_gdelt_url_data, tibble())

  all_data <-
    urls |>
    map_dfr(function(x) {
      get_gdelt_url_data_safe(
        url = x,
        remove_files = remove_files,
        file_directory = file_directory,
        folder_name = folder_name,
        return_message = return_message,
        empty_trash = empty_trash
      )
    }) |>
    distinct()

  if ('countObject' %in% names(all_data)) {
    all_data <-
      all_data |>
      dplyr::mutate(countObject = countObject |> as.numeric())
  }
  if ('idCAMEOEvents' %in% names(all_data)) {
    all_data <-
      all_data |>
      mutate(idCAMEOEvents = idCAMEOEvents |> as.character())
  }

  if (is_count_file == FALSE) {
    all_data <-
      all_data |>
      dplyr::rename(dateEvent = date)
  }

  all_data <-
    all_data |>
    mutate(isCountFile = if_else(is_count_file, TRUE, FALSE))


  if (return_message) {
    n_rows <- nrow(all_data)
    cli::cli_alert_info("You retrieved {n_rows} gkg summary events for {date_data}")
  }

  return(all_data)
}

#' Gets days summary GDELT GKG data by table
#'
#' @param dates Character vector of dates in YYYY-MM-DD format.
#' @param file_directory Character string. Path to directory for file storage.
#' @param remove_files Logical. If \code{TRUE}, removes temporary files after processing.
#' @param empty_trash Logical. If \code{TRUE}, empties trash after file removal.
#' @param return_message Logical. If \code{TRUE}, prints progress messages to console.
#'
#' @return A `tibble` of GKG daily summary data for the specified dates
#' @export
#' @examples
#' get_data_gkg_days_summary(dates = c("2016-07-18"), is_count_file = c(TRUE, FALSE))

get_data_gkg_days_summary <- function(dates = c("2016-06-01"),
                                      is_count_file = c(TRUE, FALSE),
                                      file_directory = NULL,
                                      folder_name = 'gdelt_data',
                                      remove_files = TRUE,
                                      empty_trash = TRUE,
                                      nest_data = FALSE,
                                      return_message = TRUE,
                                      use_parallel = FALSE,
                                      workers = NULL) {
  tryCatch({
  get_data_gkg_day_summary_safe <-
    possibly(get_data_gkg_day_summary, tibble())

  var_matrix <-
    expand.grid(
      date = dates,
      is_count_file = is_count_file
    ) |>
    as_tibble() |>
    suppressWarnings()

  all_data <-
    .safe_map_dfr(
      seq_len(nrow(var_matrix)),
      function(x)
        get_data_gkg_day_summary_safe(
          date_data = var_matrix$date[x],
          is_count_file = var_matrix$is_count_file[x],
          file_directory = file_directory,
          folder_name = folder_name,
          remove_files = remove_files,
          empty_trash = empty_trash,
          return_message = return_message
        ) |> suppressWarnings(),
      use_parallel = use_parallel,
      workers = workers
    ) |>
    arrange(idGKG) |>
    suppressWarnings()

  return(all_data)
  }, error = function(e) {
    cli::cli_alert_danger("Failed: {e$message}")
    return(tibble())
  })
}

#' Retreive GDELT data for a given period
#'
#' @param period the GDELT period
#' @param file_directory Character string. Path to directory for file storage.
#' @param remove_files Logical. If \code{TRUE}, removes temporary files after processing.
#' @param empty_trash Logical. If \code{TRUE}, empties trash after file removal.
#' @param return_message Logical. If \code{TRUE}, prints progress messages to console.
#' @return A `tibble` of GDELT event data for the specified period
#'
#' @examples
#' get_data_gdelt_period_event(period = 1983)
get_data_gdelt_period_event <- function(period = 1983,
                                        file_directory = NULL,
                                        folder_name = 'gdelt_data',
                                        remove_files = TRUE,
                                        empty_trash = TRUE,
                                        return_message = TRUE) {
  period <-
    period |>
    as.character() |>
    str_replace_all("\\-", '')

  gdelt_event_urls <- .cache_get('gdelt_event_urls')
  if (is.null(gdelt_event_urls)) {
    gdelt_event_urls <-
      get_urls_gdelt_event_log(return_message = return_message)
    .cache_set('gdelt_event_urls', gdelt_event_urls)
  }
  periods <-
    gdelt_event_urls$periodData
  if (!period %in% periods) {
    gdelt_event_message <-
      "Period can only be a 4 digit year between 1979 and 2005\nEXAMPLE: 1983\nA 6 digit year and month from 2006 to March 2013\nEXAMPLE: 201208\nOr an 8 digit year, month, day from March 1, 2013 until today\nEXAMPLE: 20140303"

    stop(gdelt_event_message)
  }

  urls <-
    gdelt_event_urls |>
    dplyr::filter(periodData == period) |>
    dplyr::pull(urlData)

  get_gdelt_url_data_safe <-
    possibly(get_gdelt_url_data, tibble())

  all_data <-
    urls |>
    map_dfr(function(x) {
      get_gdelt_url_data_safe(
        url = x,
        remove_files = remove_files,
        file_directory = file_directory,
        folder_name = folder_name,
        return_message = return_message,
        empty_trash = empty_trash
      )
    })

  if (return_message) {
    cli::cli_alert_info("You retrieved {nrow(all_data)} GDELT events for the period of {period}")
  }

  return(all_data)
}


#' Returns GDELT event data for a given periods
#'
#' @param periods Character vector of GDELT time periods.
#' @param file_directory Character string. Path to directory for file storage.
#' @param is_count_file Logical. If \code{TRUE}, treats input as count file.
#' @param remove_files Logical. If \code{TRUE}, removes temporary files after processing.
#' @param empty_trash Logical. If \code{TRUE}, empties trash after file removal.
#' @param return_message Logical. If \code{TRUE}, prints progress messages to console.
#' @return A `tibble` of GDELT event data for the specified periods
#' @export
#'
#' @examples
#' get_data_gdelt_periods_event (periods = c(1983))
get_data_gdelt_periods_event <- function(periods = c(1983, 1989),
                                         file_directory = NULL,
                                         folder_name = 'gdelt_data',
                                         remove_files = TRUE,
                                         empty_trash = TRUE,
                                         return_message = TRUE,
                                         use_parallel = FALSE,
                                         workers = NULL) {
  tryCatch({
  get_data_gdelt_period_event_safe <-
    possibly(get_data_gdelt_period_event, tibble())
  periods <-
    periods |>
    str_replace_all('\\-', '')
  all_data <-
    .safe_map_dfr(
      seq_along(periods),
      function(x)
        get_data_gdelt_period_event_safe(
          period = periods[x],
          file_directory = file_directory,
          folder_name = folder_name,
          remove_files = remove_files,
          empty_trash = empty_trash,
          return_message = return_message
        ),
      use_parallel = use_parallel,
      workers = workers
    ) |>
    suppressWarnings()

  return(all_data)
  }, error = function(e) {
    cli::cli_alert_danger("Failed: {e$message}")
    return(tibble())
  })
}

#' get_vgkg_schema
#'
#' @return A `tibble` of VGKG column names and descriptions
#'
#' @examples
#' \dontrun{
#' get_vgkg_schema()
#' }
get_vgkg_schema  <- function() {
  cv_schema <-
    tibble(
      nameGDELT = c(
        "DATE",
        "DocumentIdentifier",
        "ImageURL",
        "Labels",
        "GeoLandmarks",
        "Logos",
        "SafeSearch",
        "Faces",
        "OCR",
        "LangHints",
        "WidthHeight",
        "RawJSON"
      ),
      nameActual =
        c(
          "dateTimeDocument",
          "documentSource",
          "urlImage",
          "xmlLabels",
          "xmlGeoLandmarks",
          "xmlLogos",
          "xmlSafeSearch",
          "xmlFaces",
          "xmlOCR",
          "codesLanguages",
          "dimWidthHeight",
          "jsonCloudVision"
        )

    )
  return(cv_schema)
}

#' get_urls_vgkg
#'
#' @return A `tibble` of VGKG data URLs with datetime metadata
#' @export
#' @examples
#' \dontrun{
#' get_urls_vgkg()
#' }
get_urls_vgkg <- function() {
  options(scipen = 999999)
  cloud_vision_start_hms <-
    20160222113000

  time_now <-
    Sys.time() |> format("%Y%m%d%H%M%S") |> as.numeric()

  all_dates <-
    seq(ymd_hms(cloud_vision_start_hms),
        to = time_now |> ymd_hms(),
        by = '15 min')

  url_df <-
    tibble(
      dateTimeData = all_dates,
      isoPeriod = dateTimeData |> format("%Y%m%d%H%M%S") |> as.numeric(),
      urlCloudVisionTags = 'http://data.gdeltproject.org/gdeltv2_cloudvision/' |> paste0(isoPeriod, '.imagetags.csv.gz'),
      urlTranslationTags = 'http://data.gdeltproject.org/gdeltv2_cloudvision/' |> paste0(isoPeriod, '.translation.imagetags.csv.gz')
    ) |>
    mutate(dateData = dateTimeData |> as.Date()) |>
    arrange(desc(dateTimeData))
  return(url_df)
}

#' Gets most recent CV log URLs
#'
#' @return A `tibble` of the most recent VGKG Cloud Vision log URLs
#'
#'
#' @examples get_urls_gkg_most_recent_log()
get_urls_vgkg_most_recent  <- function() {
  log_df <-
    'http://data.gdeltproject.org/gdeltv2_cloudvision/lastupdate.txt' |>
    read_tsv(col_names = FALSE) |>
    suppressMessages()

  names(log_df) <-
    c('value')

  values <-
    log_df$value |> str_split("\\ ") |>
    list_c()


  items <-
    c('idVGKPeriod', 'idVGKHash', 'urlCloudVisionTags')

  log_df <-
    tibble(item = items, value = values) |>
    tidyr::pivot_wider(names_from = item, values_from = value) |>
    mutate(idVGKPeriod = idVGKPeriod |> as.numeric())

  return(log_df)
}

.get_data_vgkg_url <-
  function(url = 'http://data.gdeltproject.org/gdeltv2_cloudvision/20160606234500.imagetagsv1.csv.gz',
           remove_json_column = TRUE,
           return_message = TRUE) {
    ok_url <-
      url |> url_ok() |> suppressWarnings()
    if (!ok_url) {
      stop("Invalid url")
    }

    cloud_vision_data <-
      url |>
      curl() |>
      gzcon() |>
      read_tsv(col_names = FALSE) |>
      suppressWarnings() |>
      suppressMessages()

    names(cloud_vision_data) <-
      get_vgkg_schema() |> pull(nameActual)

    cloud_vision_data <-
      cloud_vision_data |>
      dplyr::mutate(
        dateCodeURL = dateTimeDocument,
        idDateTime = 1:n(),
        idVGKG = dateTimeDocument |> paste0('-', idDateTime),
        dateTimeDocument = dateTimeDocument |> lubridate::ymd_hms() |> lubridate::with_tz(Sys.timezone()),
        dateDocument = dateTimeDocument |> as.Date(),
        dimWidthHeight = dimWidthHeight |> as.character() |> parse_number()
      ) |>
      dplyr::select(idVGKG, idDateTime, everything())

    if (remove_json_column) {
      cloud_vision_data <-
        cloud_vision_data |>
        dplyr::select(-jsonCloudVision)
    }


    if (return_message) {
      cli::cli_alert_info("Downloaded, parsed and imported {url}")
    }
    return(cloud_vision_data)
  }


.get_data_vgkg_day <-
  function(date_data = "2016-06-08",
           only_most_recent = FALSE,
           include_translations = FALSE,
           remove_json_column = TRUE,
           return_message = TRUE) {
    if (only_most_recent) {
      urls <-
        get_urls_vgkg_most_recent() |>
        dplyr::pull(urlCloudVisionTags)
    } else {
      if (!date_data |> substr(5, 5) == "-") {
        stop("Sorry data must be in YMD format, ie, 2016-06-01")
      }

      date_data <-
        date_data |>
        lubridate::ymd() |>
        as.Date()

      if (date_data < "2016-02-22") {
        stop("Sorry data starts on February 22, 2016")
      }

      if (date_data > Sys.Date()) {
        stop("Sorry data can't go into the future")
      }

      cv_urls <- .cache_get('cv_urls')
      if (is.null(cv_urls)) {
        cv_urls <-
          get_urls_vgkg()
        .cache_set('cv_urls', cv_urls)
      }
      urls <-
        cv_urls |>
        dplyr::filter(dateData == date_data) |>
        dplyr::pull(urlCloudVisionTags)

      if (include_translations) {
        urls <-
          c(urls,
            cv_urls |>
              dplyr::filter(dateData == date_data) |>
              dplyr::pull(urlTranslationTags))
      }
    }

    .get_data_vgkg_url_safe <-
      possibly(.get_data_vgkg_url, tibble())

    all_data <-
      urls |>
      map_dfr(function(x) {
        .get_data_vgkg_url_safe(
          url = x,
          remove_json_column = remove_json_column,
          return_message = return_message
        )
      }) |>
      distinct() |>
      dplyr::select(idVGKG, idDateTime, dateTimeDocument, everything()) |>
      suppressWarnings()

    if (return_message) {
      cli::cli_alert_info("You retrieved {nrow(all_data)} cloud vision processed items for {date_data}")
    }

    return(all_data)

  }

#' Gets CV Data for stated periods
#'
#' @param date_data Character string. Date in YYYY-MM-DD format.
#' @param include_translations Logical. If \code{TRUE}, includes translation data.
#' @param file_directory Character string. Path to directory for file storage.
#' @param remove_files Logical. If \code{TRUE}, removes temporary files after processing.
#' @param empty_trash Logical. If \code{TRUE}, empties trash after file removal.
#' @param return_message Logical. If \code{TRUE}, prints progress messages to console.
#' @return A `tibble` of parsed VGKG data for the specified dates
#' @export
#'
#' @examples
#' \dontrun{
#' get_data_vgkg_dates()
#' }
get_data_vgkg_dates <-
  function(dates = c("2016-06-09", "2016-06-08"),
           include_translations = FALSE,
           only_most_recent = FALSE,
           remove_json_column = TRUE,
           return_message = TRUE,
           use_parallel = FALSE,
           workers = NULL) {
    tryCatch({
    if (only_most_recent) {
      dates <-
        Sys.Date()
    }

    .get_data_vgkg_day_safe <-
      possibly(.get_data_vgkg_day, tibble())

    all_data <-
      .safe_map_dfr(
        dates,
        function(x)
          .get_data_vgkg_day_safe(
            date_data = x,
            only_most_recent = only_most_recent,
            remove_json_column = remove_json_column,
            return_message = return_message
          ),
        use_parallel = use_parallel,
        workers = workers
      )

    all_data <-
      all_data |>
      separate(idVGKG, into = c('VGKG', 'remove'), '\\-') |>
      group_by(dateDocument) |>
      mutate(count = 1:n()) |>
      mutate(idVGKG = VGKG |> paste0('-', count)) |>
      dplyr::select(-c(count, idDateTime, remove, VGKG)) |>
      dplyr::select(idVGKG, everything()) |>
      ungroup()

    all_data <-
      all_data |>
      mutate(domainSource = documentSource |> urltools::domain()) |>
      dplyr::select(idVGKG:dateTimeDocument, domainSource, everything())

    return(all_data)
    }, error = function(e) {
      cli::cli_alert_danger("Failed: {e$message}")
      return(tibble())
    })
  }

.parse_xml_extra <-
  function(x =  "<PAGE_LINKS>http://therealdeal.com/2015/05/07/hfz-secures-1b-in-financing-for-high-line-site/;http://therealdeal.com/2015/09/10/uk-hedge-fund-loaning-850m-for-relateds-hudson-yards-resi-tower/;http://therealdeal.com/2016/02/05/hfz-seeks-250m-from-eb-5-investors-for-high-line-condos/;http://therealdeal.com/2016/07/28/macklowe-seeking-1b-loan-for-1-wall-street-conversion/;http://therealdeal.com/2016/08/18/first-look-floor-plans-at-hfzs-high-line-development/;http://therealdeal.com/2016/10/05/inside-gary-barnetts-game-of-real-estate-tetris/;http://therealdeal.com/2016/10/06/six-senses-to-open-hotel-at-hfzs-high-line-project/;http://therealdeal.com/issues_articles/whos-bankrolling-the-boom/;http://therealdeal.com/issues_articles/ziel-feldman-it-was-worth-every-penny/</PAGE_LINKS><PAGE_PRECISEPUBTIMESTAMP>20161025180000</PAGE_PRECISEPUBTIMESTAMP>") {
    safe_xml <-
      possibly(read_xml, otherwise = NULL)

    gdelt_xml <-
      list("<item>", x, "</item>") |>
      (\(lst) invoke(paste0, lst))() |>
      safe_xml()

    if (length(gdelt_xml) > 0) {
      values <-
        gdelt_xml |>
        xml_children() |>
        html_text()

      items <-
        gdelt_xml |>
        xml_children() |>
        xml_name()

      xml_df <-
        tibble(item = items, value = values)

      xml_df <-
        xml_df$item |>
        unique() |>
        map_dfr(function(x) {
          item_value <-
            xml_df |>
            filter(item == x) |>
            dplyr::pull(value) |>
            str_split('\\;') |>
            list_c()

          tibble(item = x, value = item_value) |>
            group_by(item) |>
            mutate(idXMLItem = seq_along(item)) |>
            ungroup() |>
            dplyr::select(idXMLItem, item, value)
        })
    } else {
      xml_df <-
        tibble()
    }
    return(xml_df)
  }


#' Parse XML Extra
#'
#' @param data A \code{tibble} of data to process.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#'
#' @return A `tibble` of parsed XML extra data from GKG records
#' @export
#'
#' @examples
#' \dontrun{
#' parse_xml_extras()
#' }
parse_xml_extras <-
  function(data, return_wide = FALSE) {
    xml_data <-
      data |>
      filter(!xmlExtras |> is.na()) |>
      select(idGKG, xmlExtras)
    parse_xml_extra_safe <-
      possibly(.parse_xml_extra, NULL)

    xml_extra_df <-
      1:nrow(xml_data) |>
      map_dfr(function(x) {
        row_data <-
          xml_data |>
          slice(x)

        has_data <-
          row_data$xmlExtras |>
          parse_xml_extra_safe() |> nrow() > 0

        if (has_data) {
          xml_df <-
            row_data$xmlExtras |>
            parse_xml_extra_safe() |>
            mutate(idGKG = row_data$idGKG) |>
            dplyr::select(idGKG, everything())
          return(xml_df)
        }
      })

    xml_extra_df <-
      xml_extra_df |>
      dplyr::filter(!value |> is.na()) |>
      suppressWarnings()

    if (return_wide) {
      xml_extra_df <-
        xml_extra_df |>
        mutate(idXMLItem = idXMLItem - 1,
               item = ifelse(idXMLItem > 0, paste0(item, idXMLItem), item)) |>
        dplyr::select(-idXMLItem) |>
        tidyr::pivot_wider(names_from = item, values_from = value)
    }

    return(xml_extra_df)
  }

#' Parse XML Labels
#'
#' @param data A \code{tibble} of data to process.
#' @param id_dateTime Character string. DateTime identifier column name.
#'
#' @return A `tibble` of cleaned VGKG count data in long or wide format
#'
#' @examples
#' \dontrun{
#' get_clean_count_vkg_data()
#' }

get_clean_count_vkg_data <-
  function(all_counts,
           extra_key = NA,
           count_col = 'idImageLabel',
           return_wide = TRUE) {
    if (!extra_key |> is.na()) {
      clean_data <-
        all_counts |>
        dplyr::rename(count = !!rlang::sym(count_col),
                      ek = !!rlang::sym(extra_key)) |>
        group_by(idVGKG) |>
        mutate(count = 1:n()) |>
        ungroup() |>
        mutate(count = count - 1) |>
        tidyr::pivot_longer(cols = -c(idVGKG, count, ek), names_to = "item", values_to = "value", values_drop_na = TRUE) |>
        mutate(item = ifelse(count == 0, item, item |> paste0(count)),
               value = value |> str_trim()) |>
        separate(
          idVGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = FALSE
        ) |>
        mutate(dateTime = dateTime |> as.numeric()) |>
        arrange(dateTime, count) |>
        dplyr::select(-c(dateTime, GKG))
    } else {
      clean_data <-
        all_counts |>
        dplyr::rename(count = !!rlang::sym(count_col)) |>
        group_by(idVGKG) |>
        mutate(count = 1:n()) |>
        ungroup() |>
        mutate(count = count - 1) |>
        tidyr::pivot_longer(cols = -c(idVGKG, count), names_to = "item", values_to = "value", values_drop_na = TRUE) |>
        mutate(item = ifelse(count == 0, item, item |> paste0(count)),
               value = value |> str_trim()) |>
        separate(
          idVGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = FALSE
        ) |>
        mutate(dateTime = dateTime |> as.numeric()) |>
        arrange(dateTime, count) |>
        dplyr::select(-c(dateTime, GKG))

    }

    if (return_wide) {
      if (!extra_key |> is.na()) {
        names_order <-
          c('idVGKG', 'ek',  clean_data$item |> unique())
        clean_data <-
          clean_data |>
          dplyr::select(-count)
      } else {
        names_order <-
          c('idVGKG',  clean_data$item |> unique())
        clean_data <-
          clean_data |>
          dplyr::select(-count) |>
          tidyr::pivot_wider(names_from = item, values_from = value)
      }
      keywords <-
        c(
          'amountValue',
          'latitude',
          'scoreGoldstein',
          'articleWordCount',
          'longitude',
          'countEvent',
          'idTypeLocation',
          'charLoc',
          'length',
          'count',
          'month',
          'day',
          'year',
          'score'
        )

      length_nums <-
        clean_data |>
        dplyr::select(dplyr::matches(keywords |> paste0(collapse = '|'))) |>
        dplyr::select(-dplyr::matches('namePerson|idCountry')) |>
        names() |> length()

      if (length_nums > 0) {
        numeric_vars <-
          clean_data |>
          dplyr::select(dplyr::matches(keywords |> paste0(collapse = '|'))) |>
          dplyr::select(-dplyr::matches('namePerson|idCountry')) |>
          names()
        clean_data <-
          clean_data |>
          dplyr::mutate(dplyr::across(dplyr::all_of(numeric_vars), as.numeric))

      }

      clean_data <-
        clean_data |>
        dplyr::select(dplyr::all_of(names_order)) |>
        separate(
          idVGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = FALSE
        ) |>
        mutate(dateTime = dateTime |> as.numeric()) |>
        arrange(dateTime) |>
        dplyr::select(-c(GKG, dateTime))

      if (!extra_key |> is.na()) {
        names(clean_data)[2] <-
          extra_key
      }
    } else {
      clean_data <-
        clean_data |>
        dplyr::select(-count) |>
        separate(
          idVGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = FALSE
        ) |>
        mutate(dateTime = dateTime |> as.numeric()) |>
        arrange(dateTime) |>
        dplyr::select(-c(GKG, dateTime))

    }

    return(clean_data)
  }

parse_xml_labels <-
  function(data, id_vgkg = 1)
  {
    xmlData <-
      data |>
      dplyr::filter(idVGKG == id_vgkg) |>
      dplyr::pull(xmlLabels)

    if (xmlData |> is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      if (xmlData |> str_detect('<RECORD>')) {
        xmlData <-
          xmlData |>
          str_split('<RECORD>') |>
          list_c()
      }


      xml_df <-
        tibble(xmlData) |>
        separate(
          xmlData,
          into = c('nameLabel', 'scoreConfidenceLabel', 'midGoogle'),
          sep = '<FIELD>'
        ) |>
        mutate(
          idVGKG = id_vgkg,
          idImageLabel = 1:n(),
          scoreConfidenceLabel = scoreConfidenceLabel |> as.numeric()
        ) |>
        dplyr::select(idVGKG, idImageLabel, everything()) |>
        suppressWarnings()
    }

    return(xml_df)

  }

#' Parses Cloud Vision Lables
#'
#' @param gdelt_data A \code{tibble} of GDELT data to parse.
#' @param filter_na Logical. If \code{TRUE}, removes rows with \code{NA} values.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#'
#' @return A `tibble` of parsed Cloud Vision labels from VGKG data
#' @export
#' @examples
#' \dontrun{
#' parse_vgkg_labels()
#' }
parse_vgkg_labels <- function(gdelt_data,
                              filter_na = TRUE,
                              return_wide = TRUE) {
  parse_xml_labels_safe <-
    possibly(parse_xml_labels, tibble())

  allxmlLabels <-
    gdelt_data$idVGKG |>
    map_dfr(function(x) {
      parse_xml_labels_safe(data = gdelt_data, id_vgkg = x)
    }) |>
    suppressWarnings()

  allxmlLabels <-
    allxmlLabels |>
    get_clean_count_vkg_data(count_col = 'idImageLabel', return_wide = return_wide)

  if (!return_wide) {
    allxmlLabels <-
      allxmlLabels |>
      .resolve_long_names()
  }

  return(allxmlLabels)
}


#' Parses XML Landmarks
#'
#' @param data A \code{tibble} of data to process.
#' @param id_vgkg Integer. Row index into the VGKG data.
#'
#' @return A `tibble` of parsed XML landmark data for the specified VGKG record
#'
#' @examples
#' \dontrun{
#' parse_xml_landmarks()
#' }
parse_xml_landmarks <-
  function(data, id_vgkg = "20160610194500-2247") {
    xmlData <-
      data |>
      dplyr::filter(idVGKG == id_vgkg) |>
      dplyr::pull(xmlGeoLandmarks)

    if (xmlData |> is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      if (xmlData |> str_detect('<RECORD>')) {
        xmlData <-
          xmlData |>
          str_split('<RECORD>') |>
          list_c()
      }


      xml_df <-
        tibble(xmlData) |>
        separate(
          xmlData,
          into = c(
            'nameLandmark',
            'scoreConfidenceLandmark',
            'midGoogle',
            'latLonLandmark'
          ),
          sep = '<FIELD>'
        ) |>
        separate(
          col = 'latLonLandmark',
          sep = '\\,',
          into = c('latitudeLandmark', 'longitudeLandmark')
        ) |>
        mutate(
          idVGKG = id_vgkg,
          idLandmarkImage = 1:n(),
          scoreConfidenceLandmark = scoreConfidenceLandmark |> as.numeric(),
          latitudeLandmark = latitudeLandmark |> as.numeric(),
          longitudeLandmark = longitudeLandmark |> as.numeric()
        ) |>
        dplyr::select(idVGKG, idLandmarkImage, everything()) |>
        suppressWarnings()
    }
    return(xml_df)

  }

#' Parses Cloud Vision Landmark
#'
#' @param gdelt_data A \code{tibble} of GDELT data to parse.
#' @param filter_na Logical. If \code{TRUE}, removes rows with \code{NA} values.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#'
#' @return A `tibble` of parsed Cloud Vision landmarks from VGKG data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_vgkg_landmarks()
#' }
parse_vgkg_landmarks <- function(gdelt_data,
                                 filter_na = TRUE,
                                 return_wide = TRUE) {
  parse_xml_landmarks_safe <-
    possibly(parse_xml_landmarks, tibble())

  all_data <-
    gdelt_data$idVGKG |>
    map_dfr(function(x) {
      parse_xml_landmarks_safe(data = gdelt_data, id_vgkg = x)
    }) |>
    suppressWarnings()

  if (filter_na) {
    all_data <-
      all_data |>
      dplyr::filter(!nameLandmark |> is.na())
  }

  all_data <-
    all_data |>
    get_clean_count_vkg_data(count_col = 'idLandmarkImage', return_wide = return_wide)

  if (!return_wide) {
    all_data <-
      all_data |>
      .resolve_long_names()
  }
  return(all_data)
}


#' Parses XML Logo
#'
#' @param data A \code{tibble} of data to process.
#' @param id_vgkg Integer. Row index into the VGKG data.
#'
#' @return A `tibble` of parsed XML logo data for the specified VGKG record
#'
#' @examples
#' \dontrun{
#' parse_xml_logos()
#' }
parse_xml_logos <-
  function(data, id_vgkg = 1) {
    xmlData <-
      data |>
      dplyr::filter(idVGKG == id_vgkg) |>
      dplyr::pull(xmlLogos)

    if (xmlData |> is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      xmlData <-
        xmlData |>
        str_split('<RECORD>') |>
        list_c()

      xml_df <-
        tibble(xmlData) |>
        separate(
          xmlData,
          into = c('nameLogo', 'scoreConfidenceLogo', 'midGoogle'),
          sep = '<FIELD>'
        ) |>
        mutate(
          idVGKG = id_vgkg,
          idLogoImage = 1:n(),
          scoreConfidenceLogo = scoreConfidenceLogo |> as.numeric()
        ) |>
        dplyr::select(idVGKG, idLogoImage, everything()) |>
        suppressWarnings()
    }

    return(xml_df)

  }

#' Parses Cloud Vision Logos
#'
#' @param gdelt_data A \code{tibble} of GDELT data to parse.
#' @param filter_na Logical. If \code{TRUE}, removes rows with \code{NA} values.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#'
#' @return A `tibble` of parsed Cloud Vision logos from VGKG data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_vgkg_logos()
#' }
parse_vgkg_logos <- function(gdelt_data,
                             filter_na = TRUE,
                             return_wide = TRUE) {
  parse_xml_logos_safe <-
    possibly(parse_xml_logos,tibble())

  all_data <-
    gdelt_data$idVGKG |>
    map_dfr(function(x) {
      parse_xml_logos_safe(data = gdelt_data, id_vgkg = x)
    }) |>
    suppressWarnings()

  all_data <-
    all_data |>
    mutate(midGoogle = ifelse(midGoogle == '', NA, midGoogle))

  if (filter_na) {
    all_data <-
      all_data |>
      dplyr::filter(!nameLogo |> is.na())
  }

  all_data <-
    all_data |>
    get_clean_count_vkg_data(count_col = 'idLogoImage', return_wide = return_wide)

  if (!return_wide) {
    all_data <-
      all_data |>
      .resolve_long_names() |>
      suppressWarnings()
  }

  return(all_data)
}

#' Parses Safe Search
#'
#' @param data A \code{tibble} of data to process.
#' @param id_vgkg Integer. Row index into the VGKG data.
#'
#' @return A `tibble` of parsed XML safe search data for the specified VGKG record
#'
#' @examples
#' \dontrun{
#' parse_xml_safe_search()
#' }
parse_xml_safe_search <-
  function(data, id_vgkg = 1) {
    xmlData <-
      data |>
      dplyr::filter(idVGKG == id_vgkg) |>
      dplyr::pull(xmlSafeSearch)

    if (xmlData |> is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      xmlData <-
        xmlData |>
        str_split('<RECORD>') |>
        list_c()

      xml_df <-
        tibble(xmlData) |>
        separate(
          xmlData,
          into = c(
            'scoreViolenceLikelihood',
            'scoreMedicalLikelihood',
            'scoreSpoofLikelihood',
            'scoreAdultLikelihood'
          ),
          sep = '<FIELD>'
        )

      xml_df <-
        xml_df |>
        dplyr::mutate(dplyr::across(dplyr::matches("score"), as.integer)) |>
        mutate(idVGKG = id_vgkg,
               idSafeSearchImage = 1:n()) |>
        dplyr::select(idVGKG, idSafeSearchImage, everything()) |>
        suppressWarnings() |>
        distinct()
    }

    return(xml_df)

  }

#' Parses Safe Search
#'
#' @param gdelt_data A \code{tibble} of GDELT data to parse.
#' @param filter_na Logical. If \code{TRUE}, removes rows with \code{NA} values.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#'
#' @return A `tibble` of parsed Cloud Vision safe search scores from VGKG data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_vgkg_safe_search()
#' }
parse_vgkg_safe_search <- function(gdelt_data,
                                   filter_na = TRUE,
                                   return_wide = TRUE) {
  parse_xml_safe_search_safe <-
    possibly(parse_xml_safe_search, tibble())

  all_data <-
    gdelt_data$idVGKG |>
    map_dfr(function(x) {
      parse_xml_safe_search_safe(data = gdelt_data, id_vgkg = x)
    }) |>
    suppressWarnings()

  if (filter_na) {
    all_data <-
      all_data |>
      dplyr::filter(!idSafeSearchImage |> is.na())
  }

  if (!return_wide) {
    all_data <-
      all_data |>
      .resolve_long_names()
  }

  return(all_data)
}

#' Parses XML Faces
#'
#' @param data A \code{tibble} of data to process.
#' @param id_vgkg Integer. Row index into the VGKG data.
#'
#' @return A `tibble` of parsed XML face detection data for the specified VGKG record
#'
#' @examples
#' \dontrun{
#' parse_xml_faces()
#' }
parse_xml_faces <-
  function(data, id_vgkg = 1) {
    xmlData <-
      data |>
      dplyr::filter(idVGKG == id_vgkg) |>
      dplyr::pull(xmlFaces)

    if (xmlData |> is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      xmlData <-
        xmlData |>
        str_split('<RECORD>') |>
        list_c()

      xml_df <-
        tibble(xmlData) |>
        separate(
          xmlData,
          into = c(
            'scoreDetectionConfidence',
            'angleRoll',
            'anglePan',
            'angleTilt',
            'scoreLandmarkingConfidence',
            'boxBounding',
            'scoreEmotionSorrowLikelihood',
            'scoreEmotionAngerLikelihood',
            'scoreHeadwearLikelihood',
            'scoreEmotionJoyLikelihood',
            'scoreEmotionSurpriseLikelihood',
            'scoreUnderExposedLikelihood',
            'scoreBlurredLikelihood'
          ),
          sep = '<FIELD>'
        ) |>
        separate('boxBounding', sep = ';', c('xy1', 'xy2', 'xy3', 'xy4')) |>
        separate('xy1', sep = ',', c('faceX1', 'faceY1')) |>
        separate('xy2', sep = ',', c('faceX2', 'faceY2')) |>
        separate('xy3', sep = ',', c('faceX3', 'faceY3')) |>
        separate('xy4', sep = ',', c('faceX4', 'faceY4'))

      xml_df <-
        xml_df |>
        dplyr::mutate(dplyr::across(dplyr::matches("score|face|angle"), as.numeric)) |>
        mutate(idVGKG = id_vgkg,
               idFace = 1:n()) |>
        dplyr::select(idVGKG, idFace, everything()) |>
        suppressWarnings() |>
        distinct()
    }

    return(xml_df)

  }

#' Parses Cloud Vision Faces
#'
#' @param gdelt_data A \code{tibble} of GDELT data to parse.
#' @param filter_na Logical. If \code{TRUE}, removes rows with \code{NA} values.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#'
#' @return A `tibble` of parsed Cloud Vision face detection results from VGKG data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_vgkg_faces()
#' }
parse_vgkg_faces <- function(gdelt_data,
                             filter_na = TRUE,
                             return_wide = TRUE) {
  parse_xml_faces_search_safe <-
    possibly(parse_xml_faces, tibble())

  all_data <-
    gdelt_data$idVGKG |>
    map_dfr(function(x) {
      parse_xml_faces_search_safe(data = gdelt_data, id_vgkg = x)
    }) |>
    suppressWarnings()

  if (filter_na) {
    all_data <-
      all_data |>
      dplyr::filter(!scoreDetectionConfidence |> is.na())
  }

  all_data <-
    all_data |>
    get_clean_count_vkg_data(count_col = 'idFace', return_wide = return_wide)

  if (!return_wide) {
    all_data <-
      all_data |>
      .resolve_long_names()
  }

  return(all_data)
}

#' Parses XML OCR
#'
#' @param data A \code{tibble} of data to process.
#' @param id_vgkg Integer. Row index into the VGKG data.
#'
#' @return A `tibble` of parsed XML OCR text for the specified VGKG record
#'
#' @examples
#' \dontrun{
#' parse_xml_ocr()
#' }
parse_xml_ocr <-
  function(data, id_vgkg = 1)  {
    xmlData <-
      data |>
      dplyr::filter(idVGKG == id_vgkg) |>
      dplyr::pull(xmlOCR)

    if (xmlData |> is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      xmlData <-
        xmlData |>
        str_split('<RECORD>') |>
        list_c() |>
        (\(x) gsub('\\n', '', x))()

      xml_df <-
        tibble(textOCR = xmlData)

      xml_df <-
        xml_df |>
        mutate(idVGKG = id_vgkg,
               idItemOCR = 1:n()) |>
        dplyr::select(idVGKG, idItemOCR, everything()) |>
        suppressWarnings() |>
        distinct()
    }
    return(xml_df)

  }

#' Parses Cloud Vision OCR
#'
#' @param gdelt_data A \code{tibble} of GDELT data to parse.
#' @param filter_na Logical. If \code{TRUE}, removes rows with \code{NA} values.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#'
#' @return A `tibble` of parsed Cloud Vision OCR text from VGKG data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_vgkg_ocr()
#' }
parse_vgkg_ocr <- function(gdelt_data,
                           filter_na = TRUE,
                           return_wide = TRUE) {
  parse_xml_ocr_safe <-
    possibly(parse_xml_ocr, tibble())

  all_data <-
    gdelt_data$idVGKG |>
    map_dfr(function(x) {
      parse_xml_ocr_safe(data = gdelt_data, id_vgkg = x)
    }) |>
    suppressWarnings()

  if (filter_na) {
    all_data <-
      all_data |>
      dplyr::filter(!idItemOCR |> is.na())
  }

  all_data <-
    all_data |>
    get_clean_count_vkg_data(count_col = 'idItemOCR', return_wide = return_wide)

  if (!return_wide) {
    all_data <-
      all_data |>
      .resolve_long_names()
  }

  return(all_data)
}


#' Parses XML Language Type
#'
#' @param data A \code{tibble} of data to process.
#' @param id_vgkg Integer. Row index into the VGKG data.
#'
#' @return A `tibble` of parsed language type data for the specified VGKG record
#'
#' @examples
#' \dontrun{
#' parse_language_types()
#' }
parse_language_types <-
  function(data, id_vgkg = 1)  {
    xmlData <-
      data |>
      dplyr::filter(idVGKG == id_vgkg) |>
      dplyr::pull(codesLanguages)

    if (xmlData |> is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      xmlData <-
        xmlData |>
        str_split('\\,') |>
        list_c()

      xml_df <-
        tibble(idLanguage = xmlData) |>
        mutate(idVGKG = id_vgkg,
               idItemLanguage = 1:n()) |>
        dplyr::select(idVGKG, idItemLanguage, everything()) |>
        suppressWarnings()
    }

    return(xml_df)

  }

#' Parses Cloud Vision Languages
#'
#' @param gdelt_data A \code{tibble} of GDELT data to parse.
#' @param filter_na Logical. If \code{TRUE}, removes rows with \code{NA} values.
#' @param return_wide Logical. If \code{TRUE}, returns data in wide format; otherwise long format.
#'
#' @return A `tibble` of parsed Cloud Vision detected languages from VGKG data
#' @export
#'
#' @examples
#' \dontrun{
#' parse_vgkg_languages()
#' }
parse_vgkg_languages <- function(gdelt_data,
                                 filter_na = TRUE,
                                 return_wide = TRUE) {
  parse_language_types_safe <-
    possibly(parse_language_types, tibble())

  all_data <-
    gdelt_data$idVGKG |>
    map_dfr(function(x) {
      parse_language_types_safe(data = gdelt_data, id_vgkg = x)
    }) |>
    suppressWarnings()

  if (filter_na) {
    all_data <-
      all_data |>
      dplyr::filter(!idItemLanguage |> is.na())
  }

  all_data <-
    all_data |>
    get_clean_count_vkg_data(count_col = 'idItemLanguage', return_wide = return_wide)

  if (!return_wide) {
    all_data <-
      all_data |>
      .resolve_long_names()
  }

  return(all_data)
}


#' Gets most recent GKG TV log URLs
#'
#' @return A `tibble` of GKG TV log URLs with date metadata
#'
#' @export
#'
#' @examples get_urls_gkg_most_recent_log()
get_urls_gkgtv_most_recent_log <- function() {
  urlData <-
    'http://data.gdeltproject.org/gdeltv2_iatelevision/lastupdate.txt' |>
    read_tsv(col_names = FALSE) |>
    dplyr::select(-1) |>
    suppressMessages()

  names(urlData) <-
    c('idHash', 'urlData')

  urlData <-
    urlData |>
    mutate(
      dateData = urlData |> str_replace_all(
        'http://data.gdeltproject.org/gdeltv2_iatelevision/|.gkg.csv.gz',
        ''
      ) |> as.Date('%Y%m%d')
    ) |>
    dplyr::select(dateData, everything())

  return(urlData)
}

#' Retrieves GKG TV Schema
#'
#' @return A `tibble` of GKG TV column names and descriptions
#' @examples
#' get_tv_schema

get_tv_schema  <- function() {
  tv_schema <-
    tibble(
      nameGDELT = c(
        "GKGRECORDID",
        "DATE",
        "SourceCollectionIdentifier",
        "SourceCommonName",
        "DocumentIdentifier",
        "Counts",
        "V2Counts",
        "Themes",
        "V2Themes",
        "Locations",
        "V2Locations",
        "Persons",
        "V2Persons",
        "Organizations",
        "V2Organizations",
        "V2Tone",
        "Dates",
        "GCAM",
        "SharingImage",
        "RelatedImages",
        "SocialImageEmbeds",
        "SocialVideoEmbeds",
        "Quotations",
        "AllNames",
        "Amounts",
        "TranslationInfo",
        "Extras"
      ),
      nameActual = c(
        "idGKG",
        "dateDocument",
        "idSourceCollectionIdentifier",
        "nameSource",
        "documentSource",
        "counts",
        "countsCharLoc",
        "themes",
        "themesCharLoc",
        "locations",
        "locationsCharLoc",
        "persons",
        "personsCharLoc",
        "organizations",
        "organizationsCharLoc",
        "tone",
        "dates",
        "gcam",
        "urlImage",
        "urlImageRelated",
        "urlSocialMediaImageEmbeds",
        "urlSocialMediaVideoEmbeds",
        "quotations",
        "mentionedNamesCounts",
        "mentionedNumericsCounts",
        "translationInfo",
        "xmlExtras"
      )
    )
  return(tv_schema)

}
#' Gets Global Knowledge Graph TV Summary Files
#'
#' @param remove_count_files Logical. If \code{TRUE}, removes count files after processing.
#' @param return_message Logical. If \code{TRUE}, prints progress messages to console.
#'
#' @return A `tibble` of GKG TV daily summary URLs with date metadata
#' @export
#'
#' @examples
#' get_urls_gkg_daily_summaries(remove_count_files = TRUE)
dictionary_gkg_tv_daily_summaries <-
  function(return_message = TRUE) {
    url <-
      'http://data.gdeltproject.org/gdeltv2_iatelevision/masterfilelist.txt'

    urlData <-
      url |>
      read_tsv(col_names = FALSE) |>
      dplyr::select(-1) |>
      suppressMessages()

    names(urlData) <-
      c('idHash', 'urlData')

    urlData <-
      urlData |>
      mutate(
        dateData = urlData |> str_replace_all(
          'http://data.gdeltproject.org/gdeltv2_iatelevision/|.gkg.csv.gz',
          ''
        ) |> as.Date('%Y%m%d')
      ) |>
      dplyr::select(dateData, everything())

    if (return_message) {
      count.files <-
        urlData |>
        nrow()

      min.date <-
        urlData$dateData |> min(na.rm = TRUE)

      max.date <-
        urlData$dateData |> max(na.rm = TRUE)

      cli::cli_alert_info("You got {count.files} GDELT TV Global Knowledge Graph URLS from {min.date} to {max.date}")
    }

    return(urlData)
  }

.get_data_gkg_tv <-
  function(url = 'http://data.gdeltproject.org/gdeltv2_iatelevision/20160609.gkg.csv.gz',
           return_message = TRUE) {
    ok_url <-
      url |> url_ok() |> suppressWarnings()
    if (ok_url == FALSE) {
      stop("Invalid url")
    }

    gkg_tv_data <-
      url |>
      curl() |>
      gzcon() |>
      read_tsv(col_names = FALSE) |>
      suppressWarnings() |>
      suppressMessages()

    names(gkg_tv_data) <-
      get_tv_schema() |> dplyr::pull(nameActual)

    gkg_tv_data <-
      gkg_tv_data |>
      dplyr::mutate(
        idSourceCollectionIdentifier = idSourceCollectionIdentifier |> as.numeric(),
        isDocumentURL = ifelse(documentSource |> str_detect('http'), TRUE, FALSE)
      ) |>
      dplyr::select(idGKG:idSourceCollectionIdentifier,
                    isDocumentURL,
                    everything()) |>
      dplyr::rename(dateTimeDocument = dateDocument) |>
      dplyr::mutate(dateTimeDocument = dateTimeDocument |> lubridate::ymd_hms() |> with_tz(Sys.timezone())) |>
      suppressMessages() |>
      suppressWarnings()

    gkg_tv_data <-
      gkg_tv_data |>
      mutate(docDetails = sub('\\_', '\\-', sub('\\_', '\\-', sub('\\_', '\\-', documentSource)))) |>
      separate(
        docDetails,
        sep = '\\-',
        into = c('idTVNetwork', 'date', 'time', 'nameTVShow')
      ) |>
      dplyr::select(-c(date, time)) |>
      mutate(
        nameTVShow = nameTVShow |> str_replace_all('\\_', ' '),
        urlArchiveVideo = paste0('https://archive.org/details/', documentSource)
      ) |>
      dplyr::select(idGKG:documentSource,
                    urlArchiveVideo,
                    idTVNetwork,
                    nameTVShow,
                    everything())

    if (return_message) {
      cli::cli_alert_info("Downloaded, parsed and imported {url}")
    }
    return(gkg_tv_data)
  }

.get_data_gkg_tv_day <- function(date_data = "2016-06-01",
                                only_most_recent = FALSE,
                                return_message = TRUE) {
  if (only_most_recent) {
    date_data <-
      Sys.Date() - 2
  }

  if (!date_data |> substr(5, 5) == "-") {
    stop("Sorry data must be in YMD format, ie, 2016-06-01")
  }

  date_data <-
    date_data |>
    ymd() |> as.Date()


  if (date_data < "2009-06-04") {
    stop("Sorry data starts on June 4th, 2009")
  }

  if (date_data > Sys.Date() - 2) {
    stop("Sorry data can't go into the future is on a 2 day lag")
  }

  if (only_most_recent) {
    gkg_recent <-
      get_urls_gkgtv_most_recent_log()
    urls <-
      gkg_recent |>
      dplyr::pull(urlData)
  } else {
    gkg_tv_urls <- .cache_get('gkg_tv_urls')
    if (is.null(gkg_tv_urls)) {
      gkg_tv_urls <-
        dictionary_gkg_tv_daily_summaries()
      .cache_set('gkg_tv_urls', gkg_tv_urls)
    }

    urls <-
      gkg_tv_urls |>
      dplyr::filter(dateData == date_data) |>
      dplyr::pull(urlData)
  }

  get_data_gkg_tv_safe <-
    possibly(.get_data_gkg_tv, tibble())

  all_data <-
    urls |>
    map_dfr(function(x) {
      get_data_gkg_tv_safe(url = x,
                      return_message = return_message)
    }) |>
    distinct() |>
    suppressMessages() |>
    suppressWarnings()

  if (return_message) {
    cli::cli_alert_info("You retrieved {nrow(all_data)} gkg tv events for {date_data}")
  }

  return(all_data)
}


#' Gets GKG TV data for specified days
#'
#' @param dates specified dates, year, month day format
#' @param only_most_recent returns only the most recent period
#' \code{c(TRUE, FALSE)}
#' @param return_message Logical. If \code{TRUE}, prints progress messages to console.
#'
#' @return A `tibble` of GKG TV data for the specified dates
#' @export
#' @examples
#' \dontrun{
#' gkg_tv_days()
#' }
gkg_tv_days <-
  function(dates = c("2016-06-01", "2016-02-01"),
           only_most_recent = FALSE,
           return_message = TRUE) {
    get_data_gkg_tv_day_safe <-
      possibly(.get_data_gkg_tv_day, tibble())

    all_data <-
      dates |>
      map_dfr(
        function(x)
          get_data_gkg_tv_day_safe(
            date_data = x,
            only_most_recent = only_most_recent,
            return_message = return_message
          )
      ) |>
      suppressWarnings()

    return(all_data)
  }


#' Convert date columns to numeric
#'
#' @param data A \code{tibble} of data to process.
#' @param column_keyword Keyword of the data frame's columns to look for that signifies dates
#' \code{c('date')}
#' @param exclude_col_type The types of column to exclude from converting to numeric
#' \code{c('character')}
#' @param return_message  return a message
#' @return A `tibble` with date columns converted to numeric format
#' @export
#'
#' @examples
#' \dontrun{
#' date_columns_to_numeric()
#' }
date_columns_to_numeric <-
  function(data,
           column_keyword = 'date',
           exclude_col_type = c('character'),
           return_message = TRUE) {
    name_df <-
      data |>
      dplyr::select(dplyr::matches(column_keyword)) |>
      map(class) |>
      unlist() |>
      data.frame(col_class = .) |>
      rownames_to_column()

    exclude_cols <-
      name_df |>
      dplyr::filter(col_class %in% col_type) |>
      dplyr::pull(rowname)

    mutate_cols <-
      data |>
      dplyr::select(dplyr::matches(column_keyword)) |>
      dplyr::select(-any_of(exclude_cols)) |> names()

    data <-
      data |>
      dplyr::mutate(dplyr::across(dplyr::all_of(mutate_cols), as.numeric))

    if (return_message) {
      cols_str <- paste0(mutate_cols, collapse = ", ")
      cli::cli_alert_info("You converted {cols_str} to numeric")
    }

    return(data)

  }


# trelliscope -------------------------------------------------------------
check_for_trelliscope_js <-
  function() {
    missing <-
      installed.packages() |> dplyr::as_tibble() |>
      dplyr::filter(Package == 'trelliscopejs') |>
      nrow() == 0
    if (missing) {
      devtools::install_github("hafen/trelliscopejs")
    }
  }

.filter_domain_data <-
  function(data,
           domains = c(
             'wsj.com',
             'law360',
             'nytimes.com',
             'dailymail',
             'recode',
             'architizer',
             'globest',
             'realert',
             'seekingalpha.com',
             'bloomberg.com',
             'curbed.com',
             'commercialobserver.com',
             'globest.com',
             'realdeal.com',
             'techcrunch.com',
             'nytimes.com',
             'architizer',
             'pehub',
             'rew-online',
             'nypost'
           ),
           random_domains = 20,
           only_pictures = TRUE) {
    search_domains <-
      c()
    if (!random_domains |> is_null()) {
      randoms <-
        data$domainSource |> unique() |> sample(random_domains)
      search_domains <-
        c(search_domains, randoms) |> unique()
    }

    if (!domains |> is_null()) {
      search_domains <-
        c(search_domains, domains)
    }

    search_domains <-
      search_domains |>
      unique() |>
      paste(collapse = '|')

    data <-
      data |>
      filter(domainSource |> str_detect(search_domains))

    if (only_pictures) {
      data <-
        data |>
        filter(!urlImage |> is.na())
    }

    return(data)
  }

#' Visualize Cloudvision trelliscope
#'
#' @param data A \code{tibble} of data to process.
#' @param extra_columns Character vector of additional columns to include.
#' @param trelliscope_parameters \itemize{
#' \item title: Trelliscope title
#' \item rows: Trelliscope rows
#' \item columns: Trelliscope columns
#' \item path: file path to save trelliscope, if NULL no where
#' }
#' @param vgkg_parse \itemize{
#' \item faces,
#' \item labels,
#' \item landmarks,
#' \item languages,
#' \item logos,
#' \item ocr
#' }
#' @param domains Character vector of domain names to filter.
#' @param random_domains Integer. Number of random domains to sample.
#' @param only_pictures Logical. If \code{TRUE}, returns only picture-containing entries.
#'
#' @return A trelliscope visualization of VGKG Cloud Vision data
#' @export
#' @examples
#' \dontrun{
#' visualize_vgkg_trelliscope()
#' }
visualize_vgkg_trelliscope <-
  function(data,
           vgkg_parse = 'landmarks',
           domains = c(
             'wsj.com',
             'seekingalpha.com',
             'bloomberg.com',
             'curbed.com',
             'commercialobserver.com',
             'globest.com',
             'realdeal.com',
             'techcrunch.com',
             'nytimes.com',
             'architizer',
             'pehub',
             'rew-online.com',
             'nypost.com'
           ),
           trelliscope_parameters = list(
             title = NULL,
             path = NULL,
             rows = NULL,
             columns = NULL
           ),
           random_domains = 0,
           only_pictures = TRUE,
           extra_columns = NULL) {
    check_for_trelliscope_js()

    if (!extra_columns |> is_null()) {
      df_extra <-
        data |>
        dplyr::select(idVGKG, any_of(extra_columns))
    }
    has_domains <-
      !domains |> is_null() |
      !random_domains |> is_null()
    if (has_domains) {
      data <-
        data |>
        .filter_domain_data(domains = domains, random_domains = random_domains)
    }
    if (!vgkg_parse |> is_null()) {
      vgkg_options <-
        c('faces',
          'labels',
          'landmarks',
          'languages',
          'logos',
          'ocr')
      vgkg_parse <-
        vgkg_parse |> str_to_lower()
      if (!vgkg_parse %in% vgkg_options) {
        stop(list(
          "Parse options can only be:\n",
          paste(vgkg_options, collapse = '\n')
        ) |> reduce(paste0))
      }

      is_faces <-
        vgkg_parse == 'faces'

      if (is_faces) {
        data <-
          data |>
          parse_vgkg_faces(filter_na = TRUE, return_wide = TRUE) |>
          left_join(data |> select(idVGKG:urlImage, dateDocument)) |>
          suppressMessages() |>
          suppressWarnings()

        main_name <-
          c('scoreDetectionConfidence',
            'scoreEmotionAngerLikelihood')
      }

      is_labels <-
        vgkg_parse == 'labels'

      if (is_labels) {
        data <-
          data |>
          parse_vgkg_labels(filter_na = TRUE, return_wide = FALSE) |>
          left_join(data |> select(idVGKG:urlImage, dateDocument)) |>
          suppressMessages() |>
          suppressWarnings()

        main_name <-
          c('nameLabel', 'scoreConfidenceLabel')
      }

      is_landmark <-
        vgkg_parse == 'landmarks'

      if (is_landmark) {
        data <-
          data |>
          parse_vgkg_landmarks(filter_na = TRUE, return_wide = FALSE) |>
          left_join(data |> select(idVGKG:urlImage, dateDocument)) |>
          suppressMessages() |>
          suppressWarnings()

        main_name <-
          c('nameLandmark', 'scoreConfidenceLandmark')
      }

      is_language <-
        vgkg_parse == 'languages'

      if (is_language) {
        data <-
          data |>
          parse_vgkg_languages(filter_na = TRUE, return_wide = FALSE) |>
          left_join(data |> select(idVGKG:urlImage, dateDocument)) |>
          suppressMessages() |>
          suppressWarnings()
        main_name <-
          c('idLanguage')
      }

      is_ocr <-
        vgkg_parse == 'ocr'

      if (is_ocr) {
        data <-
          data |>
          parse_vgkg_ocr(filter_na = TRUE, return_wide = FALSE) |>
          left_join(data |> select(idVGKG:urlImage, dateDocument)) |>
          suppressMessages() |>
          suppressWarnings()
        main_name <-
          c('textOCR')
      }

      is_logos <-
        vgkg_parse == 'logos'

      if (is_logos) {
        data <-
          data |>
          parse_vgkg_logos(filter_na = TRUE, return_wide = FALSE) |>
          left_join(data |> select(idVGKG:urlImage, dateDocument)) |>
          suppressMessages() |>
          suppressWarnings()
        main_name <-
          c('nameLogo')
      }

    } else {
      main_name <-
        ''
    }

    if (!vgkg_parse |> is_null()) {
      parse_title <-
        vgkg_parse |> str_to_title()
    } else {
      parse_title <- ''
    }

    dates <-
      data$dateTimeDocument |> as.Date("%Y-%m-%d") |> unique() |>
      suppressWarnings()

    dates <-
      dates[!is.na(dates)][[1]]


    title <-
      list('GDELT VGKG Explorer for ',  parse_title, ' ', dates) |>
      reduce(paste0)


    if (!extra_columns |> is_null()) {
      data <-
        data |>
        left_join(df_extra) |>
        suppressMessages()
    }

    data <-
      data |>
      dplyr::mutate(dplyr::across(dplyr::where(is.logical), ~ifelse(is.na(.x), FALSE, .x)))

    if (!trelliscope_parameters$title |> is_null()) {
      title <-
        trelliscope_parameters$title
    }

    if (trelliscope_parameters$rows |> is_null()) {
      rows <- 1
    } else {
      rows <-
        trelliscope_parameters$rows
    }

    if (trelliscope_parameters$columns |> is_null()) {
      columns <- 2
    } else {
      columns <-
        trelliscope_parameters$columns
    }
    no_path <-
      trelliscope_parameters$path |> is_null()

    if (no_path) {
      viz <-
        data |>
        dplyr::select(where(~mean(is.na(.x)) < 0.5)) |>
        mutate(
          panel = trelliscopejs::img_panel(urlImage),
          urlArticle = trelliscopejs::cog_href(documentSource),
          idItem = 1:n()
        ) |>
        select(idItem, idVGKG, everything()) |>
        arrange(idItem) |>
        trelliscopejs::trelliscope(
          name = title,
          nrow = rows,
          ncol = columns,
          state = list(labels = c(
            "domainSource", "idVGKG", main_name[[1]], 'urlArticle'
          ))
        )
    } else {
      file_path <-
        trelliscope_parameters$path
      viz <-
        data |>
        dplyr::select(where(~mean(is.na(.x)) < 0.5)) |>
        mutate(
          panel = trelliscopejs::img_panel(urlImage),
          urlArticle = trelliscopejs::cog_href(documentSource),
          idItem = 1:n()
        ) |>
        select(idItem, idVGKG, everything()) |>
        arrange(idItem) |>
        trelliscopejs::trelliscope(
          name = title,
          nrow = rows,
          ncol = columns,
          path = file_path,
          state = list(labels = c(
            "domainSource", "idVGKG", main_name, 'urlArticle'
          ))
        )
    }

    return(viz)
  }

#' Trelliscope gkg data
#'
#' @param data A \code{tibble} of data to process.
#' @param domains Character vector of domain names to filter.
#' @param trelliscope_parameters \itemize{
#' \item title: Trelliscope title
#' \item rows: Trelliscope rows
#' \item columns: Trelliscope columns
#' \item path: file path to save trelliscope, if NULL no where
#' }
#' @param extra_columns Character vector of additional columns to include.
#' @param random_domains Integer. Number of random domains to sample.
#' @param only_pictures Logical. If \code{TRUE}, returns only picture-containing entries.
#' @param gkg_parse \itemize{
#' \item 'tone',
#' \item 'dates',
#' \item 'events',
#' \item 'gcam',
#' \item 'locations',
#' \item 'names',
#' \item 'numerics',
#' \item 'organizations',
#' \item 'people',
#' \item 'quotes',
#' \item 'social',
#' \item 'themes',
#' \item 'xml
#' }
#'
#' @return A trelliscope visualization of GKG parsed data
#' @export
#' @examples
#' \dontrun{
#' visualize_gkg_trelliscope()
#' }
visualize_gkg_trelliscope <-
  function(data,
           domains = c(
             'wsj.com',
             'law360',
             'nytimes.com',
             'dailymail',
             'recode',
             'architizer',
             'globest',
             'realert',
             'seekingalpha.com',
             'bloomberg.com',
             'curbed.com',
             'commercialobserver.com',
             'globest.com',
             'realdeal.com',
             'techcrunch.com',
             'nytimes.com',
             'architizer',
             'pehub',
             'rew-online',
             'nypost'
           ),
           random_domains = 0,
           only_pictures = TRUE,
           extra_columns = NULL,
           trelliscope_parameters = list(
             title = NULL,
             path = NULL,
             rows = NULL,
             columns = NULL
           ),
           gkg_parse = 'names') {
    check_for_trelliscope_js()
    has_domains <-
      !domains |> is_null() |
      !random_domains |> is_null()
    if (has_domains) {
      data <-
        data |>
        .filter_domain_data(domains = domains, random_domains = random_domains)
    }

    if (!gkg_parse |> is_null()) {
      gkg_options <-
        c(
          'tone',
          'dates',
          'events',
          'gcam',
          'locations',
          'names',
          'numerics',
          'organizations',
          'people',
          'quotes',
          'social',
          'themes',
          'xml'
        )
      gkg_parse <-
        gkg_parse |> str_to_lower()
      if (!gkg_parse %in% gkg_options) {
        stop(list(
          "Parse options can only be:\n",
          paste(gkg_options, collapse = '\n')
        ) |> reduce(paste0))
      }

      is_tone <-
        gkg_parse == 'tone'

      if (is_tone) {
        plot_data <-
          data |>
          parse_gkg_mentioned_article_tone(filter_na = TRUE, return_wide = FALSE) |>
          left_join(data |> select(idGKG:domainSource, urlImage)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('scoreTone')
      }

      is_xml <-
        gkg_parse == 'xml'

      if (is_xml) {
        plot_data <-
          data |>
          filter(!xmlExtras |> is.na()) |>
          parse_xml_extras(return_wide = FALSE) |>
          left_join(data |> select(idGKG:domainSource, urlImage)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('item', 'value')
      }

      is_date <-
        gkg_parse == 'date'

      if (is_date) {
        plot_data <-
          data |>
          parse_gkg_mentioned_dates(filter_na = TRUE, return_wide = FALSE) |>
          left_join(data |> select(idGKG:domainSource, urlImage)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('dateResolution')
      }

      is_events <-
        gkg_parse == 'events'

      if (is_events) {
        plot_data <-
          data |>
          parse_gkg_mentioned_event_counts(return_wide = FALSE, filter_na = TRUE) |>
          left_join(get_codes_gkg_themes()) |>
          left_join(data |> select(idGKG:domainSource, urlImage)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('codeGKGTheme',
            'countEvent',
            'entityEvent',
            'idADM1Code')
      }

      is_gcam <-
        gkg_parse == 'gcam'

      if (is_gcam) {
        plot_data <-
          data |>
          parse_gkg_mentioned_gcams(return_wide = FALSE, filter_na = TRUE) |>
          left_join(get_codes_gcam()) |>
          left_join(data |> select(idGKG:domainSource, urlImage)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('idGCAM', 'dimensionHumanName')
      }

      is_locations <-
        gkg_parse == 'locations'

      if (is_locations) {
        plot_data <-
          data |>
          parse_gkg_mentioned_locations(return_wide = FALSE, filter_na = TRUE) |>
          left_join(get_codes_stability_locations()) |>
          left_join(data |> select(idGKG:domainSource, urlImage)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('idADM1Code', 'location')
      }

      is_names <-
        gkg_parse == 'names'

      if (is_names) {
        plot_data <-
          data |>
          parse_gkg_mentioned_names(return_wide = FALSE, filter_na = TRUE) |>
          left_join(data |> select(idGKG:domainSource, urlImage)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('nameMentionedName')
      }

      is_numerics <-
        gkg_parse == 'numerics'

      if (is_numerics) {
        plot_data <-
          data |>
          parse_gkg_mentioned_numerics(return_wide = FALSE, filter_na = TRUE) |>
          left_join(data |> select(idGKG:domainSource, urlImage)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('amountTerm', 'amountValue')
      }

      is_organizations <-
        gkg_parse == 'organizations'

      if (is_organizations) {
        plot_data <-
          data |>
          parse_gkg_mentioned_organizations(return_wide = FALSE, filter_na = TRUE) |>
          left_join(data |> select(idGKG:domainSource, urlImage)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('nameOrganization')
      }

      is_people <-
        gkg_parse == 'people'

      if (is_people) {
        plot_data <-
          data |>
          parse_gkg_mentioned_people(return_wide = FALSE, filter_na = TRUE) |>
          left_join(data |> select(idGKG:domainSource, urlImage)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('namePerson')
      }

      is_quotes <-
        gkg_parse == 'quotes'

      if (is_quotes) {
        plot_data <-
          data |>
          parse_gkg_mentioned_quotes(return_wide = FALSE, filter_na = TRUE) |>
          left_join(data |> select(idGKG:domainSource, urlImage)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('lengthQuote', 'textQuote')
      }

      is_social <-
        gkg_parse == 'social'

      if (is_social) {
        plot_data <-
          data |>
          parse_gkg_mentioned_social_embeds() |>
          left_join(data |> select(idGKG:domainSource, urlImage)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('domainSocialMediaImageEmbed')
      }

      is_themes <-
        gkg_parse == 'themes'

      if (is_themes) {
        plot_data <-
          data |>
          parse_gkg_mentioned_themes(return_wide = FALSE) |>
          left_join(get_codes_gkg_themes() |> dplyr::rename(codeTheme = codeGKGTheme)) |>
          left_join(data |> select(idGKG:domainSource, urlImage)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('codeTheme')
      }

    } else {
      main_name <-
        ''
    }

    if (!gkg_parse |> is_null()) {
      parse_title <-
        gkg_parse |> str_to_title()
    } else {
      parse_title <- ''
    }
    dates <-
      data$dateTimeDocument |> as.Date("%Y-%m-%d") |> unique()

    dates <-
      dates[!is.na(dates)][[1]]

    title <-
      list('GDELT GKG Explorer for ',  parse_title, ' ', dates) |>
      reduce(paste0) |> str_trim()

    if (!extra_columns |> is_null()) {
      plot_data <-
        plot_data |>
        left_join(
          data |> dplyr::select(any_of(c('idGKG', extra_columns)))
        ) |>
        suppressMessages()
    }

    plot_data <-
      plot_data |>
      dplyr::mutate(dplyr::across(dplyr::where(is.logical), ~ifelse(is.na(.x), FALSE, .x)))

    if (!trelliscope_parameters$title |> is_null()) {
      title <-
        trelliscope_parameters$title
    }

    if (trelliscope_parameters$rows |> is_null()) {
      rows <- 1
    } else {
      rows <-
        trelliscope_parameters$rows
    }

    if (trelliscope_parameters$columns |> is_null()) {
      columns <- 2
    } else {
      columns <-
        trelliscope_parameters$columns
    }
    no_path <-
      trelliscope_parameters$path |> is_null()

    if (no_path) {
    viz <-
      plot_data |>
      mutate(
        panel = trelliscopejs::img_panel(urlImage),
        urlArticle = trelliscopejs::cog_href(documentSource),
        idItem = 1:n()
      ) |>
      select(idItem, idGKG, everything()) |>
      arrange(idItem) |>
      trelliscopejs::trelliscope(
        name = title,
        nrow = rows,
        ncol = columns,
        state = list(labels = c(
          "domainSource", "idGKG", main_name, 'urlArticle'
        ))
      )
    } else {
      file_path <-
        trelliscope_parameters$path
      viz <-
        plot_data |>
        mutate(
          panel = trelliscopejs::img_panel(urlImage),
          urlArticle = trelliscopejs::cog_href(documentSource),
          idItem = 1:n()
        ) |>
        select(idItem, idGKG, everything()) |>
        arrange(idItem) |>
        trelliscopejs::trelliscope(
          name = title,
          nrow = rows,
          ncol = columns,
          path = file_path,
          state = list(labels = c(
            "domainSource", "idGKG", main_name, 'urlArticle'
          ))
        )
    }
    return(viz)
  }


#' Trelliscope gkg data
#'
#' @param data A \code{tibble} of data to process.
#' @param domains Character vector of domain names to filter.
#' @param random_domains Integer. Number of random domains to sample.
#' @param only_pictures Logical. If \code{TRUE}, returns only picture-containing entries.
#' @param gkg_parse \itemize{
#' \item 'tone',
#' \item 'dates',
#' \item 'events',
#' \item 'gcam',
#' \item 'locations',
#' \item 'names',
#' \item 'numerics',
#' \item 'organizations',
#' \item 'people',
#' \item 'quotes',
#' \item 'social',
#' \item 'themes',
#' \item 'xml
#' }
#'
#' @return A trelliscope visualization of GKG TV data
#' @export
#' @examples
#' \dontrun{
#' visualize_gkg_tv_trelliscope()
#' }
visualize_gkg_tv_trelliscope <-
  function(data,
           stations = c(
             "CNN",
             "Bloomberg",
             "FBC",
             "BBCNews",
             "WTTG"
           ),
           random_stations = 5,
           only_pictures = TRUE,
           gkg_parse = 'people') {
    check_for_trelliscope_js()

    station_search <-
      c()
    has_random_stations <-
      !random_stations |> is_null()
    if (has_random_stations) {
      random_tv <-
        data$nameSource |>
        unique() |>
        sample(random_stations)

      station_search <-
        c(station_search, random_tv)
    }

    if (!stations |> is_null()) {
      station_search <-
        c(station_search, stations) |>
        str_to_upper()
    }

    station_search <-
      station_search |> str_to_upper() |>
      unique() |> paste0(collapse = '|')

    data <-
      data |>
      filter(nameSource |> str_detect(station_search))

    if (!gkg_parse |> is_null()) {
      gkg_options <-
        c(
          'tone',
          'dates',
          'events',
          'gcam',
          'locations',
          'names',
          'numerics',
          'organizations',
          'people',
          'quotes',
          'social',
          'themes',
          'xml'
        )
      gkg_parse <-
        gkg_parse |> str_to_lower()
      if (!gkg_parse %in% gkg_options) {
        stop(list(
          "Parse options can only be:\n",
          paste(gkg_options, collapse = '\n')
        ) |> reduce(paste0))
      }

      is_tone <-
        gkg_parse == 'tone'

      if (is_tone) {
        plot_data <-
          data |>
          parse_gkg_mentioned_article_tone(filter_na = TRUE, return_wide = FALSE) |>
          left_join(data |> select(idGKG:nameTVShow)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('scoreTone')
      }

      is_xml <-
        gkg_parse == 'xml'

      if (is_xml) {
        plot_data <-
          data |>
          filter(!xmlExtras |> is.na()) |>
          parse_xml_extras(return_wide = FALSE) |>
          left_join(data |> select(idGKG:nameTVShow)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('item', 'value')
      }

      is_date <-
        gkg_parse == 'date'

      if (is_date) {
        plot_data <-
          data |>
          parse_gkg_mentioned_dates(filter_na = TRUE, return_wide = FALSE) |>
          left_join(data |> select(idGKG:nameTVShow)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('dateResolution')
      }

      is_events <-
        gkg_parse == 'events'

      if (is_events) {
        plot_data <-
          data |>
          parse_gkg_mentioned_event_counts(return_wide = FALSE, filter_na = TRUE) |>
          left_join(get_codes_gkg_themes()) |>
          left_join(data |> select(idGKG:nameTVShow)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('codeGKGTheme',
            'countEvent',
            'entityEvent',
            'idADM1Code')
      }

      is_gcam <-
        gkg_parse == 'gcam'

      if (is_gcam) {
        plot_data <-
          data |>
          parse_gkg_mentioned_gcams(return_wide = FALSE, filter_na = TRUE) |>
          left_join(get_codes_gcam()) |>
          left_join(data |> select(idGKG:nameTVShow)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('idGCAM', 'dimensionHumanName')
      }

      is_locations <-
        gkg_parse == 'locations'

      if (is_locations) {
        plot_data <-
          data |>
          parse_gkg_mentioned_locations(return_wide = FALSE, filter_na = TRUE) |>
          left_join(get_codes_stability_locations()) |>
          left_join(data |> select(idGKG:nameTVShow)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('idADM1Code', 'location')
      }

      is_names <-
        gkg_parse == 'names'

      if (is_names) {
        plot_data <-
          data |>
          parse_gkg_mentioned_names(return_wide = FALSE, filter_na = TRUE) |>
          left_join(data |> select(idGKG:nameTVShow)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('nameMentionedName')
      }

      is_numerics <-
        gkg_parse == 'numerics'

      if (is_numerics) {
        plot_data <-
          data |>
          parse_gkg_mentioned_numerics(return_wide = FALSE, filter_na = TRUE) |>
          left_join(data |> select(idGKG:nameTVShow)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('amountTerm', 'amountValue')
      }

      is_organizations <-
        gkg_parse == 'organizations'

      if (is_organizations) {
        plot_data <-
          data |>
          parse_gkg_mentioned_organizations(return_wide = FALSE, filter_na = TRUE) |>
          left_join(data |> select(idGKG:nameTVShow)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('nameOrganization')
      }

      is_people <-
        gkg_parse == 'people'

      if (is_people) {
        plot_data <-
          data |>
          parse_gkg_mentioned_people(people_column = 'personsCharLoc', return_wide = FALSE, filter_na = TRUE) |>
          left_join(data |> select(idGKG:nameTVShow)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('namePerson')
      }

      is_quotes <-
        gkg_parse == 'quotes'

      if (is_quotes) {
        plot_data <-
          data |>
          parse_gkg_mentioned_quotes(return_wide = FALSE, filter_na = TRUE) |>
          left_join(data |> select(idGKG:nameTVShow)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('lengthQuote', 'textQuote')
      }

      is_social <-
        gkg_parse == 'social'

      if (is_social) {
        plot_data <-
          data |>
          parse_gkg_mentioned_social_embeds() |>
          left_join(data |> select(idGKG:nameTVShow)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('domainSocialMediaImageEmbed')
      }

      is_themes <-
        gkg_parse == 'themes'

      if (is_themes) {
        plot_data <-
          data |>
          parse_gkg_mentioned_themes(return_wide = FALSE) |>
          left_join(get_codes_gkg_themes() |> dplyr::rename(codeTheme = codeGKGTheme)) |>
          left_join(data |> select(idGKG:nameTVShow)) |>
          suppressMessages() |>
          suppressWarnings() |>
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('codeTheme')
      }

    } else {
      main_name <-
        ''
    }

    if (!gkg_parse |> is_null()) {
      parse_title <-
        gkg_parse |> str_to_title()
    } else {
      parse_title <- ''
    }
    dates <-
      data$dateTimeDocument |> as.Date("%Y-%m-%d") |> unique()

    dates <-
      dates[!is.na(dates)][[1]]

    title <-
      list('GDELT GKG TV Explorer for ',  parse_title, ' ', dates) |>
      reduce(paste0) |> str_trim()

    viz <-
      plot_data |>
      mutate(urlImage = "http://www.novelupdates.com/img/noimagefound.jpg") |>
      mutate(
        panel = trelliscopejs::img_panel(urlImage),
        urlArchiveVideo = trelliscopejs::cog_href(urlArchiveVideo),
        idItem = 1:n()
      ) |>
      select(idItem, idGKG, everything()) |>
      arrange(idItem) |>
      trelliscopejs::trelliscope(
        name = title,
        nrow = 1,
        ncol = 2,
        state = list(
          labels = c(
            'dateTimeDocument',
            'urlArchiveVideo',
            "idTVNetwork",
            "nameTVShow",
            main_name,
            'idTVNetwork'
          )
        )
      )

    return(viz)
  }