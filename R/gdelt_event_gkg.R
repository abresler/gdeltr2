

# utilities ---------------------------------------------------------------
#' Filters a gdelt data frame to specified sources
#'
#' @param data gkg or vgkg data frame
#' @param sources vector of sources
#'
#' @return
#' @export
#'
#' @examples
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
      sources %>%
      stringr::str_to_lower() %>%
      paste0(collapse = '|')
    if (!'documentSource' %in% names(data)) {
      stop("Data must count the documentSource colum")
    }
    data <-
      data %>%
      filter(documentSource %>% str_detect(sources))

    return(data)
  }

.resolve_long_names <-
  function(data) {
    data <-
      data %>%
      mutate(
        numberItem = ifelse(
          item %>% str_detect("idADM"),
          item %>% str_replace_all("idADM1", '') %>% as.character() %>% readr::parse_number(),
          item %>%
            as.character() %>%
            readr::parse_number()
        ),
        numberItem = ifelse(numberItem %>% is.na(), 0 , numberItem),
        item = item %>% str_replace_all("^\\d+|\\d+$", '')
      ) %>%
      distinct() %>%
      suppressWarnings()

    data <-
      data %>%
      tidyr::spread(item, value) %>%
      suppressWarnings()

    data <-
      data %>%
      mutate_at(data %>% select(
        dplyr::matches(
          "^score|^count|^amount|^value|^face|^angle|^latitude|^longitude|^day|^month|^year|^idTypeLocation"
        )
      ) %>% names,
      funs(. %>% as.character() %>% readr::parse_number()))

    return(data)
  }

#' Loads needed packages
#'
#' @param required_packages vector of pckages to lad
#'
#' @return invisible
#' @export
#'
#' @examples
#' load_neeeded_packages(c('magrittr', 'dplyr))
#'
load_needed_packages <- function(required_packages = c('dplyr')) {
  loaded_packages <- gsub("package:", "", search())
  package_to_load <- required_packages[!required_packages %in%
                                         loaded_packages]
  if (length(package_to_load) > 0) {
    lapply(package_to_load, library, character.only = T)
  }
}


# logs --------------------------------------------------------------------



#' Loads gdelt v2 Global Knowledge Graph master log data, updated every 15 minutes
#'
#' @return
#' @export
#' @import dplyr
#' @import stringr
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate with_tz
#' @importFrom readr read_tsv
#' @importFrom magrittr %>%
#' @importFrom tidyr separate
#' @examples
#' get_urls_gkg_15_minute_log()
get_urls_gkg_15_minute_log <- function() {
  url <-
    'http://data.gdeltproject.org/gdeltv2/masterfilelist.txt'

  log_df <-
    url %>%
    readr::read_tsv(col_names = F) %>%
    tidyr::separate(
      col = X1,
      into = c('idFile', 'idHash', 'urlData'),
      sep = '\\ '
    ) %>%
    suppressWarnings() %>%
    suppressMessages()

  log_df <-
    log_df %>%
    dplyr::mutate(dateTimeFile = urlData %>% str_replace_all('http://data.gdeltproject.org/gdeltv2/', '')) %>%
    tidyr::separate(dateTimeFile,
                    into = c('timestamp', 'nameFile', 'typeFile', 'isZip'))

  log_df <-
    log_df %>%
    dplyr::mutate(
      dateTimeData = timestamp %>% as.numeric() %>% lubridate::ymd_hms() %>% lubridate::with_tz(Sys.timezone()),
      dateData = dateTimeData %>% as.Date(),
      typeFile = typeFile %>% str_to_lower(),
      idFile = idFile %>% as.integer()
    ) %>%
    dplyr::mutate_at(.vars = c('idHash', 'nameFile', 'urlData'),
                     .funs = str_trim) %>%
    suppressWarnings()

  return(log_df)

}

#' Gets GDELT Event data, by year from 1979-2005, by year month 2006 - 2013, then by dat
#'
#' @param return_message
#'
#' @return
#' @import stringr
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @importFrom tidyr separate
#' @importFrom lubridate ymd
#' @export
#'
#' @examples
#' get_urls_gdelt_event_log()

get_urls_gdelt_event_log <- function(return_message = T) {
  url <-
    'http://data.gdeltproject.org/events/md5sums'

  urlData <-
    url %>%
    readr::read_tsv(col_names = F) %>%
    tidyr::separate(col = X1,
                    into = c('idHash', 'stemData'),
                    sep = '\\  ') %>%
    dplyr::mutate(
      urlData = 'http://data.gdeltproject.org/events/' %>% paste0(stemData),
      slugDatabaseGDELT = 'EVENTS',
      isZipFile = ifelse(stemData %>% str_detect(".zip"), T, F)
    ) %>%
    suppressWarnings() %>%
    suppressMessages()

  urlData <-
    urlData %>%
    separate(
      col = stemData,
      into = c('periodData', 'nameFile', 'typeFile', 'zip_file'),
      sep = '\\.'
    ) %>%
    dplyr::select(-zip_file) %>%
    dplyr::mutate(
      periodData = ifelse(periodData == 'GDELT', typeFile, periodData),
      isDaysData = ifelse(periodData %>% nchar == 8, T, F)
    ) %>%
    dplyr::select(-c(nameFile, typeFile)) %>%
    suppressWarnings()

  urlData <-
    urlData %>%
    dplyr::filter(isDaysData == F) %>%
    dplyr::mutate(dateData = NA) %>%
    bind_rows(
      urlData %>%
        dplyr::filter(isDaysData == T) %>%
        dplyr::mutate(dateData = periodData %>% lubridate::ymd() %>% as.Date())
    ) %>%
    dplyr::select(idHash,
                  dateData,
                  isZipFile,
                  isDaysData,
                  urlData,
                  everything())

  if (return_message) {
    count.files <-
      urlData %>%
      nrow

    min.date <-
      urlData$dateData %>% min(na.rm = T)

    max.date <-
      urlData$dateData %>% max(na.rm = T)

    "You got " %>%
      paste0(count.files,
             ' GDELT Global Knowledge Graph URLS from ',
             min.date,
             ' to ',
             max.date) %>%
      cat(fill = T)
  }

  return(urlData)
}

#' Gets most recent GKG log URLs
#'
#' @return
#' @import stringr
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @importFrom tidyr separate
#' @export
#'
#' @examples
#' get_urls_gkg_most_recent_log()
get_urls_gkg_most_recent_log <- function() {
  log_df <-
    'http://data.gdeltproject.org/gdeltv2/lastupdate.txt' %>%
    readr::read_tsv(col_names = F) %>%
    tidyr::separate(
      col = X1,
      into = c('idFile', 'idHash', 'urlData'),
      sep = '\\ '
    ) %>%
    suppressWarnings() %>%
    suppressMessages()

  log_df <-
    log_df %>%
    dplyr::mutate(dateTimeFile = urlData %>% str_replace_all('http://data.gdeltproject.org/gdeltv2/', '')) %>%
    tidyr::separate(dateTimeFile,
                    into = c('timestamp', 'nameFile', 'typeFile', 'isZip')) %>%
    dplyr::mutate(
      typeFile = typeFile %>% str_to_lower(),
      isZip = ifelse(isZip %>% str_detect("ZIP|zip"), T, F),
      idFile = idFile %>% as.integer()
    ) %>%
    mutate_at(c('idHash', 'nameFile', 'urlData'),
              funs(str_trim))

  return(log_df)
}


#' Gets Global Knowledge Graph summary files by day since April 2013
#'
#' @param remove_count_files
#' @param return_message
#'
#' @return
#' @export
#' @import stringr
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @importFrom lubridate ymd
#' @importFrom tidyr separate
#' @examples
#' get_urls_gkg_daily_summaries(remove_count_files = T)
get_urls_gkg_daily_summaries <-
  function(remove_count_files = F,
           return_message = T) {
    url <-
      'http://data.gdeltproject.org/gkg/md5sums'

    urlData <-
      url %>%
      readr::read_tsv(col_names = F) %>%
      tidyr::separate(col = X1,
                      into = c('idHash', 'stemData'),
                      sep = '\\  ') %>%
      dplyr::mutate(urlData = 'http://data.gdeltproject.org/gkg/' %>% paste0(stemData),
                    slugDatabaseGDELT = 'gkg') %>%
      separate(
        col = stemData,
        into = c('dateData', 'nameFile', 'typeFile', 'isZipFile'),
        sep = '\\.'
      ) %>%
      dplyr::mutate(
        isZipFile = ifelse(isZipFile == "zip", T, F),
        isCountFile = ifelse(nameFile == 'gkgcounts', T, F),
        dateData = dateData %>% lubridate::ymd() %>% as.Date()
      ) %>%
      dplyr::select(-c(nameFile, typeFile)) %>%
      dplyr::select(idHash,
                    dateData,
                    isZipFile,
                    isCountFile,
                    urlData,
                    everything()) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (remove_count_files) {
      urlData <-
        urlData %>%
        dplyr::filter(isCountFile == F)
    }

    if (return_message) {
      count.files <-
        urlData %>%
        nrow

      min.date <-
        urlData$dateData %>% min(na.rm = T)

      max.date <-
        urlData$dateData %>% max(na.rm = T)

      "You got " %>%
        paste0(count.files,
               ' GDELT Global Knowledge Graph URLS from ',
               min.date,
               ' to ',
               max.date) %>%
        cat(fill = T)
    }

    return(urlData)
  }

#' Retrives most recent GDELT Global Content Analysis Measures (GCAM) code book
#'
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @examples
#' get_codes_gcam()
get_codes_gcam <- function() {
  url <-
    'http://data.gdeltproject.org/documentation/GCAM-MASTER-CODEBOOK.TXT'
  gcam_data <-
    url %>%
    read_tsv() %>%
    suppressWarnings() %>%
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
}

#' Retrives GDELT CAMEO religion code book
#'
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @examples
#' get_codes_cameo_religion()
get_codes_cameo_religion <- function() {
  url <-
    'http://gdeltproject.org/data/lookups/CAMEO.religion.txt'
  code_df <-
    url %>%
    readr::read_tsv() %>%
    suppressWarnings() %>%
    suppressMessages()

  names(code_df) <-
    c('codeCAMEOReligion', 'nameCAMEOReligion')

  return(code_df)
}

#' Retrieves GDELT CAMEO country code book
#'
#'
#' @return
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @export
#'
#' @examples
#' get_codes_cameo_country()
get_codes_cameo_country <- function() {
  url <-
    'http://gdeltproject.org/data/lookups/CAMEO.country.txt'
  code_df <-
    url %>%
    read_tsv() %>%
    suppressWarnings() %>%
    suppressMessages()

  names(code_df) <-
    c('codeISO', 'nameCountry')

  code_df
}

#' Retrieves GDELT CAMEO type code book
#'
#' @return
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @export
#'
#' @examples
#' get_codes_cameo_type()
get_codes_cameo_type <- function() {
  url <- 'http://gdeltproject.org/data/lookups/CAMEO.type.txt'
  code_df <-
    url %>%
    read_tsv() %>%
    suppressWarnings() %>%
    suppressMessages()

  names(code_df) <-
    c('codeCAMEOType', 'nameCAMEOType')

  code_df
}

#' Retrieves CAMEO CAMEO event code book
#'
#'
#' @return
#' @export
#' @import dplyr readr stringr purrr tidyr
#' @examples
#' get_codes_cameo_events()

get_codes_cameo_events <- function() {
  url <-
    'http://gdeltproject.org/data/lookups/CAMEO.eventcodes.txt'
  code_df <-
    url %>%
    read_tsv() %>%
    suppressWarnings() %>%
    suppressMessages()

  names(code_df) <-
    c('idCAMEOEvent', 'descriptionCAMEOEvent')

  code_df <-
    code_df %>%
    dplyr::mutate(
      isParentCode = ifelse(idCAMEOEvent %>% nchar() == 2, T, F),
      idParentCode = idCAMEOEvent %>% substr(1, 2)
    ) %>%
    dplyr::select(idParentCode, everything()) %>%
    mutate_at(c("idParentCode", "idCAMEOEvent"),
              funs(as.numeric))

  return(code_df)
}

#' Retrieves GDELT CAMEO known group code book
#'
#' @return
#' @importFrom readr read_tsv
#' @importFrom magrittr %>%
#' @export
#' @examples
#' get_codes_cameo_known_groups()
get_codes_cameo_known_groups <- function() {
  url <-
    'http://gdeltproject.org/data/lookups/CAMEO.knowngroup.txt'
  code_df <-
    url %>%
    read_tsv() %>%
    suppressWarnings() %>%
    suppressMessages()

  names(code_df) <-
    c('codeCAMEOGroup', 'nameCAMEOGroup')

  return(code_df)
}

#' Retrieves GDELT CAMEO ethnic code book
#'
#' @return
#' @export
#' @import dplyr readr stringr purrr tidyr
#' @examples
#' get_codes_cameo_ethnic()
get_codes_cameo_ethnic <- function() {
  url <-
    'http://gdeltproject.org/data/lookups/CAMEO.ethnic.txt'
  code_df <-
    url %>%
    read_tsv() %>%
    suppressWarnings() %>%
    suppressMessages() %>%
    purrr::set_names(c('codeCAMEOEthnicity', 'nameCAMEOEthnicity')) %>%
    mutate(codeCAMEOEthnicity = codeCAMEOEthnicity %>% str_to_upper())

  return(code_df)
}

#' Retrieves GKG theme code book
#'
#' @return
#' @export
#' @import dplyr readr stringr purrr tidyr
#' @examples
#' get_codes_gkg_themes()

get_codes_gkg_themes <- function() {
  url <-
    'http://data.gdeltproject.org/documentation/GKG-MASTER-THEMELIST.TXT'

  code_df <-
    url %>%
    read_tsv(col_names = F) %>%
    suppressWarnings() %>%
    suppressMessages()

  names(code_df) <-
    c('codeGKGTheme')
  code_df <-
    code_df %>%
    mutate(
      code2 = codeGKGTheme %>% str_to_lower(),
      isWBCode = ifelse(code2 %>% str_detect("wb_"), T, F),
      isEconomicEvent = ifelse(code2 %>% str_detect("econ_"), T, F),
      isSocialEvent = ifelse(code2 %>% str_detect("soc"), T, F),
      isTaxEvent = ifelse(code2 %>% str_detect("tax_"), T, F),
      isSocialEvent = ifelse(code2 %>% str_detect("soc_"), T, F),
      isMilitaryEvent = ifelse(code2 %>% str_detect("military|mil_"), T, F),
      isGovernmentEvent = ifelse(code2 %>% str_detect("gov_|government"), T, F),
      isMedicalEvent = ifelse(code2 %>% str_detect("med_|medical"), T, F),
      isAgressionAct = ifelse(code2 %>% str_detect("act_"), T, F),
      isMediaEvent = ifelse(code2 %>% str_detect("media_|_media"), T, F),
      isEmergencyEvent = ifelse(code2 %>% str_detect("emerg_"), T, F),
      isMovement = ifelse(code2 %>% str_detect("movement_"), T, F),
      isCriminalEvent = ifelse(code2 %>% str_detect("crime|crm_"), T, F)
    ) %>%
    dplyr::select(-code2)

  wb_codes <-
    code_df %>%
    dplyr::filter(isWBCode)

  wb_codes <-
    wb_codes %>%
    mutate(codeGKGTheme = codeGKGTheme %>% sub('\\_', '\\.', .)) %>%
    separate(
      codeGKGTheme,
      into = c('idDictionary', 'nameWBCode'),
      remove = F,
      sep = '\\.'
    ) %>%
    mutate(nameWBCode = nameWBCode %>% sub('\\_', '\\.', .)) %>%
    separate(
      nameWBCode,
      into = c('idWBCode', 'nameWBCode'),
      remove = T,
      sep = '\\.'
    ) %>%
    mutate(
      idWBCode = idWBCode %>% as.numeric,
      nameWBCode = nameWBCode %>% str_replace_all('\\_', ' ') %>% str_to_lower
    ) %>%
    dplyr::select(-idDictionary)

  non_wb <-
    code_df %>%
    dplyr::filter(!isWBCode)

  code_df <-
    non_wb %>%
    bind_rows(wb_codes)

  code_df <-
    code_df %>%
    mutate(codeGKGTheme = codeGKGTheme %>% str_replace_all('WB.', '\\WB_')) %>%
    dplyr::select(isWBCode, codeGKGTheme, idWBCode, nameWBCode, everything())

  code_df

}

#' Retrieves GDELT event summary by period
#'
#' @param period can be \code{c("yearly", "daily", "monthly")}
#' @param by_country is data by country
#' can be \code{c(TRUE, FALSE)}
#' @param return_message returns message
#' @importFrom readr read_csv
#' @return
#' @export
#'
#' @examples
#' get_data_gdelt_period_event_totals(period = 'monthly', by_country = T)

get_data_gdelt_period_event_totals <-
  function(period = 'yearly',
           by_country = T,
           return_message = T) {
    periods <-
      c('daily', 'monthly', 'yearly')
    if (!period %in% periods) {
      "Sorry periods can only be:\n" %>%
        stop(paste0(paste0(periods, collapse = '\n')))
    }

    if (by_country) {
      period_slug <-
        period %>%
        paste0('_country.csv')
    } else {
      period_slug <-
        period %>%
        paste0('.csv')
    }
    base <-
      'http://data.gdeltproject.org/normfiles/'

    url_data <-
      base %>%
      paste0(period_slug)

    period_data <-
      url_data %>%
      read_csv(col_names = F) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (by_country) {
      names(period_data) <-
        c('idDate', 'idCountry', 'countEvents')
    } else {
      names(period_data) <-
        c('idDate', 'countEvents')
    }

    period_data <-
      period_data %>%
      mutate(periodData = period, isByCountry = by_country)

    if (period == 'daily') {
      period_data <-
        period_data %>%
        mutate(dateData = idDate %>% lubridat::ymd() %>% as.Date()) %>%
        dplyr::select(periodData, isByCountry, dateData, everything())
    }

    if (period == 'monthly') {
      period_data <-
        period_data %>%
        mutate(yearMonth = idDate) %>%
        dplyr::select(periodData, isByCountry, yearMonth, everything()) %>%
        mutate(
          yearData = yearMonth %>% substr(1, 4) %>% as.numeric(),
          monthData = yearMonth %>% substr(5, 6) %>% as.numeric()
        ) %>%
        select(periodData, isByCountry, yearData, monthData, everything())
    }

    if (period == 'yearly') {
      period_data <-
        period_data %>%
        mutate(yearData = idDate) %>%
        dplyr::select(periodData, isByCountry, everything())
    }

    if (return_message)  {
      from_date <-
        period_data$idDate %>% min()

      to_date <-
        period_data$idDate %>% max()

      total_events <-
        period_data$countEvents %>% sum() / 1000000
      events_slug <-
        total_events %>% paste0(" million GDELT events from ")
      "There have been " %>%
        paste0(events_slug, from_date,
               ' to ', to_date) %>%
        message
    }
    return(period_data)
  }


#' Retrieves GDELT event database schema
#'
#' @return
#' @importFrom dplyr tibble
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
#' @return
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
#' @return
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
#' @return
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
#' @param url
#' @param file_directory
#' @param folder_name
#' @param remove_existing_folder
#' @param remove_files
#' @param empty_trash
#' @param return_message
#'
#' @return
#' @export
#' @import readr purrr curl dplyr lubridate tidyr stringr
#' @importFrom urltools domain
#' @examples
get_gdelt_url_data <-
  function(url = "http://data.gdeltproject.org/gdeltv2/20160531000000.gkg.csv.zip",
           file_directory = NULL,
           folder_name = 'gdelt_data',
           remove_existing_folder = T,
           remove_files = T,
           empty_trash = T,
           return_message = T) {

    use_tmp_file <-
      file_directory %>%
      purrr::is_null()

    if (use_tmp_file) {
      tmp <-
        tempfile()

      url %>%
        curl::curl_download(url = ., tmp)

      con <-
        unzip(tmp)

      gdelt_cols <-
        con %>%
        read_tsv(col_names = F,
                 n_max = 1) %>% ncol() %>%
        suppressMessages() %>%
        suppressWarnings()

      if (gdelt_cols == 16) {
        gdelt_data <-
          con %>%
          read_tsv(col_names = F) %>%
          suppressWarnings() %>%
          suppressMessages()

        names(gdelt_data) <-
          get_schema_gkg_mentions() %>% .$nameActual

        gdelt_data <-
          gdelt_data %>%
          mutate(
            dateTimeEvent = dateEvent %>% ymd_hms() %>% lubridate::with_tz(Sys.timezone()),
            dateEvent = dateTimeEvent %>% as.Date(),
            dateTimeMention = dateMention %>% ymd_hms() %>% lubridate::with_tz(Sys.timezone()),
            dateMention = dateTimeMention %>% as.Date()
          ) %>%
          dplyr::select(idGlobalEvent,
                        dateTimeEvent,
                        dateTimeMention,
                        everything()) %>%
          dplyr::left_join(tibble(
            idMentionType = 1:6,
            typeMention = c('web', 'citation', 'core', 'dtic', 'jstor', 'nontext')
          )) %>%
          dplyr::select(idGlobalEvent:idMentionType,
                        typeMention,
                        everything()) %>%
          suppressMessages()

        gdelt_data <-
          gdelt_data %>%
          mutate_at(gdelt_data %>% dplyr::select(dplyr::matches("is")) %>% names(),
                    funs(. %>% as.logical()))

      }

      if (gdelt_cols == 15) {
        gdelt_data <-
          con %>%
          read_tsv(col_names = T) %>%
          suppressWarnings() %>%
          suppressMessages()

        names(gdelt_data) <-
          get_schema_gkg_counts() %>% .$nameActual

        gdelt_data <-
          gdelt_data %>%
          dplyr::left_join(tibble(
            idTypeLocation = 1:5,
            typeLocation = c(
              'country',
              'usState',
              'usCity',
              'worldCity',
              'worldState'
            )
          )) %>%
          suppressMessages() %>%
          dplyr::mutate(
            idRecord = 1:n(),
            idGKG = dateEvent %>% paste0('.', idRecord),
            urlSources = urlSources %>% str_replace_all("<UDIV>", ';')
          ) %>%
          dplyr::mutate(dateEvent = dateEvent %>% ymd()) %>%
          dplyr::select(dateEvent:idTypeLocation, typeLocation, everything()) %>%
          dplyr::select(idRecord, idGKG, everything())
      }

      if (gdelt_cols == 61) {
        gdelt_data <-
          con %>%
          readr::read_tsv(col_names = F) %>%
          suppressWarnings() %>%
          suppressMessages()

        names(gdelt_data) <-
          get_schema_gdelt_events() %>% .$nameActual

        gdelt_data <-
          gdelt_data %>%
          dplyr::rename(dateTimeDocument = dateTimeDataAdded) %>%
          dplyr::mutate(
            dateEvent = dateEvent %>% lubridate::ymd,
            dateTimeDocument = dateTimeDocument %>% ymd_hms() %>% lubridate::with_tz(Sys.timezone()),
            nameSource = urlSource %>% urltools::domain() %>% str_replace_all("www.", '')
          )

        gdelt_data <-
          gdelt_data %>%
          mutate_at(gdelt_data %>% dplyr::select(dplyr::matches("is")) %>% names(),
                    funs(. %>% as.logical()))
        gdelt_data <-
          gdelt_data %>%
          dplyr::left_join(tibble(
            classQuad =  1:4,
            nameQuad =  c(
              'Verbal Cooperation',
              'Material Cooperation',
              'Verbal Conflict',
              'Material Conflict'
            )
          )) %>%
          suppressMessages()
      }

      if (gdelt_cols == 57) {
        gdelt_data <-
          con %>%
          readr::read_tsv(col_names = F) %>%
          suppressWarnings() %>%
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
          gdelt_data %>%
          dplyr::rename(dateDocument = dateAdded) %>%
          dplyr::mutate(
            dateEvent = dateEvent %>% lubridate::ymd,
            dateDocument = dateDocument %>% lubridate::ymd
          ) %>%
          suppressWarnings()

        gdelt_data <-
          gdelt_data %>%
          mutate_at(gdelt_data %>% dplyr::select(dplyr::matches("is")) %>% names(),
                    funs(. %>% as.logical()))

        gdelt_data <-
          gdelt_data %>%
          dplyr::left_join(tibble(
            classQuad =  1:4,
            nameQuad =  c(
              'Verbal Cooperation',
              'Material Cooperation',
              'Verbal Conflict',
              'Material Conflict'
            )
          )) %>%
          suppressMessages()
      }

      if (gdelt_cols == 58) {
        gdelt_data <-
          con %>%
          readr::read_tsv(col_names = F) %>%
          suppressWarnings() %>%
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
          gdelt_data %>%
          dplyr::rename(dateTimeDocument = dateAdded) %>%
          dplyr::mutate(
            dateEvent = dateEvent %>% lubridate::ymd(),
            nameSource = urlSource %>% urltools::domain() %>% str_replace_all("www.", '')
          ) %>%
          suppressWarnings()

        gdelt_data <-
          gdelt_data %>%
          dplyr::mutate_at(gdelt_data %>% dplyr::select(dplyr::matches("is")) %>% names(),
                              funs(. %>% as.logical()))

        gdelt_data <-
          gdelt_data %>%
          dplyr::left_join(tibble(
            classQuad =  1:4,
            nameQuad =  c(
              'Verbal Cooperation',
              'Material Cooperation',
              'Verbal Conflict',
              'Material Conflict.'
            )
          )) %>%
          suppressMessages()

      }

      if (gdelt_cols == 11) {
        gdelt_data <-
          con %>%
          readr::read_tsv(col_names = T) %>%
          suppressWarnings() %>%
          suppressMessages()

        schema_df <-
          get_schema_gkg_general()

        names(gdelt_data) <-
          schema_df$nameActual[names(gdelt_data) %>% match(schema_df$nameGDELT)]

        names(gdelt_data)[1] <-
          c('date')

        gdelt_data <-
          gdelt_data %>%
          dplyr::mutate(
            idRecord = 1:n(),
            idGKG = date %>% paste0('.', idRecord),
            date = date %>% lubridate::ymd(),
            urlSources = urlSources %>% str_replace_all("<UDIV>", ';')
          ) %>%
          dplyr::select(idRecord, idGKG, everything())

      }

      if (gdelt_cols == 27) {
        gdelt_data <-
          con %>%
          readr::read_tsv(col_names = F) %>%
          suppressWarnings() %>%
          suppressMessages()

        schema_df <-
          get_schema_gkg_general()

        names(gdelt_data) <-
          schema_df$nameActual[1:27]

        gdelt_data <-
          gdelt_data %>%
          dplyr::mutate(
            idSourceCollectionIdentifier = idSourceCollectionIdentifier %>% as.numeric(),
            isDocumentURL = ifelse(documentSource %>% str_detect('http'), T, F)
          ) %>%
          dplyr::select(idGKG:idSourceCollectionIdentifier,
                        isDocumentURL,
                        everything()) %>%
          dplyr::rename(dateTimeDocument = dateDocument) %>%
          dplyr::mutate(
            dateTimeDocument = dateTimeDocument %>% lubridate::ymd_hms() %>% lubridate::with_tz(Sys.timezone())
          ) %>%
          separate(
            idGKG,
            into = c('dateTime', 'idDateTimeArticle'),
            sep = '\\-',
            remove = F
          ) %>%
          dplyr::select(-c(dateTime, translationInfo)) %>%
          dplyr::mutate(
            idDateTimeArticle = idDateTimeArticle %>% as.numeric,
            domainSource = if_else(
              isDocumentURL == T,
              documentSource %>% urltools::domain(),
              nameSource
            )
          ) %>%
          dplyr::select(idGKG:documentSource, domainSource, everything()) %>%
          suppressMessages() %>%
          suppressWarnings()

      }
      con %>%
        unlink()
    }  else {
      only_folder <-
        !folder_name %>% purrr::is_null() &
        file_directory %>% purrr::is_null()
      if (only_folder) {
        file_directory <-
          getwd()
      }
      file_directory <-
        file_directory %>%
        paste0('/', folder_name)

      file <-
        url %>% basename()

      temp.dir <-
        file_directory

      file_path <-
        temp.dir %>% str_split('/') %>% flatten_chr() %>% .[seq_along(.)] %>% paste0(collapse = '/')
      if (remove_existing_folder) {
        if (dir.exists(paths = file_path)) {
          "rm -R " %>%
            paste0(temp.dir) %>%
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
        temp.dir %>%
        paste0('/', file)

      url %>%
        curl_download(url = ., destfile = file)

      file %>%
        unzip(exdir = paste0(temp.dir, '/'))

      dir_files <-
        temp.dir %>%
        list.files()

      file_name <-
        dir_files %>%
        str_detect('CSV|csv|TXT|txt|XLS|XLSX|xlsx|xls') %>%
        dir_files[.]

      csv_file_loc <-
        file_name[!file_name %>% str_detect("zip")] %>%
        paste0(file_directory, '/', .)

      gdelt_cols <-
        csv_file_loc %>%
        read_tsv(col_names = F,
                 n_max = 1) %>% ncol %>% suppressWarnings() %>%
        as.character() %>%
        readr::parse_number() %>%
        suppressMessages()

      if (gdelt_cols == 16) {
        gdelt_data <-
          csv_file_loc %>%
          read_tsv(col_names = F) %>%
          suppressWarnings() %>%
          suppressMessages()

        names(gdelt_data) <-
          get_schema_gkg_mentions() %>% .$nameActual

        gdelt_data <-
          gdelt_data %>%
          mutate(
            dateTimeEvent = dateEvent %>% ymd_hms %>% with_tz(Sys.timezone()),
            dateEvent = dateTimeEvent %>% as.Date(),
            dateTimeMention = dateMention %>% ymd_hms %>% with_tz(Sys.timezone()),
            dateMention = dateTimeMention %>% as.Date()
          ) %>%
          dplyr::select(idGlobalEvent,
                        dateTimeEvent,
                        dateTimeMention,
                        everything()) %>%
          dplyr::left_join(tibble(
            idMentionType = 1:6,
            mention_type = c('web', 'citation', 'core', 'dtic', 'jstor', 'nontext')
          )) %>%
          dplyr::select(idGlobalEvent:idMentionType,
                        mention_type,
                        everything()) %>%
          suppressMessages()

        gdelt_data <-
          gdelt_data %>%
          mutate_at(.vars = gdelt_data %>% dplyr::select(dplyr::matches("is")) %>% names(),
                    .funs = as.logical)
      }

      if (gdelt_cols == 15) {
        gdelt_data <-
          csv_file_loc %>%
          read_tsv(col_names = T) %>%
          suppressWarnings() %>%
          suppressMessages()

        names(gdelt_data) <-
          get_schema_gkg_counts() %>% .$nameActual

        gdelt_data <-
          gdelt_data %>%
          dplyr::left_join(tibble(
            idTypeLocation = 1:5,
            typeLocation = c(
              'country',
              'usState',
              'usCity',
              'worldCity',
              'worldState'
            )
          )) %>%
          suppressMessages() %>%
          dplyr::mutate(
            idRecord = 1:n(),
            idGKG = dateEvent %>% paste0('.', idRecord),
            urlSources = urlSources %>% str_replace_all("<UDIV>", ';')
          ) %>%
          dplyr::mutate(dateEvent = dateEvent %>% ymd()) %>%
          dplyr::select(dateEvent:idTypeLocation, typeLocation, everything()) %>%
          dplyr::select(idRecord, idGKG, everything())
      }

      if (gdelt_cols == 61) {
        gdelt_data <-
          csv_file_loc %>%
          readr::read_tsv(col_names = F) %>%
          suppressWarnings() %>%
          suppressMessages()

        names(gdelt_data) <-
          get_schema_gdelt_events() %>% .$nameActual

        gdelt_data <-
          gdelt_data %>%
          dplyr::rename(dateTimeDocument = dateTimeDataAdded) %>%
          dplyr::mutate(
            dateEvent = dateEvent %>% lubridate::ymd,
            dateTimeDocument = dateTimeDocument %>% ymd_hms() %>% with_tz(Sys.timezone()),
            nameSource = urlSource %>% domain() %>% str_replace_all("www.", '')
          )

        gdelt_data <-
          gdelt_data %>%
          dplyr::mutate_at(gdelt_data %>% dplyr::select(dplyr::matches("is")) %>% names(),
                           funs(. %>% as.logical()))

        gdelt_data <-
          gdelt_data %>%
          dplyr::left_join(tibble(
            classQuad =  1:4,
            nameQuad =  c(
              'Verbal Cooperation',
              'Material Cooperation',
              'Verbal Conflict',
              'Material Conflict'
            )
          )) %>%
          suppressMessages()
      }

      if (gdelt_cols == 57) {
        gdelt_data <-
          csv_file_loc %>%
          readr::read_tsv(col_names = F) %>%
          suppressWarnings() %>%
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
          gdelt_data %>%
          dplyr::rename(dateDocument = dateAdded) %>%
          dplyr::mutate(
            dateEvent = dateEvent %>% lubridate::ymd,
            dateDocument = dateDocument %>% lubridate::ymd
          ) %>%
          suppressWarnings()

        gdelt_data <-
          gdelt_data %>%
          dplyr::mutate_at(gdelt_data %>% dplyr::select(dplyr::matches("is")) %>% names(),
                           funs(. %>% as.logical()))

        gdelt_data <-
          gdelt_data %>%
          dplyr::left_join(tibble(
            classQuad =  1:4,
            nameQuad =  c(
              'Verbal Cooperation',
              'Material Cooperation',
              'Verbal Conflict',
              'Material Conflict'
            )
          )) %>%
          suppressMessages()
      }

      if (gdelt_cols == 58) {
        load_needed_packages(c('urltools'))
        gdelt_data <-
          csv_file_loc %>%
          readr::read_tsv(col_names = F) %>%
          suppressWarnings() %>%
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
          gdelt_data %>%
          dplyr::rename(dateTimeDocument = dateAdded) %>%
          dplyr::mutate(
            dateEvent = dateEvent %>% lubridate::ymd,
            dateTimeDocument %>% lubridate::ymd_hms() %>% with_tz(Sys.timezone()),
            dateDocument = dateTimeDocument %>% as.Date(),
            nameSource = urlSource %>% domain() %>% str_replace_all("www.", '')
          ) %>%
          suppressWarnings()

        gdelt_data <-
          gdelt_data %>%
          dplyr::mutate_at(gdelt_data %>% dplyr::select(dplyr::matches("is")) %>% names(),
                           funs(. %>% as.logical()))

        gdelt_data <-
          gdelt_data %>%
          dplyr::left_join(tibble(
            classQuad =  1:4,
            nameQuad =  c(
              'Verbal Cooperation',
              'Material Cooperation',
              'Verbal Conflict',
              'Material Conflict.'
            )
          )) %>%
          suppressMessages()

      }

      if (gdelt_cols == 11) {
        gdelt_data <-
          csv_file_loc %>%
          readr::read_tsv(col_names = T) %>%
          suppressWarnings() %>%
          suppressMessages()

        schema_df <-
          get_schema_gkg_general()

        names(gdelt_data) <-
          schema_df$nameActual[names(gdelt_data) %>% match(schema_df$nameGDELT)]

        names(gdelt_data)[1] <-
          c('date')

        gdelt_data <-
          gdelt_data %>%
          dplyr::mutate(
            idRecord = 1:n(),
            idGKG = date %>% paste0('.', idRecord),
            date = date %>% lubridate::ymd(),
            urlSources = urlSources %>% str_replace_all("<UDIV>", ';')
          ) %>%
          dplyr::select(idRecord, idGKG, everything())

      }

      if (gdelt_cols == 27) {
        gdelt_data <-
          csv_file_loc %>%
          readr::read_tsv(col_names = F) %>%
          suppressWarnings() %>%
          suppressMessages()

        schema_df <-
          get_schema_gkg_general()

        names(gdelt_data) <-
          schema_df$nameActual[1:27]

        gdelt_data <-
          gdelt_data %>%
          dplyr::mutate(
            idSourceCollectionIdentifier = idSourceCollectionIdentifier %>% as.numeric(),
            isDocumentURL = ifelse(documentSource %>% str_detect('http'), T, F)
          ) %>%
          dplyr::select(idGKG:idSourceCollectionIdentifier,
                        isDocumentURL,
                        everything()) %>%
          dplyr::rename(dateTimeDocument = dateDocument) %>%
          dplyr::mutate(dateTimeDocument = dateTimeDocument %>% lubridate::ymd_hms() %>% with_tz(Sys.timezone())) %>%
          separate(
            idGKG,
            into = c('dateTime', 'idDateTimeArticle'),
            sep = '\\-',
            remove = F
          ) %>%
          dplyr::select(-c(dateTime, translationInfo)) %>%
          dplyr::mutate(
            idDateTimeArticle = idDateTimeArticle %>% as.numeric,
            domainSource = if_else(
              isDocumentURL == T,
              documentSource %>% urltools::domain,
              nameSource
            )
          ) %>%
          dplyr::select(idGKG:documentSource, domainSource, everything()) %>%
          suppressMessages() %>%
          suppressWarnings()

      }

      if (remove_files) {
        "rm -R " %>%
          paste0(temp.dir) %>%
          system()
        if (empty_trash) {
          system('rm -rf ~/.Trash/*')
        }
      }


    }
    if ('idADM1CodeActor1' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data %>%
        mutate(idADM1CodeActor1 = idADM1CodeActor1 %>% as.character())

    }

    if ('idADM1CodeActor2' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data %>%
        mutate(idADM1CodeActor2 = idADM1CodeActor2 %>% as.character())

    }

    if ('idADM2CodeActor1' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data %>%
        mutate(idADM2CodeActor1 = idADM2CodeActor1 %>% as.character())

    }

    if ('idADM2CodeActor2' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data %>%
        mutate(idADM2CodeActor2 = idADM2CodeActor2 %>% as.character())

    }

    if ('idADM1CodeAction' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data %>%
        mutate(idADM1CodeAction = idADM1CodeAction %>% as.character())

    }

    if ('idADM2CodeAction' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data %>%
        mutate(idADM2CodeAction = idADM2CodeAction %>% as.character())

    }

    if ('idCAMEOEvent' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data %>%
        mutate_at(
          .vars = gdelt_data %>% dplyr::select(dplyr::matches('idCAMEOEvent')) %>% names(),
          funs(. %>% as.character() %>% as.numeric())
        )

    }

    if ('idFeatureActor' %in% names(gdelt_data)) {
      gdelt_data <-
        gdelt_data %>%
        mutate_at(.vars = gdelt_data %>% dplyr::select(dplyr::matches('idFeatureActor')) %>% names(),
                  .funs = as.character)

    }

    if (return_message) {
      "Downloaded, parsed and imported " %>%
        paste0(url) %>%
        cat(fill = T)
    }
    return(gdelt_data)
  }

#' Gets clean count data
#'
#' @param all_counts
#' @param count_col
#' @param return_wide
#'
#' @return
#'
#' @examples
get_clean_count_data <-
  function(all_counts,
           extra_key = NA,
           count_col = 'idArticle.tone',
           return_wide = F) {
    if (!extra_key %>% is.na()) {
      clean_data <-
        all_counts %>%
        dplyr::rename_(count = count_col,
                       ek = extra_key) %>%
        group_by(idGKG) %>%
        mutate(count = 1:n()) %>%
        ungroup %>%
        mutate(count = count - 1) %>%
        gather(item, value, -c(idGKG, count, ek), na.rm = T) %>%
        mutate(item = ifelse(count == 0, item, item %>% paste0(count)),
               value = value %>% str_trim) %>%
        separate(
          idGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = F
        ) %>%
        mutate(dateTime = dateTime %>% as.numeric) %>%
        arrange(dateTime, count) %>%
        dplyr::select(-c(dateTime, GKG))
    } else {
      clean_data <-
        all_counts %>%
        dplyr::rename_(count = count_col) %>%
        group_by(idGKG) %>%
        mutate(count = 1:n()) %>%
        ungroup %>%
        mutate(count = count - 1)
      if ('charLoc' %in% names(clean_data)) {
        clean_data <-
          clean_data %>%
          gather(item, value, -c(idGKG, count, charLoc), na.rm = T) %>%
          mutate(
            item = ifelse(count == 0, item, item %>% paste0(count)),
            value = value %>% str_trim
          ) %>%
          distinct() %>%
          separate(
            idGKG,
            into = c('GKG', 'dateTime'),
            sep = '\\-',
            remove = F
          ) %>%
          mutate(dateTime = dateTime %>% as.numeric) %>%
          arrange(dateTime, count) %>%
          dplyr::select(-c(dateTime, GKG))
      } else {
        clean_data <-
          clean_data %>%
          gather(item, value, -c(idGKG, count), na.rm = T) %>%
          mutate(
            item = ifelse(count == 0, item, item %>% paste0(count)),
            value = value %>% str_trim
          ) %>%
          distinct() %>%
          separate(
            idGKG,
            into = c('GKG', 'dateTime'),
            sep = '\\-',
            remove = F
          ) %>%
          mutate(dateTime = dateTime %>% as.numeric) %>%
          arrange(dateTime, count) %>%
          dplyr::select(-c(dateTime, GKG))
      }
    }

    if (return_wide) {
      if (!extra_key %>% is.na()) {
        names_order <-
          c('idGKG', 'ek',  clean_data$item %>% unique)
      } else {
        names_order <-
          c('idGKG',  clean_data$item %>% unique)
      }

      clean_data <-
        clean_data %>%
        dplyr::select(-count) %>%
        spread(item, value)

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
        clean_data %>%
        dplyr::select(dplyr::matches(keywords %>% paste0(collapse = '|'))) %>%
        dplyr::select(-dplyr::matches('namePerson|idCountry')) %>%
        names %>% length

      if (length_nums > 0) {
        numeric_vars <-
          clean_data %>%
          dplyr::select(dplyr::matches(keywords %>% paste0(collapse = '|'))) %>%
          dplyr::select(-dplyr::matches('namePerson|idCountry')) %>%
          names

        clean_data <-
          clean_data %>%
          dplyr::mutate_at(numeric_vars,
                           funs(. %>% as.numeric()))

      }

      clean_data <-
        clean_data %>%
        dplyr::select_(.dots = names_order) %>%
        separate(
          idGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = F
        ) %>%
        mutate(dateTime = dateTime %>% as.numeric) %>%
        arrange(dateTime) %>%
        dplyr::select(-c(GKG, dateTime))

      if (!extra_key %>% is.na()) {
        names(clean_data)[2] <-
          extra_key
      }
    } else {
      clean_data <-
        clean_data %>%
        dplyr::select(-count) %>%
        separate(
          idGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = F
        ) %>%
        mutate(dateTime = dateTime %>% as.numeric) %>%
        arrange(dateTime) %>%
        dplyr::select(-c(GKG, dateTime))

    }

    return(clean_data)
  }

#' Returns long or wide mentioned numerics from a GKG data frame
#'
#' @param gdelt_data
#' @param filter_na
#' @param include_char_locg
#' @param return_wide
#' @importFrom tidyr gather
#' @importFrom tidyr unite
#' @importFrom tidyr spread
#' @importFrom tidyr separate
#' @importFrom purrr map
#' @importFrom purrr compact
#' @import stringr purrr
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_numerics <- function(gdelt_data,
                                         filter_na = T,
                                         include_char_loc = T,
                                         return_wide = T) {
  parse_mentioned_numerics <-
    function(field = "170,Scotland Road,1600;170,Scotland Road,2475;",
             return_wide = F) {
      options(scipen = 99999)
      if (field %>% is.na) {
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
          field %>%
          str_split('\\;') %>%
          flatten_chr() %>%
          .[!. %in% '']

        fields_df <-
          tibble(field = fields) %>%
          dplyr::mutate(idArticleNumericItem = 1:n()) %>%
          separate(
            field,
            into = c('amountValue', 'amountTerm', 'charLoc'),
            sep = '\\,'
          ) %>%
          dplyr::mutate(amountTerm = amountTerm %>% str_trim) %>%
          suppressMessages() %>%
          suppressWarnings()

        if (return_wide) {
          fields_df <-
            fields_df %>%
            gather(item, value, -idArticleNumericItem) %>%
            arrange(idArticleNumericItem) %>%
            unite(item, item, idArticleNumericItem, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            dplyr::mutate_at(
              field_data %>% dplyr::select(dplyr::matches('amountValue|charLoc')) %>% names(),
              funs(. %>% as.character() %>% readr::parse_number())
            )

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(idArticleNumericItem,
                          amountValue,
                          amountTerm,
                          charLoc) %>%
            dplyr::mutate(charLoc = charLoc %>% as.numeric,
                          amountValue = amountValue %>% as.numeric)
        }
      }

      return(field_data)
    }

  if (!'mentionedNumericsCounts' %in% names(gdelt_data)) {
    stop("Sorry missing numeric count column")
  }
  counts_data <-
    gdelt_data %>%
    dplyr::select(idGKG, mentionedNumericsCounts)

  if (filter_na) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!mentionedNumericsCounts %>% is.na)
  }

  all_counts <-
    seq_along(counts_data$mentionedNumericsCounts) %>%
    future_map_dfr(function(x) {
      parse_mentioned_numerics(field = counts_data$mentionedNumericsCounts[x],
                               return_wide = F) %>%
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) %>%
    dplyr::select(idGKG, everything())

  if (include_char_loc == F) {
    all_counts <-
      all_counts %>%
      dplyr::select(-charLoc)
  }

  all_counts <-
    all_counts %>%
    get_clean_count_data(count_col = 'idArticleNumericItem', return_wide = return_wide) %>%
    separate(
      idGKG,
      into = c('GKG', 'dateTime'),
      sep = '\\-',
      remove = F
    ) %>%
    mutate(dateTime = dateTime %>% as.numeric) %>%
    arrange(dateTime) %>%
    dplyr::select(-c(dateTime, GKG)) %>%
    suppressWarnings()

  if (!return_wide) {
    all_counts <-
      all_counts %>%
      .resolve_long_names()
  }

  return(all_counts)
}

#' Returns long or wide mentioned people from a GKG data frame
#'
#' @param gdelt_data
#' @param people_column options \code{c('person', 'persons', 'persons.count', 'personsCharLoc', 'charLoc'))}
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_people <- function(gdelt_data,
                                       people_column = 'personsCharLoc',
                                       filter_na = T,
                                       return_wide = T) {
  people_count_cols <-
    c('person',
      'persons',
      'persons.count',
      'personsCharLoc',
      'charLoc')

  if (!people_column %in% people_count_cols) {
    stop("Sorry people column can only be\n" %>%
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
             return_wide = F) {
      options(scipen = 99999)
      if (field %>% is.na) {
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
          field %>%
          str_split('\\;') %>%
          flatten_chr() %>%
          .[!. %in% '']

        fields_df <-
          tibble(field = fields) %>%
          dplyr::mutate(idArticlePerson = 1:n()) %>%
          separate(field,
                   into = c('namePerson', 'charLoc'),
                   sep = '\\,') %>%
          suppressWarnings() %>%
          suppressMessages()

        if (return_wide) {
          fields_df <-
            fields_df %>%
            gather(item, value, -idArticlePerson) %>%
            arrange(idArticlePerson) %>%
            unite(item, item, idArticlePerson, sep = '.') %>%
            suppressWarnings()

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            mutate_at(
              field_data %>% dplyr::select(dplyr::matches('charLoc')) %>% names(),
              funs(. %>% as.character() %>% readr::parse_number())
            )

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(idArticlePerson, namePerson, charLoc) %>%
            dplyr::mutate(charLoc = charLoc %>% as.numeric)
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
    gdelt_data %>%
    dplyr::select_(.dots = col_names)

  names(counts_data)[2] <-
    'people_col'

  if (filter_na) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!people_col %>% is.na())
  }

  all_counts <-
    seq_along(counts_data$people_col) %>%
    future_map_dfr(function(x) {
      parse_mentioned_people_counts(field = counts_data$people_col[x],
                                    return_wide = F) %>%
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) %>%
    dplyr::select(idGKG, everything())

  if (people_column == 'persons') {
    all_counts <-
      all_counts %>%
      dplyr::select(-charLoc)
  }

  all_counts <-
    all_counts %>%
    get_clean_count_data(count_col = 'idArticlePerson', return_wide = return_wide) %>%
    separate(
      idGKG,
      into = c('GKG', 'dateTime'),
      sep = '\\-',
      remove = F
    ) %>%
    mutate(dateTime = dateTime %>% as.numeric) %>%
    arrange(dateTime) %>%
    dplyr::select(-c(dateTime, GKG)) %>%
    suppressWarnings()

  if (!return_wide) {
    all_counts <-
      all_counts %>%
      .resolve_long_names()
  }

  return(all_counts)
}

#' Returns long or wide mentioned organizations from a GKG data frame
#'
#' @param gdelt_data
#' @param organization_column options \code{c('organization', 'organizations', 'organizations.count', 'organizationsCharLoc', 'charLoc'))}
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_organizations <- function(gdelt_data,
                                              organization_column = 'organizations',
                                              filter_na = T,
                                              return_wide = T) {
  organization_count_cols <-
    c(
      'organization',
      'organizations',
      'organizations.count',
      'organizationsCharLoc',
      'charLoc'
    )

  if (!organization_column %in% organization_count_cols) {
    stop("Sorry people column can only be\n" %>%
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
             return_wide = F) {
      options(scipen = 99999)
      if (field %>% is.na) {
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
          field %>%
          str_split('\\;') %>%
          flatten_chr() %>%
          .[!. %in% '']

        fields_df <-
          tibble(field = fields) %>%
          dplyr::mutate(idArticle.organization = 1:n()) %>%
          separate(field,
                   into = c('nameOrganization', 'charLoc'),
                   sep = '\\,') %>%
          suppressMessages() %>%
          suppressWarnings()

        if (return_wide) {
          fields_df <-
            fields_df %>%
            gather(item, value, -idArticle.organization) %>%
            arrange(idArticle.organization) %>%
            unite(item, item, idArticle.organization, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            mutate_at(
              field_data %>% dplyr::select(dplyr::matches('charLoc')) %>% names(),
              funs(. %>% as.character() %>% readr::parse_number())
            )
        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(idArticle.organization, nameOrganization, charLoc) %>%
            dplyr::mutate(charLoc = charLoc %>% as.numeric)
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
    gdelt_data %>%
    dplyr::select_(.dots = col_names)

  names(counts_data)[2] <-
    'org_col'

  if (filter_na) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!org_col %>% is.na())
  }

  all_counts <-
    seq_along(counts_data$org_col) %>%
    future_map_dfr(function(x) {
      parse_mentioned_organization_counts(field = counts_data$org_col[x],
                                          return_wide = F) %>%
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) %>%
    dplyr::select(idGKG, everything())

  if (organization_column == 'organizations') {
    all_counts <-
      all_counts %>%
      dplyr::select(-charLoc)
  }

  all_counts <-
    all_counts %>%
    get_clean_count_data(count_col = 'idArticle.organization', return_wide = return_wide) %>%
    separate(
      idGKG,
      into = c('GKG', 'dateTime'),
      sep = '\\-',
      remove = F
    ) %>%
    mutate(dateTime = dateTime %>% as.numeric) %>%
    arrange(dateTime) %>%
    dplyr::select(-c(dateTime, GKG)) %>%
    suppressWarnings()

  if (!return_wide) {
    all_counts <-
      all_counts %>%
      .resolve_long_names()
  }


  return(all_counts)
}

#' Returns mentioned names from a GKG data frame.
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_names <- function(gdelt_data,
                                      filter_na = T,
                                      return_wide = T) {
  parse_mentioned_names_counts <-
    function(field = "Interior Minister Chaudhry Nisar Ali Khan,47;Mullah Mansour,87;Afghan Taliban,180;Mullah Mansour,382;Mullah Mansor,753;Mullah Mansour,815;Mullah Mansour,1025",
             return_wide = return_wide) {
      options(scipen = 99999)
      if (field %>% is.na()) {
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
          field %>%
          str_split('\\;') %>%
          flatten_chr() %>%
          .[!. %in% '']

        fields_df <-
          tibble(field = fields) %>%
          dplyr::mutate(idArticleMentionedName = 1:n()) %>%
          separate(field,
                   into = c('nameMentionedName', 'charLoc'),
                   sep = '\\,') %>%
          mutate(charLoc = charLoc %>% as.numeric()) %>%
          suppressMessages() %>%
          suppressWarnings()

        if (return_wide) {
          fields_df <-
            fields_df %>%
            gather(item, value, -c(idArticleMentionedName, charLoc)) %>%
            arrange(idArticleMentionedName) %>%
            unite(item, item, idArticleMentionedName, sep = '')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            dplyr::select(-dplyr::matches("charLoc")) %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            mutate_at(
              field_data %>% dplyr::select(dplyr::matches('charLoc')) %>% names(),
              funs(. %>% as.character() %>% readr::parse_number())
            )
        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
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
    gdelt_data %>%
    dplyr::select(idGKG, mentionedNamesCounts)

  all_counts <-
    seq_along(counts_data$mentionedNamesCounts) %>%
    future_map_dfr(function(x) {
      parse_mentioned_names_counts(field = counts_data$mentionedNamesCounts[x],
                                   return_wide = F) %>%
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) %>%
    dplyr::select(idGKG, everything())

  if (filter_na) {
    if ('nameMentionedName' %in% names(all_counts)) {
      all_counts <-
        all_counts %>%
        dplyr::filter(!nameMentionedName %>% is.na())
    }
  }

  all_counts <-
    all_counts %>%
    get_clean_count_data(count_col = 'idArticleMentionedName',
                         return_wide = F) %>%
    separate(
      idGKG,
      into = c('GKG', 'dateTime'),
      sep = '\\-',
      remove = F
    ) %>%
    mutate(dateTime = dateTime %>% as.numeric()) %>%
    select(-dplyr::matches("charLoc")) %>%
    arrange(dateTime) %>%
    dplyr::select(-c(dateTime, GKG)) %>%
    suppressWarnings()

  if (return_wide) {
    all_counts <-
      all_counts %>%
      spread(item, value)
  }

  if (!return_wide) {
    all_counts <-
      all_counts %>%
      .resolve_long_names()
  }

  return(all_counts)
}

#' Returns mentioned themes from a gkg data frame
#'
#' @param gdelt_data
#' @param filter_na
#' @param organization_column options \code{c('theme', 'themes', 'countThemes', 'themesCharLoc', 'charLoc'))}
#' @param return_wide
#' @importFrom purrr map
#' @import purrr
#' @return
#' @export
#'
#' @examples

parse_gkg_mentioned_themes <- function(gdelt_data,
                                       filter_na = T,
                                       theme_column = 'themes',
                                       return_wide = T) {
  theme_count_cols <-
    c('theme',
      'themes',
      'countThemes',
      'themesCharLoc',
      'charLoc')

  if (!theme_column %in% theme_count_cols) {
    stop("Sorry theme column can only be\n" %>%
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
             return_wide = F) {
      options(scipen = 99999)
      if (field %>% is.na) {
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
          field %>%
          str_split('\\;') %>%
          flatten_chr() %>%
          .[!. %in% '']

        fields_df <-
          tibble(field = fields) %>%
          dplyr::mutate(idArticleGKGTheme = 1:n()) %>%
          separate(field,
                   into = c('codeTheme', 'charLoc'),
                   sep = '\\,') %>%
          suppressWarnings()

        if (return_wide) {
          fields_df <-
            fields_df %>%
            gather(item, value, -idArticleGKGTheme) %>%
            arrange(idArticleGKGTheme) %>%
            unite(item, item, idArticleGKGTheme, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            mutate_at(
              field_data %>% dplyr::select(dplyr::matches('charLoc')) %>% names(),
              funs(. %>% as.character() %>% readr::parse_number())
            )
        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(idArticleGKGTheme, codeTheme, charLoc) %>%
            dplyr::mutate(charLoc = charLoc %>% as.numeric)
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
    gdelt_data %>%
    dplyr::select_(.dots = col_names)
  names(counts_data)[2] <-
    'theme_col'

  if (filter_na) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!theme_col %>% is.na())
  }

  all_counts <-
    seq_along(counts_data$theme_col) %>%
    future_map_dfr(function(x) {
      parse_mentioned_names_themes(field = counts_data$theme_col[x],
                                   return_wide = F) %>%
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) %>%
    dplyr::select(idGKG, everything())

  if (theme_column == 'themes') {
    all_counts <-
      all_counts %>%
      dplyr::select(-charLoc)
  }

  all_counts <-
    all_counts %>%
    get_clean_count_data(count_col = 'idArticleGKGTheme', return_wide = return_wide) %>%
    separate(
      idGKG,
      into = c('GKG', 'dateTime'),
      sep = '\\-',
      remove = F
    ) %>%
    mutate(dateTime = dateTime %>% as.numeric) %>%
    arrange(dateTime) %>%
    dplyr::select(-c(dateTime, GKG)) %>%
    suppressWarnings()

  if (!return_wide) {
    all_counts <-
      all_counts %>%
      .resolve_long_names()
  }

  return(all_counts)
}

#' Returns social embed information from a gkg data frame
#'
#' @param gdelt_data
#' @param social_embed_column options \code{c('urlSocialMediaImageEmbeds', 'images', 'urlSocialMediaVideoEmbeds', 'video', 'videos'))}
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples

parse_gkg_mentioned_social_embeds <-
  function(gdelt_data,
           social_embed_column = 'urlSocialMediaImageEmbeds',
           filter_na = T,
           return_wide = T) {
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
      stop("Social embed column can only be\n" %>% paste0(paste0(image_video_cols, collapse = '\n')))
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
               return_wide = F) {
        options(scipen = 99999)
        if (field %>% is.na) {
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
            field %>%
            str_split('\\;') %>%
            flatten_chr() %>%
            .[!. %in% '']

          fields_df <-
            tibble(urlSocialMediaImageEmbed = fields) %>%
            dplyr::mutate(
              idArticleSocialMediaImageEmbed = 1:n(),
              domainSocialMediaImageEmbed = urlSocialMediaImageEmbed %>% urltools::domain()
            )
          if (return_wide) {
            fields_df <-
              fields_df %>%
              gather(item, value, -idArticleSocialMediaImageEmbed) %>%
              arrange(idArticleSocialMediaImageEmbed) %>%
              unite(item, item, idArticleSocialMediaImageEmbed, sep = '.')

            order_fields <-
              fields_df$item

            field_data <-
              fields_df %>%
              spread(item, value) %>%
              dplyr::select_(.dots = order_fields)

          } else {
            field_data <-
              fields_df

            field_data <-
              field_data %>%
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
      gdelt_data %>%
      dplyr::select_(.dots = col_names)

    names(counts_data)[2] <-
      'source_col'

    if (filter_na) {
      counts_data <-
        counts_data %>%
        dplyr::filter(!source_col %>% is.na())
    }

    all_counts <-
      seq_along(counts_data$source_col) %>%
      future_map_dfr(function(x) {
        parse_embeds(field = counts_data$source_col[x],
                     return_wide = F) %>%
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
      all_counts %>%
      dplyr::select(dplyr::matches("id^[A-Z]"), everything())

    id_col <-
      all_counts %>% select(dplyr::matches("^id[A-Z]")) %>% names() %>% .[[1]] %>% suppressWarnings()
    all_counts <-
      all_counts %>%
      get_clean_count_data(count_col = id_col, return_wide = return_wide) %>%
      separate(
        idGKG,
        into = c('GKG', 'dateTime'),
        sep = '\\-',
        remove = F
      ) %>%
      mutate(dateTime = dateTime %>% as.numeric) %>%
      arrange(dateTime) %>%
      dplyr::select(-c(dateTime, GKG)) %>%
      suppressWarnings()

    if (!return_wide) {
      all_counts <-
        all_counts %>%
        gather(item, value, -idGKG, na.rm = TRUE) %>%
        .resolve_long_names()
    }

    return(all_counts)
  }

#' Returns article tones from a gkg data frame
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_article_tone <-
  function(gdelt_data,
           filter_na = T,
           return_wide = T) {
    parse_article_tones <-
      function(field = "-4.65116279069767,1.55038759689922,62015503875969,7.75193798449612,13.1782945736434,0,134",
               return_wide = F) {
        options(scipen = 99999, digits = 5)
        if (field %>% is.na) {
          if (return_wide) {
            field_data <-
              tibble(amount.tone = NA)
          } else {
            field_data <-
              tibble(amount.tone = NA, idArticle.tone = 1)
          }
        }  else {
          fields <-
            field %>%
            str_split('\\,') %>%
            flatten_chr() %>%
            .[!. %in% ''] %>%
            as.numeric()

          fields_df <-
            tibble(amount.tone = fields) %>%
            dplyr::mutate(idArticle.tone = 1:n())
          if (return_wide) {
            fields_df <-
              fields_df %>%
              gather(item, value, -idArticle.tone) %>%
              arrange(idArticle.tone) %>%
              unite(item, item, idArticle.tone, sep = '.')

            order_fields <-
              fields_df$item

            field_data <-
              fields_df %>%
              spread(item, value) %>%
              dplyr::select_(.dots = order_fields)

          } else {
            field_data <-
              fields_df

            field_data <-
              field_data %>%
              dplyr::select(idArticle.tone, amount.tone)
          }
        }

        return(field_data)
      }

    if (!'tone' %in% names(gdelt_data)) {
      stop("Sorry missing tone column")
    }
    counts_data <-
      gdelt_data %>%
      dplyr::select(idGKG, tone)

    all_counts <-
      seq_along(counts_data$tone) %>%
      future_map_dfr(function(x) {
        parse_article_tones(field = counts_data$tone[x], return_wide = F) %>%
          dplyr::mutate(idGKG = counts_data$idGKG[x])
      }) %>%
      dplyr::select(idGKG, everything()) %>%
      dplyr::rename(scoreTone = amount.tone)

    if (filter_na) {
      all_counts <-
        all_counts %>%
        dplyr::filter(!scoreTone %>% is.na)
    }

    all_counts <-
      all_counts %>%
      get_clean_count_data(count_col = 'idArticle.tone',
                           extra_key = NA,
                           return_wide = return_wide) %>%
      arrange(idGKG) %>%
      separate(
        idGKG,
        into = c('GKG', 'dateTime'),
        sep = '\\-',
        remove = F
      ) %>%
      mutate(dateTime = dateTime %>% as.numeric) %>%
      arrange(dateTime) %>%
      dplyr::select(-c(dateTime, GKG)) %>%
      suppressWarnings()

    if (!return_wide) {
      all_counts <-
        all_counts %>%
        .resolve_long_names()
    }

    return(all_counts)
  }

#' Returns mentioned CAMEO event count from a gkg data frame
#'
#' @param gdelt_data
#' @param count_column options \code{c('count', 'counts'))}
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_event_counts <- function(gdelt_data,
                                             count_column = 'counts',
                                             filter_na = T,
                                             return_wide = T) {
  count_cols <-
    c('counts',
      'count',
      'countsCharLoc',
      'countCharLoc',
      'charLoc')

  if (!count_column %in% count_cols) {
    stop("Sorry count column can only be\n" %>%
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
             return_wide = F) {
      options(scipen = 99999, digits = 5)
      if (field %>% is.na) {
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
          field %>%
          str_split('\\;') %>%
          flatten_chr() %>%
          .[!. %in% '']

        fields_df <-
          tibble(field_item = fields) %>%
          dplyr::mutate(idArticle.field = 1:n()) %>%
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
          ) %>%
          suppressMessages() %>%
          suppressWarnings()

        fields_df <-
          fields_df %>%
          mutate_at(
            fields_df %>% dplyr::select(dplyr::matches("count|charLoc|idTypeLocation")) %>% dplyr::select(-idCountry) %>% names(),
            funs(. %>% as.numeric())
          ) %>%
          mutate_at(fields_df %>% dplyr::select(dplyr::matches("latitude|longitude")) %>% names(),
                    (funs(as.numeric(., digits = 5))))

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
          fields_df %>%
          dplyr::left_join(tibble(
            idTypeLocation = 1:5,
            typeLocation = c(
              'country',
              'usState',
              'usCity',
              'worldCity',
              'worldState'
            )
          )) %>%
          dplyr::select(codeGKGTheme:idTypeLocation,
                        typeLocation,
                        everything()) %>%
          suppressMessages()

        if (return_wide) {
          fields_df <-
            fields_df %>%
            gather(item, value, -idArticle.field) %>%
            arrange(idArticle.field) %>%
            unite(item, item, idArticle.field, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            mutate_at(
              fields_df %>% dplyr::select(dplyr::matches("count|charLoc|idTypeLocation")) %>% dplyr::select(-one_of("idCountry")) %>% names(),
              funs(. %>% as.numeric())
            ) %>%
            mutate_at(fields_df %>% dplyr::select(dplyr::matches("latitude|longitude")) %>% names(),
                      (funs(as.numeric(., digits = 5))))

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
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
    gdelt_data %>%
    dplyr::select_(.dots = col_names)

  names(counts_data)[2] <-
    'count_col'

  if (filter_na) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!count_col %>% is.na())
  }

  all_counts <-
    seq_along(counts_data$count_col) %>%
    future_map_dfr(function(x) {
      parse_field_count(field = counts_data$count_col[x],
                        return_wide = F) %>%
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) %>%
    dplyr::select(idGKG, everything()) %>%
    select(which(colMeans(is.na(.)) < 1))

  all_counts <-
    all_counts %>%
    get_clean_count_data(count_col = 'idArticle.field', return_wide = return_wide) %>%
    separate(
      idGKG,
      into = c('GKG', 'dateTime'),
      sep = '\\-',
      remove = F
    ) %>%
    mutate(dateTime = dateTime %>% as.numeric) %>%
    arrange(dateTime) %>%
    dplyr::select(-c(dateTime, GKG)) %>%
    suppressWarnings()

  if (!return_wide) {
    all_counts <-
      all_counts %>%
      .resolve_long_names()
  }

  return(all_counts)
}

#' Returns mentioned locations from a gkg data frame
#'
#' @param gdelt_data
#' @param location_column options \code{c('location', 'locations', 'locationsCharLoc', 'locationCharLoc', 'charLoc'))}
#' @param isCharLoc
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_locations <-
  function(gdelt_data,
           location_column = 'locations',
           isCharLoc = F,
           filter_na = T,
           return_wide = T) {
    location_cols <-
      c('location',
        'locations',
        'locationsCharLoc',
        'locationCharLoc',
        'charLoc')

    if (!location_column %in% location_cols) {
      stop("Sorry location column can only be\n" %>%
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
               return_wide = F) {
        options(scipen = 99999, digits = 5)
        if (field %>% is.na) {
          if (return_wide) {
            field_data <-
              tibble(location = NA)
          } else {
            field_data <-
              tibble(location = NA, idArticle.location = 1)
          }
        }  else {
          fields <-
            field %>%
            str_split('\\;') %>%
            flatten_chr() %>%
            .[!. %in% '']

          fields_df <-
            tibble(field_item = fields) %>%
            dplyr::mutate(idArticle.location = 1:n())


          if (isCharLoc) {
            fields_df <-
              fields_df %>%
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
              ) %>%
              suppressMessages() %>%
              suppressWarnings()

          } else {
            fields_df <-
              fields_df %>%
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
              ) %>%
              suppressMessages() %>%
              suppressWarnings()
          }

          fields_df <-
            fields_df %>%
            mutate_at(
              fields_df %>% dplyr::select(dplyr::matches("idTypeLocation|charLoc")) %>% names(),
              funs(. %>% as.numeric())
            ) %>%
            mutate_at(fields_df %>% dplyr::select(dplyr::matches("latitude|longitude")) %>% names(),
                      (funs(as.numeric(., digits = 5)))) %>%
            dplyr::left_join(tibble(
              idTypeLocation = 1:5,
              typeLocation = c(
                'country',
                'usState',
                'usCity',
                'worldCity',
                'worldState'
              )
            )) %>%
            suppressMessages() %>%
            dplyr::select(idTypeLocation, typeLocation, everything()) %>%
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
              fields_df %>%
              gather(item, value, -idArticle.location) %>%
              arrange(idArticle.location) %>%
              unite(item, item, idArticle.location, sep = '.')

            order_fields <-
              fields_df$item

            field_data <-
              fields_df %>%
              spread(item, value) %>%
              dplyr::select_(.dots = order_fields)

            field_data <-
              field_data %>%
              mutate_at(
                fields_df %>% dplyr::select(dplyr::matches("idTypeLocation|charLoc")) %>% names(),
                funs(. %>% as.numeric())
              ) %>%
              mutate_at(fields_df %>% dplyr::select(dplyr::matches("latitude|longitude")) %>% names(),
                        (funs(as.numeric(., digits = 5))))
          } else {
            field_data <-
              fields_df

            field_data <-
              field_data %>%
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
      gdelt_data %>%
      dplyr::select_(.dots = col_names)

    names(counts_data)[2] <-
      'loc_col'

    if (filter_na) {
      counts_data <-
        counts_data %>%
        dplyr::filter(!loc_col %>% is.na())
    }

    all_counts <-
      seq_along(counts_data$loc_col) %>%
      future_map_dfr(function(x) {
        parse_location_count(field = counts_data$loc_col[x],
                             return_wide = F) %>%
          dplyr::mutate(idGKG = counts_data$idGKG[x])
      }) %>%
      dplyr::select(idGKG, everything())

    if (filter_na) {
      all_counts <-
        all_counts %>%
        dplyr::filter(!location %>% is.na())
    }
    all_counts <-
      all_counts %>%
      get_clean_count_data(count_col = 'idArticle.location', return_wide = return_wide) %>%
      separate(
        idGKG,
        into = c('GKG', 'dateTime'),
        sep = '\\-',
        remove = F
      ) %>%
      mutate(dateTime = dateTime %>% as.numeric) %>%
      arrange(dateTime) %>%
      dplyr::select(-c(dateTime, GKG)) %>%
      suppressWarnings()

    if (!return_wide) {
      all_counts <-
        all_counts %>%
        .resolve_long_names()
    }


    return(all_counts)
  }

#' Returns mentioned dates from gkg data frame
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_dates <- function(gdelt_data,
                                      filter_na = T,
                                      return_wide = T) {
  parse_dates <-
    function(field = "4#6#16#0#734;4#4#26#0#2258",
             return_wide = F) {
      options(scipen = 99999, digits = 5)
      if (field %>% is.na) {
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
          field %>%
          str_split('\\;') %>%
          flatten_chr() %>%
          .[!. %in% '']

        fields_df <-
          tibble(field_item = fields) %>%
          dplyr::mutate(idDateArticle = 1:n()) %>%
          separate(
            col = field_item,
            sep = '\\#',
            into = c('idDateResolution', 'month', 'day', 'year', 'charLoc')
          ) %>%
          suppressMessages() %>%
          suppressWarnings()

        fields_df <-
          fields_df %>%
          mutate_at(fields_df  %>% names(),
                    funs(. %>% as.numeric())) %>%
          dplyr::left_join(tibble(
            idDateResolution = 1:4,
            dateResolution = c(
              'ex_mon_date',
              'year_only',
              'month_date' ,
              'fully_resolved'
            )
          )) %>%
          suppressMessages() %>%
          dplyr::select(idDateResolution, dateResolution, everything())

        if (return_wide) {
          fields_df <-
            fields_df %>%
            gather(item, value, -idDateArticle) %>%
            arrange(idDateArticle) %>%
            unite(item, item, idDateArticle, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            mutate_at(field_data %>% dplyr::select(dplyr::matches(
              "idDateResolution|month|day|year|charLoc"
            )) %>% names(),
            funs(. %>% as.numeric()))

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(idDateArticle, everything())
        }
      }

      return(field_data)
    }

  if (!'dates' %in% names(gdelt_data)) {
    stop("Sorry missing date column")
  }
  counts_data <-
    gdelt_data %>%
    dplyr::select(idGKG, dates)

  if (filter_na) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!dates %>% is.na)
  }

  all_counts <-
    seq_along(counts_data$dates) %>%
    future_map_dfr(function(x) {
      parse_dates(field = counts_data$dates[x], return_wide =  F) %>%
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) %>%
    dplyr::select(idGKG, everything()) %>%
    dplyr::select(-idDateResolution)

  all_counts <-
    all_counts %>%
    get_clean_count_data(count_col = 'idDateArticle', return_wide = return_wide) %>%
    separate(
      idGKG,
      into = c('GKG', 'dateTime'),
      sep = '\\-',
      remove = F
    ) %>%
    mutate(dateTime = dateTime %>% as.numeric) %>%
    arrange(dateTime) %>%
    dplyr::select(-c(dateTime, GKG)) %>%
    suppressWarnings()

  if (!return_wide) {
    all_counts <-
      all_counts %>%
      .resolve_long_names()
  }

  return(all_counts)
}

#' Returns mentioned quotes from a gkg data frame
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_quotes <-
  function(gdelt_data,
           filter_na = T,
           return_wide = T) {
    parse_quotes <-
      function(field = "495|51||knowingly aided and abetted an international kidnap#865|50||nothing less than an international child abduction#2764|49|| staff member should be singled out for dismissal#3373|48||make any serious attempt to independently verify#4059|46||wants to go through every single little detail#4802|156||And xC2 ; xA0 ; you're keeping all of xC2 ; xA0 ; them xC2 ; xA0 ; - xC2 ; xA0 ; except one sacrificial lamb - xC2 ; xA0 ; to run the show?#4879|28||How do you think that looks?#6093|60||an extraordinary conspiracy to remove the children illegally#6150|50||nothing less than an international child abduction#6828|408||I xE2 ; x80 ; xA6 ; have found nothing that supports a finding that any Australian Government official somehow knowingly assisted the mother to do something that was wrong xE2 ; x80 ; xA6 ; I do not find xE2 ; x80 ; xA6 ; that any Australian Embassy officials who helped the mother did so knowing that the mother did not have the father consent to remove the girls permanently from Italy",
               return_wide = F) {
        options(scipen = 99999, digits = 5)
        if (field %>% is.na) {
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
            field %>%
            str_split('\\#') %>%
            flatten_chr() %>%
            .[!. %in% '']

          fields_df <-
            tibble(quote_items = fields) %>%
            dplyr::mutate(idArticleQuote = 1:n()) %>%
            separate(
              col = quote_items,
              sep = '\\|',
              into = c('charLoc', 'lengthQuote', 'verbIntro', 'textQuote')
            ) %>%
            suppressMessages() %>%
            suppressWarnings()

          fields_df <-
            fields_df %>%
            dplyr::mutate(textQuote = textQuote %>% str_trim) %>%
            mutate_at(c('charLoc', 'lengthQuote'),
                      funs(. %>% as.numeric())) %>%
            dplyr::select(idArticleQuote, everything())

          fields_df$verbIntro[fields_df$verbIntro == ''] <-
            NA

          if (return_wide) {
            fields_df <-
              fields_df %>%
              gather(item, value, -idArticleQuote) %>%
              arrange(idArticleQuote) %>%
              unite(item, item, idArticleQuote, sep = '.')

            order_fields <-
              fields_df$item

            field_data <-
              fields_df %>%
              spread(item, value) %>%
              dplyr::select_(.dots = order_fields)

            field_data <-
              field_data %>%
              mutate_at(field_data %>% dplyr::select(dplyr::matches("charLoc|lengthQuote")) %>% names(),
                        funs(. %>% as.numeric()))

          } else {
            field_data <-
              fields_df

            field_data <-
              field_data %>%
              dplyr::select(idArticleQuote, everything())
          }
        }

        return(field_data)
      }

    if (!'quotations' %in% names(gdelt_data)) {
      stop("Sorry missing quotations column")
    }

    counts_data <-
      gdelt_data %>%
      dplyr::select(idGKG, quotations)

    if (filter_na) {
      counts_data <-
        counts_data %>%
        dplyr::filter(!quotations %>% is.na())
    }

    all_counts <-
      seq_along(counts_data$quotations) %>%
      future_map_dfr(function(x) {
        parse_quotes(field = counts_data$quotations[x], return_wide = F) %>%
          dplyr::mutate(idGKG = counts_data$idGKG[x])
      }) %>%
      dplyr::select(idGKG, everything())

    if (all_counts$verbIntro %>% is.na() %>% as.numeric() %>% sum == nrow(all_counts)) {
      all_counts <-
        all_counts %>%
        dplyr::select(-verbIntro)
    }

    all_counts <-
      all_counts %>%
      get_clean_count_data(count_col = 'idArticleQuote', return_wide = T) %>%
      separate(
        idGKG,
        into = c('GKG', 'dateTime'),
        sep = '\\-',
        remove = F
      ) %>%
      mutate(dateTime = dateTime %>% as.numeric) %>%
      arrange(dateTime) %>%
      dplyr::select(-c(dateTime, GKG)) %>%
      suppressWarnings()

    if (!return_wide) {
      all_counts <-
        all_counts %>%
        gather(item, value, -idGKG, na.rm = TRUE) %>%
        .resolve_long_names()
    }

    return(all_counts)
  }

#' Returns GCAM codes from a gkg data frame
#'
#' @param gdelt_data
#' @param merge_gcam_codes
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_gcams <- function(gdelt_data,
                                      merge_gcam_codes = F,
                                      filter_na = T,
                                      return_wide = T) {
  parse_gcam_data <-
    function(field = "wc:284,c121:5",
             return_wide = F) {
      options(scipen = 99999)
      if (field %>% is.na) {
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
          field %>%
          str_split('\\,') %>%
          flatten_chr() %>%
          .[!. %in% '']

        articleWordCount <-
          fields[1] %>%
          as.character() %>%
          readr::parse_number()

        fields_df <-
          tibble(articleWordCount,
                     idGCAM = fields[2:length(fields)]) %>%
          separate(idGCAM,
                   into = c('idGCAM', 'scoreGoldsteinWords'),
                   sep = '\\:') %>%
          dplyr::mutate(
            idArticleGCAM = 1:n(),
            scoreGoldsteinWords = scoreGoldsteinWords %>% as.numeric(., digits = 4)
          )


        if (return_wide) {
          fields_df <-
            fields_df %>%
            gather(item, value, -c(articleWordCount, idArticleGCAM)) %>%
            arrange(idArticleGCAM) %>%
            unite(item, item, idArticleGCAM, sep = '.')

          order_fields <-
            fields_df$item

          field_data <-
            fields_df %>%
            spread(item, value) %>%
            dplyr::select_(.dots = order_fields)

          field_data <-
            field_data %>%
            mutate_at(field_data %>% dplyr::select(dplyr::matches("scoreGoldsteinWords")) %>% names(),
                      funs(. %>% as.numeric()))

        } else {
          field_data <-
            fields_df

          field_data <-
            field_data %>%
            dplyr::select(idArticleGCAM, everything())
        }
      }

      return(field_data)
    }

  if (!'gcam' %in% names(gdelt_data)) {
    stop("Sorry missing video embed column")
  }

  counts_data <-
    gdelt_data %>%
    dplyr::select(idGKG, gcam)

  if (filter_na) {
    counts_data <-
      counts_data %>%
      dplyr::filter(!gcam %>% is.na())
  }

  all_counts <-
    seq_along(counts_data$gcam) %>%
    future_map_dfr(function(x) {
      parse_gcam_data(field = counts_data$gcam[x],
                      return_wide = F) %>%
        dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) %>%
    dplyr::select(idGKG, everything())

  if (merge_gcam_codes) {
    all_counts <-
      all_counts %>%
      dplyr::left_join(
        get_codes_gcam() %>%
          dplyr::select(
            idGCAM,
            typeDictionary,
            dictionaryHumanName,
            dimensionHumanName
          )
      )
  }

  all_counts <-
    all_counts %>%
    get_clean_count_data(count_col = 'idArticleGCAM',
                         extra_key = 'articleWordCount',
                         return_wide = return_wide) %>%
    separate(
      idGKG,
      into = c('GKG', 'dateTime'),
      sep = '\\-',
      remove = F
    ) %>%
    mutate(dateTime = dateTime %>% as.numeric) %>%
    arrange(dateTime) %>%
    dplyr::select(-c(dateTime, GKG)) %>%
    suppressWarnings()

  if (!return_wide) {
    all_counts <-
      all_counts %>%
      .resolve_long_names()
  }

  return(all_counts)
}


#' Returns source name or source url from a gkg data frame
#'
#' @param gdelt_data
#' @param source_column options \code{c('sources', 'source', 'sources.url', 'source.url'))}
#' @param filter_na
#' @param return_wide
#' @importFrom urltools domain
#'
#' @return
#' @export
#'
#' @examples
parse_gkg_mentioned_source_data <-
  function(gdelt_data,
           source_column = 'sources',
           filter_na = T,
           return_wide = F) {
    source_options <-
      c('sources', 'source', 'urlSources', 'urlSource')
    if (!source_column %in% source_options) {
      stop("Sorry source column can only be\n" %>%
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
               return_wide = F) {
        options(scipen = 99999)
        if (field %>% is.na) {
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
            field %>%
            str_split('\\;') %>%
            flatten_chr() %>%
            .[!. %in% ''] %>%
            unique

          fields_df <-
            tibble(nameSource = fields) %>%
            dplyr::mutate(idArticle.source = 1:n())
          if (return_wide) {
            fields_df <-
              fields_df %>%
              gather(item, value, -idArticle.source) %>%
              arrange(idArticle.source) %>%
              unite(item, item, idArticle.source, sep = '.')

            order_fields <-
              fields_df$item

            field_data <-
              fields_df %>%
              spread(item, value) %>%
              dplyr::select_(.dots = order_fields)

          } else {
            field_data <-
              fields_df

            field_data <-
              field_data %>%
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
      gdelt_data %>%
      dplyr::select_(.dots = col_names)

    names(counts_data)[2] <-
      'source_col'

    if (filter_na) {
      counts_data <-
        counts_data %>%
        dplyr::filter(!source_col %>% is.na())
    }

    all_counts <-
      seq_along(counts_data$source_col) %>%
      future_map_dfr(function(x) {
        parse_source_name(field = counts_data$source_col[x],
                          return_wide = return_wide) %>%
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
      all_counts %>%
      dplyr::select(idGKG, everything())

    if (!return_wide) {
      all_counts <-
        all_counts %>%
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
           only_most_recent = F,
           remove_files = T,
           empty_trash = T,
           return_message = T) {
    if (only_most_recent) {
      date_data <-
        Sys.Date()
    }

    if (!date_data %>% substr(5, 5) == "-") {
      stop("Sorry data must be in YMD format, ie, 2016-06-01")
    }
    tables <-
      c('gkg', 'export', 'mentions')
    if (!table_name %in% tables) {
      stop("Sorry tables can only be:\n" %>% paste0(paste0(tables, collapse = '\n')))
    }

    date_data <-
      date_data %>%
      ymd() %>% as.Date()


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
        gdelt_detailed_logs %>%
        dplyr::filter(nameFile == table_name) %>%
        .$urlData
    } else {
      if (!'gdelt_detailed_logs' %>% exists()) {
        gdelt_detailed_logs <-
          get_urls_gkg_15_minute_log()

        assign(x = 'gdelt_detailed_logs',
               eval(gdelt_detailed_logs),
               env = .GlobalEnv)

      }

      urls <-
        gdelt_detailed_logs %>%
        dplyr::filter(dateData == date_data) %>%
        dplyr::filter(nameFile == table_name) %>%
        .$urlData
    }

    get_gdelt_url_data_safe <-
      purrr::possibly(get_gdelt_url_data, tibble())

    all_data <-
      urls %>%
      future_map_dfr(function(x) {
        data <-
          get_gdelt_url_data_safe(
            url = x,
            remove_files = remove_files,
            file_directory = file_directory,
            folder_name = folder_name,
            return_message = return_message,
            empty_trash = empty_trash
          )
        data %>%
          mutate_at(.vars = data %>% dplyr::select(dplyr::matches('idFeatureActor|idFeatureAction')) %>% names(),
                    .funs = as.character)
      }) %>%
      distinct() %>%
      suppressWarnings()

    if ('domainSource' %in% names(all_data)) {
      all_data <-
        all_data %>%
        mutate(domainSource = documentSource %>% urltools::domain())
    }

    if ('urlSource' %in% names(all_data)) {
      all_data <-
        all_data %>%
        mutate(domainSource = urlSource %>% urltools::domain())
    }

    if (return_message) {
      "You retrieved " %>%
        paste0(all_data %>% nrow, " gkg detailed events for ", date_data) %>%
        cat(fill = T)
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
#' \code{T, F}
#' @param empty_trash Do You want to empy the trash
#' \code{T, F}
#' @param return_message Do you want to return a message
#' \code{T, F}
#' @importFrom purrr flatten_chr
#' @importFrom readr parse_number
#' @importFrom purrr compact
#' @import dplyr utils dplyr purrr readr
#' @importFrom urltools domain
#' @importFrom curl curl_download curl
#' @importFrom urltools domain
#' @importFrom purrr map
#' @return
#' @export
#'
#' @examples

get_data_gkg_days_detailed <-
  function(dates = c("2016-07-19"),
           table_name = c("gkg"),
           file_directory = NULL,
           folder_name = 'gdelt_data',
           only_most_recent = F,
           remove_files = T,
           empty_trash = T,
           return_message = T) {
    .get_data_gkg_day_detailed_safe <-
    purrr::possibly(.get_data_gkg_day_detailed, tibble())

  var_matrix <-
    expand.grid(date = dates,
                table_name = table_name,
                stringsAsFactors = F) %>%
    as_tibble()

  all_data <-
    seq_len(var_matrix %>% nrow()) %>%
    future_map_dfr(
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
        )
    ) %>%
    suppressWarnings()

  if ('idDateTime' %in% names(all_data)) {
    all_data <-
      all_data %>%
      dplyr::rename(idDateTime = idDateTimeArticle) %>%
      dplyr::mutate(idDateTime = 1:n()) %>%
      separate(idGKG, sep = '\\-', c('dateCode', 'remove')) %>%
      unite(idGKG,
            dateCode,
            idDateTime,
            sep = '-',
            remove = F) %>%
      dplyr::select(-c(dateCode, remove))
  }

  return(all_data)
}

#' Retrieves gkg summary file for a given day
#'
#' @param date_data Date of the data, must be in year, month day form.
#' @param file_directory location of where the file is to be saved
#' @param is_count_file options
#' \code{c(TRUE, FALSE)}
#' @param remove_files
#' \code{c(TRUE, FALSE)}
#' @param empty_trash Empty trash
#' \code{c(TRUE, FALSE)}
#' @param return_message
#' \code{c(TRUE, FALSE)}
#' @importFrom purrr flatten_chr
#' @importFrom readr parse_number
#' @importFrom purrr compact
#' @import dplyr utils dplyr purrr readr
#' @importFrom urltools domain
#' @importFrom curl curl_download curl
#' @return
#'
#' @examples

get_data_gkg_day_summary <- function(date_data = "2016-06-01",
                                     file_directory = NULL,
                                     folder_name = 'gdelt_data',
                                     is_count_file = F,
                                     remove_files = T,
                                     empty_trash = T,
                                     return_message = T) {
  options(scipen = 99999)
  if (!date_data %>% substr(5, 5) == "-") {
    stop("Sorry data must be in YMD format, ie, 2016-06-01")
  }

  date_data <-
    date_data %>%
    ymd %>% as.Date()


  if (date_data < "2013-04-01") {
    stop("Sorry data starts on April 1st, 2013")
  }

  if (date_data > Sys.Date()) {
    stop("Sorry data can't go into the future")
  }
  if (!'summary_data_urls' %>% exists()) {
    summary_data_urls <-
      get_urls_gkg_daily_summaries(return_message = return_message)

    assign(x = 'summary_data_urls', eval(summary_data_urls), env = .GlobalEnv)
  }

  if (is_count_file) {
    summary_data_urls <-
      summary_data_urls %>%
      dplyr::filter(isCountFile == T)
  } else {
    summary_data_urls <-
      summary_data_urls %>%
      dplyr::filter(isCountFile == F)
  }

  urls <-
    summary_data_urls %>%
    dplyr::filter(dateData == date_data) %>%
    .$urlData

  get_gdelt_url_data_safe <-
    purrr::possibly(get_gdelt_url_data, tibble())

  all_data <-
    urls %>%
    future_map_dfr(function(x) {
      get_gdelt_url_data_safe(
        url = x,
        remove_files = remove_files,
        file_directory = file_directory,
        folder_name = folder_name,
        return_message = return_message,
        empty_trash = empty_trash
      )
    }) %>%
    distinct()

  if ('countObject' %in% names(all_data)) {
    all_data <-
      all_data %>%
      dplyr::mutate(countObject = countObject %>% as.numeric())
  }
  if ('idCAMEOEvents' %in% names(all_data)) {
    all_data <-
      all_data %>%
      mutate(idCAMEOEvents = idCAMEOEvents %>% as.character())
  }

  if (is_count_file == F) {
    all_data <-
      all_data %>%
      dplyr::rename(dateEvent = date)
  }

  all_data <-
    all_data %>%
    mutate(isCountFile = if_else(is_count_file, T, F))


  if (return_message) {
    "You retrieved " %>%
      paste0(all_data %>% nrow(), " gkg summary events for ", date_data) %>%
      cat(fill = T)
  }

  return(all_data)
}

#' Gets days summary GDELT GKG data by table
#'
#' @param dates
#' @param file_directory
#' @param remove_files
#' @param empty_trash
#' @param return_message
#'
#' @return
#' @export
#' @examples
#' get_data_gkg_days_summary(dates = c("2016-07-18"), is_count_file = c(T, F))

get_data_gkg_days_summary <- function(dates = c("2016-06-01"),
                                      is_count_file = c(T, F),
                                      file_directory = NULL,
                                      folder_name = 'gdelt_data',
                                      remove_files = T,
                                      empty_trash = T,
                                      nest_data = F,
                                      return_message = T) {
  get_data_gkg_day_summary_safe <-
    purrr::possibly(get_data_gkg_day_summary, tibble())

  var_matrix <-
    expand.grid(
      date = dates,
      is_count_file = is_count_file,
      stringsAsFactors = F
    ) %>%
    as_tibble %>%
    suppressWarnings()

  all_data <-
    seq_len(var_matrix %>% nrow) %>%
    future_map_dfr(
      function(x)
        get_data_gkg_day_summary_safe(
          date_data = var_matrix$date[x],
          is_count_file = var_matrix$is_count_file[x],
          file_directory = file_directory,
          folder_name = folder_name,
          remove_files = remove_files,
          empty_trash = empty_trash,
          return_message = return_message
        ) %>% suppressWarnings()
    ) %>%
    arrange(idGKG) %>%
    suppressWarnings()

  return(all_data)
}

#' Retreive GDELT data for a given period
#'
#' @param period the GDELT period
#' @param file_directory
#' @param remove_files
#' @param empty_trash
#' @param return_message
#' @importFrom purrr compact
#' @importFrom purrr flatten_chr
#' @importFrom readr parse_number
#' @importFrom purrr compact
#' @import dplyr
#' @import utils
#' @importFrom urltools domain
#' @importFrom curl curl_download curl
#' @return
#'
#' @examples
#' get_data_gdelt_period_event(period = 1983)
get_data_gdelt_period_event <- function(period = 1983,
                                        file_directory = NULL,
                                        folder_name = 'gdelt_data',
                                        remove_files = T,
                                        empty_trash = T,
                                        return_message = T) {
  period <-
    period %>%
    as.character() %>%
    stringr::str_replace_all("\\-", '')

  if (!'gdelt_event_urls' %>% exists()) {
    gdelt_event_urls <-
      get_urls_gdelt_event_log(return_message = return_message)

    assign(x = 'gdelt_event_urls', eval(gdelt_event_urls), env = .GlobalEnv)
  }
  periods <-
    gdelt_event_urls$periodData
  if (!period %in% periods) {
    gdelt_event_message <-
      "Period can only be a 4 digit year between 1979 and 2005\nEXAMPLE: 1983\nA 6 digit year and month from 2006 to March 2013\nEXAMPLE: 201208\nOr an 8 digit year, month, day from March 1, 2013 until today\nEXAMPLE: 20140303"

    stop(gdelt_event_message)
  }

  urls <-
    gdelt_event_urls %>%
    dplyr::filter(periodData == period) %>%
    .$urlData

  get_gdelt_url_data_safe <-
    purrr::possibly(get_gdelt_url_data, tibble())

  all_data <-
    urls %>%
    future_map_dfr(function(x) {
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
    "You retrieved " %>%
      paste0(all_data %>% nrow, " GDELT events for the period of ", period) %>%
      cat(fill = T)
  }

  return(all_data)
}


#' Returns GDELT event data for a given periods
#'
#' @param periods
#' @param file_directory
#' @param is_count_file
#' @param remove_files
#' @param empty_trash
#' @param return_message
#' @import dplyr stringr purrr tidyr readr lubridate
#' @return
#' @export
#'
#' @examples
#' get_data_gdelt_periods_event (periods = c(1983))
get_data_gdelt_periods_event <- function(periods = c(1983, 1989),
                                         file_directory = NULL,
                                         folder_name = 'gdelt_data',
                                         remove_files = T,
                                         empty_trash = T,
                                         return_message = T) {
  get_data_gdelt_period_event_safe <-
    purrr::possibly(get_data_gdelt_period_event, tibble())
  periods <-
    periods %>%
    str_replace_all('\\-', '')
  all_data <-
    seq_along(periods) %>%
    future_map_dfr(
      function(x)
        get_data_gdelt_period_event_safe(
          period = periods[x],
          file_directory = file_directory,
          folder_name = folder_name,
          remove_files = remove_files,
          empty_trash = empty_trash,
          return_message = return_message
        )
    ) %>%
    suppressWarnings()

  return(all_data)
}

#' get_vgkg_schema
#'
#' @return
#' @importFrom dplyr tibble
#'
#' @examples
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
#' @return
#' @export
#' @importFrom lubridate ymd_hms
#' @importFrom dplyr tibble
#' @examples
get_urls_vgkg <- function() {
  options(scipen = 999999)
  cloud_vision_start_hms <-
    20160222113000

  time_now <-
    Sys.time() %>% format("%Y%m%d%H%M%S") %>% as.numeric()

  all_dates <-
    seq(ymd_hms(cloud_vision_start_hms),
        to = time_now %>% ymd_hms,
        by = '15 min')

  url_df <-
    tibble(
      dateTimeData = all_dates,
      isoPeriod = dateTimeData %>% format("%Y%m%d%H%M%S") %>% as.numeric(),
      urlCloudVisionTags = 'http://data.gdeltproject.org/gdeltv2_cloudvision/' %>% paste0(isoPeriod, '.imagetags.csv.gz'),
      urlTranslationTags = 'http://data.gdeltproject.org/gdeltv2_cloudvision/' %>% paste0(isoPeriod, '.translation.imagetags.csv.gz')
    ) %>%
    mutate(dateData = dateTimeData %>% as.Date()) %>%
    arrange(desc(dateTimeData))
  return(url_df)
}

#' Gets most recent CV log URLs
#'
#' @return
#' @import stringr
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @importFrom tidyr spread
#'
#' @examples get_urls_gkg_most_recent_log()
get_urls_vgkg_most_recent  <- function() {
  log_df <-
    'http://data.gdeltproject.org/gdeltv2_cloudvision/lastupdate.txt' %>%
    read_tsv(col_names = F) %>%
    suppressMessages()

  names(log_df) <-
    c('value')

  values <-
    log_df$value %>% str_split("\\ ") %>%
    flatten_chr


  items <-
    c('idVGKPeriod', 'idVGKHash', 'urlCloudVisionTags')

  log_df <-
    tibble(item = items, value = values) %>%
    spread(item, value) %>%
    mutate(idVGKPeriod = idVGKPeriod %>% as.numeric())

  return(log_df)
}

.get_data_vgkg_url <-
  function(url = 'http://data.gdeltproject.org/gdeltv2_cloudvision/20160606234500.imagetagsv1.csv.gz',
           remove_json_column = TRUE,
           return_message = TRUE) {
    ok_url <-
      url %>% httr::url_ok() %>% suppressWarnings()
    if (!ok_url) {
      stop("Invalid url")
    }

    cloud_vision_data <-
      url %>%
      curl::curl() %>%
      gzcon() %>%
      read_tsv(col_names = F) %>%
      suppressWarnings() %>%
      suppressMessages()

    names(cloud_vision_data) <-
      get_vgkg_schema() %>% pull(nameActual)

    cloud_vision_data <-
      cloud_vision_data %>%
      dplyr::mutate(
        dateCodeURL = dateTimeDocument,
        idDateTime = 1:n(),
        idVGKG = dateTimeDocument %>% paste0('-', idDateTime),
        dateTimeDocument = dateTimeDocument %>% lubridate::ymd_hms() %>% lubridate::with_tz(Sys.timezone()),
        dateDocument = dateTimeDocument %>% as.Date(),
        dimWidthHeight = dimWidthHeight %>% as.character() %>% readr::parse_number()
      ) %>%
      dplyr::select(idVGKG, idDateTime, everything())

    if (remove_json_column) {
      cloud_vision_data <-
        cloud_vision_data %>%
        dplyr::select(-jsonCloudVision)
    }


    if (return_message) {
      "Downloaded, parsed and imported " %>%
        paste0(url) %>%
        cat(fill = T)

    }
    return(cloud_vision_data)
  }


.get_data_vgkg_day <-
  function(date_data = "2016-06-08",
           only_most_recent = F,
           include_translations = F,
           remove_json_column = T,
           return_message = T) {
    if (only_most_recent) {
      urls <-
        get_urls_vgkg_most_recent() %>%
        .$urlCloudVisionTags
    } else {
      if (!date_data %>% substr(5, 5) == "-") {
        stop("Sorry data must be in YMD format, ie, 2016-06-01")
      }

      date_data <-
        date_data %>%
        lubridate::ymd() %>%
        as.Date()

      if (date_data < "2016-02-22") {
        stop("Sorry data starts on February 22, 2016")
      }

      if (date_data > Sys.Date()) {
        stop("Sorry data can't go into the future")
      }

      if (!'cv_urls' %>% exists()) {
        cv_urls <-
          get_urls_vgkg()

        assign(x = 'get_urls_vgkg', eval(get_urls_vgkg), env = .GlobalEnv)
      }
      urls <-
        cv_urls %>%
        dplyr::filter(dateData == date_data) %>%
        .$urlCloudVisionTags

      if (include_translations) {
        urls <-
          c(urls,
            cv_urls %>%
              dplyr::filter(dateData == date_data) %>%
              .$urlTranslationTags)
      }
    }

    .get_data_vgkg_url_safe <-
      purrr::possibly(.get_data_vgkg_url, tibble())

    all_data <-
      urls %>%
      future_map_dfr(function(x) {
        .get_data_vgkg_url_safe(
          url = x,
          remove_json_column = remove_json_column,
          return_message = return_message
        )
      }) %>%
      distinct() %>%
      dplyr::select(idVGKG, idDateTime, dateTimeDocument, everything()) %>%
      suppressWarnings()

    if (return_message) {
      "You retrieved " %>%
        paste0(all_data %>% nrow,
               " cloud vision processed items for ",
               date_data) %>%
        cat(fill = T)
    }

    return(all_data)

  }

#' Gets CV Data for stated periods
#'
#' @param date_data
#' @param include_translations
#' @param file_directory
#' @param remove_files
#' @param empty_trash
#' @param return_message
#' @importFrom urltools domain
#' @importFrom httr url_ok
#' @import stringr dplyr purrr curl lubridate tidyr
#' @importFrom purrr flatten_chr
#' @importFrom curl curl_download
#' @importFrom readr read_tsv
#' @importFrom lubridate ymd_hms with_tz ymd
#' @return
#' @export
#'
#' @examples
get_data_vgkg_dates <-
  function(dates = c("2016-06-09", "2016-06-08"),
           include_translations = FALSE,
           only_most_recent = FALSE,
           remove_json_column = TRUE,
           return_message = TRUE) {
    if (only_most_recent) {
      dates <-
        Sys.Date()
    }

    .get_data_vgkg_day_safe <-
      purrr::possibly(.get_data_vgkg_day, tibble())

    all_data <-
      dates %>%
      future_map_dfr(
        function(x)
          .get_data_vgkg_day_safe(
            date_data = x,
            only_most_recent = only_most_recent,
            remove_json_column = remove_json_column,
            return_message = return_message
          )
      )

    all_data <-
      all_data %>%
      separate(idVGKG, into = c('VGKG', 'remove'), '\\-') %>%
      group_by(dateDocument) %>%
      mutate(count = 1:n()) %>%
      mutate(idVGKG = VGKG %>% paste0('-', count)) %>%
      dplyr::select(-c(count, idDateTime, remove, VGKG)) %>%
      dplyr::select(idVGKG, everything()) %>%
      ungroup()

    all_data <-
      all_data %>%
      mutate(domainSource = documentSource %>% urltools::domain()) %>%
      dplyr::select(idVGKG:dateTimeDocument, domainSource, everything())

    return(all_data)

  }

.parse_xml_extra <-
  function(x =  "<PAGE_LINKS>http://therealdeal.com/2015/05/07/hfz-secures-1b-in-financing-for-high-line-site/;http://therealdeal.com/2015/09/10/uk-hedge-fund-loaning-850m-for-relateds-hudson-yards-resi-tower/;http://therealdeal.com/2016/02/05/hfz-seeks-250m-from-eb-5-investors-for-high-line-condos/;http://therealdeal.com/2016/07/28/macklowe-seeking-1b-loan-for-1-wall-street-conversion/;http://therealdeal.com/2016/08/18/first-look-floor-plans-at-hfzs-high-line-development/;http://therealdeal.com/2016/10/05/inside-gary-barnetts-game-of-real-estate-tetris/;http://therealdeal.com/2016/10/06/six-senses-to-open-hotel-at-hfzs-high-line-project/;http://therealdeal.com/issues_articles/whos-bankrolling-the-boom/;http://therealdeal.com/issues_articles/ziel-feldman-it-was-worth-every-penny/</PAGE_LINKS><PAGE_PRECISEPUBTIMESTAMP>20161025180000</PAGE_PRECISEPUBTIMESTAMP>") {
    safe_xml <-
      purrr::possibly(read_xml, otherwise = NULL)

    gdelt_xml <-
      list("<item>", x, "</item>") %>%
      purrr::invoke(paste0, .) %>%
      safe_xml()

    if (gdelt_xml %>% length > 0) {
      values <-
        gdelt_xml %>%
        xml2::xml_children() %>%
        rvest::html_text()

      items <-
        gdelt_xml %>%
        xml2::xml_children() %>%
        xml2::xml_name()

      xml_df <-
        tibble(item = items, value = values)

      xml_df <-
        xml_df$item %>%
        unique %>%
        future_map_dfr(function(x) {
          item_value <-
            xml_df %>%
            filter(item == x) %>%
            .$value %>%
            str_split('\\;') %>%
            flatten_chr()

          tibble(item = x, value = item_value) %>%
            group_by(item) %>%
            mutate(idXMLItem = seq_along(item)) %>%
            ungroup() %>%
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
#' @param data
#' @param return_wide
#'
#' @return
#' @import xml2 purrr dplyr tidyr
#' @return
#' @export
#'
#' @examples
parse_xml_extras <-
  function(data, return_wide = FALSE) {
    xml_data <-
      data %>%
      filter(!xmlExtras %>% is.na()) %>%
      select(idGKG, xmlExtras)
    parse_xml_extra_safe <-
      possibly(.parse_xml_extra, NULL)

    xml_extra_df <-
      1:nrow(xml_data) %>%
      future_map_dfr(function(x) {
        row_data <-
          xml_data %>%
          slice(x)

        has_data <-
          row_data$xmlExtras %>%
          parse_xml_extra_safe() %>% nrow() > 0

        if (has_data) {
          xml_df <-
            row_data$xmlExtras %>%
            parse_xml_extra_safe() %>%
            mutate(idGKG = row_data$idGKG) %>%
            dplyr::select(idGKG, everything())
          return(xml_df)
        }
      })

    xml_extra_df <-
      xml_extra_df %>%
      dplyr::filter(!value %>% is.na()) %>%
      suppressWarnings()

    if (return_wide) {
      xml_extra_df <-
        xml_extra_df %>%
        mutate(idXMLItem = idXMLItem - 1,
               item = ifelse(idXMLItem > 0, paste0(item, idXMLItem), item)) %>%
        dplyr::select(-idXMLItem) %>%
        spread(item, value)
    }

    return(xml_extra_df)
  }

#' Parse XML Labels
#'
#' @param data
#' @param id_dateTime
#'
#' @return
#' @import dplyr
#' @import tidyr
#'
#' @examples

get_clean_count_vkg_data <-
  function(all_counts,
           extra_key = NA,
           count_col = 'idImageLabel',
           return_wide = T) {
    if (!extra_key %>% is.na()) {
      clean_data <-
        all_counts %>%
        dplyr::rename_(count = count_col,
                       ek = extra_key) %>%
        group_by(idVGKG) %>%
        mutate(count = 1:n()) %>%
        ungroup %>%
        mutate(count = count - 1) %>%
        gather(item, value, -c(idVGKG, count, ek), na.rm = T) %>%
        mutate(item = ifelse(count == 0, item, item %>% paste0(count)),
               value = value %>% str_trim) %>%
        separate(
          idVGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = F
        ) %>%
        mutate(dateTime = dateTime %>% as.numeric) %>%
        arrange(dateTime, count) %>%
        dplyr::select(-c(dateTime, GKG))
    } else {
      clean_data <-
        all_counts %>%
        dplyr::rename_(count = count_col) %>%
        group_by(idVGKG) %>%
        mutate(count = 1:n()) %>%
        ungroup %>%
        mutate(count = count - 1) %>%
        gather(item, value, -c(idVGKG, count), na.rm = T) %>%
        mutate(item = ifelse(count == 0, item, item %>% paste0(count)),
               value = value %>% str_trim) %>%
        separate(
          idVGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = F
        ) %>%
        mutate(dateTime = dateTime %>% as.numeric) %>%
        arrange(dateTime, count) %>%
        dplyr::select(-c(dateTime, GKG))

    }

    if (return_wide) {
      if (!extra_key %>% is.na()) {
        names_order <-
          c('idVGKG', 'ek',  clean_data$item %>% unique)
        clean_data <-
          clean_data %>%
          dplyr::select(-count)
      } else {
        names_order <-
          c('idVGKG',  clean_data$item %>% unique)
        clean_data <-
          clean_data %>%
          dplyr::select(-count) %>%
          spread(item, value)
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
        clean_data %>%
        dplyr::select(dplyr::matches(keywords %>% paste0(collapse = '|'))) %>%
        dplyr::select(-dplyr::matches('namePerson|idCountry')) %>%
        names %>% length

      if (length_nums > 0) {
        numeric_vars <-
          clean_data %>%
          dplyr::select(dplyr::matches(keywords %>% paste0(collapse = '|'))) %>%
          dplyr::select(-dplyr::matches('namePerson|idCountry')) %>%
          names
        clean_data <-
          clean_data %>%
          mutate_at(numeric_vars,
                    funs(as.numeric))

      }

      clean_data <-
        clean_data %>%
        dplyr::select_(.dots = names_order) %>%
        separate(
          idVGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = F
        ) %>%
        mutate(dateTime = dateTime %>% as.numeric) %>%
        arrange(dateTime) %>%
        dplyr::select(-c(GKG, dateTime))

      if (!extra_key %>% is.na()) {
        names(clean_data)[2] <-
          extra_key
      }
    } else {
      clean_data <-
        clean_data %>%
        dplyr::select(-count) %>%
        separate(
          idVGKG,
          into = c('GKG', 'dateTime'),
          sep = '\\-',
          remove = F
        ) %>%
        mutate(dateTime = dateTime %>% as.numeric) %>%
        arrange(dateTime) %>%
        dplyr::select(-c(GKG, dateTime))

    }

    return(clean_data)
  }

parse_xml_labels <-
  function(data, id_vgkg = 1)
  {
    xmlData <-
      data %>%
      dplyr::filter(idVGKG == id_vgkg) %>%
      .$xmlLabels

    if (xmlData %>% is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      if (xmlData %>% str_detect('<RECORD>')) {
        xmlData <-
          xmlData %>%
          str_split('<RECORD>') %>%
          flatten_chr()
      }


      xml_df <-
        tibble(xmlData) %>%
        separate(
          xmlData,
          into = c('nameLabel', 'scoreConfidenceLabel', 'midGoogle'),
          sep = '<FIELD>'
        ) %>%
        mutate(
          idVGKG = id_vgkg,
          idImageLabel = 1:n(),
          scoreConfidenceLabel = scoreConfidenceLabel %>% as.numeric()
        ) %>%
        dplyr::select(idVGKG, idImageLabel, everything()) %>%
        suppressWarnings()
    }

    return(xml_df)

  }

#' Parses Cloud Vision Lables
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#' @import purrr
#' @examples
parse_vgkg_labels <- function(gdelt_data,
                              filter_na = T,
                              return_wide = T) {
  parse_xml_labels_safe <-
    purrr::possibly(parse_xml_labels, tibble())

  allxmlLabels <-
    gdelt_data$idVGKG %>%
    future_map_dfr(function(x) {
      parse_xml_labels_safe(data = gdelt_data, id_vgkg = x)
    }) %>%
    suppressWarnings()

  allxmlLabels <-
    allxmlLabels %>%
    get_clean_count_vkg_data(count_col = 'idImageLabel', return_wide = return_wide)

  if (!return_wide) {
    allxmlLabels <-
      allxmlLabels %>%
      .resolve_long_names()
  }

  return(allxmlLabels)
}


#' Parses XML Landmarks
#'
#' @param data
#' @param id_vgkg
#'
#' @return
#'
#' @examples
parse_xml_landmarks <-
  function(data, id_vgkg = "20160610194500-2247") {
    xmlData <-
      data %>%
      dplyr::filter(idVGKG == id_vgkg) %>%
      .$xmlGeoLandmarks

    if (xmlData %>% is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      if (xmlData %>% str_detect('<RECORD>')) {
        xmlData <-
          xmlData %>%
          str_split('<RECORD>') %>%
          flatten_chr()
      }


      xml_df <-
        tibble(xmlData) %>%
        separate(
          xmlData,
          into = c(
            'nameLandmark',
            'scoreConfidenceLandmark',
            'midGoogle',
            'latLonLandmark'
          ),
          sep = '<FIELD>'
        ) %>%
        separate(
          col = 'latLonLandmark',
          sep = '\\,',
          into = c('latitudeLandmark', 'longitudeLandmark')
        ) %>%
        mutate(
          idVGKG = id_vgkg,
          idLandmarkImage = 1:n(),
          scoreConfidenceLandmark = scoreConfidenceLandmark %>% as.numeric(),
          latitudeLandmark = latitudeLandmark %>% as.numeric,
          longitudeLandmark = longitudeLandmark %>% as.numeric
        ) %>%
        dplyr::select(idVGKG, idLandmarkImage, everything()) %>%
        suppressWarnings()
    }
    return(xml_df)

  }

#' Parses Cloud Vision Landmark
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_vgkg_landmarks <- function(gdelt_data,
                                 filter_na = T,
                                 return_wide = T) {
  parse_xml_landmarks_safe <-
    purrr::possibly(parse_xml_landmarks, tibble())

  all_data <-
    gdelt_data$idVGKG %>%
    future_map_dfr(function(x) {
      parse_xml_landmarks_safe(data = gdelt_data, id_vgkg = x)
    }) %>%
    suppressWarnings()

  if (filter_na) {
    all_data <-
      all_data %>%
      dplyr::filter(!nameLandmark %>% is.na)
  }

  all_data <-
    all_data %>%
    get_clean_count_vkg_data(count_col = 'idLandmarkImage', return_wide = return_wide)

  if (!return_wide) {
    all_data <-
      all_data %>%
      .resolve_long_names()
  }
  return(all_data)
}


#' Parses XML Logo
#'
#' @param data
#' @param id_vgkg
#'
#' @return
#'
#' @examples
parse_xml_logos <-
  function(data, id_vgkg = 1) {
    xmlData <-
      data %>%
      dplyr::filter(idVGKG == id_vgkg) %>%
      .$xmlLogos

    if (xmlData %>% is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      xmlData <-
        xmlData %>%
        str_split('<RECORD>') %>%
        flatten_chr()

      xml_df <-
        tibble(xmlData) %>%
        separate(
          xmlData,
          into = c('nameLogo', 'scoreConfidenceLogo', 'midGoogle'),
          sep = '<FIELD>'
        ) %>%
        mutate(
          idVGKG = id_vgkg,
          idLogoImage = 1:n(),
          scoreConfidenceLogo = scoreConfidenceLogo %>% as.numeric()
        ) %>%
        dplyr::select(idVGKG, idLogoImage, everything()) %>%
        suppressWarnings()
    }

    return(xml_df)

  }

#' Parses Cloud Vision Logos
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_vgkg_logos <- function(gdelt_data,
                             filter_na = T,
                             return_wide = T) {
  parse_xml_logos_safe <-
    purrr::possibly(parse_xml_logos,tibble())

  all_data <-
    gdelt_data$idVGKG %>%
    future_map_dfr(function(x) {
      parse_xml_logos_safe(data = gdelt_data, id_vgkg = x)
    }) %>%
    suppressWarnings()

  all_data <-
    all_data %>%
    mutate(midGoogle = ifelse(midGoogle == '', NA, midGoogle))

  if (filter_na) {
    all_data <-
      all_data %>%
      dplyr::filter(!nameLogo %>% is.na())
  }

  all_data <-
    all_data %>%
    get_clean_count_vkg_data(count_col = 'idLogoImage', return_wide = return_wide)

  if (!return_wide) {
    all_data <-
      all_data %>%
      .resolve_long_names() %>%
      suppressWarnings()
  }

  return(all_data)
}

#' Parses Safe Search
#'
#' @param data
#' @param id_vgkg
#'
#' @return
#'
#' @examples
parse_xml_safe_search <-
  function(data, id_vgkg = 1) {
    xmlData <-
      data %>%
      dplyr::filter(idVGKG == id_vgkg) %>%
      .$xmlSafeSearch

    if (xmlData %>% is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      xmlData <-
        xmlData %>%
        str_split('<RECORD>') %>%
        flatten_chr()

      xml_df <-
        tibble(xmlData) %>%
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
        xml_df %>%
        mutate_at(xml_df %>% dplyr::select(dplyr::matches("score")) %>% names(),
                     funs(as.integer)) %>%
        mutate(idVGKG = id_vgkg,
               idSafeSearchImage = 1:n()) %>%
        dplyr::select(idVGKG, idSafeSearchImage, everything()) %>%
        suppressWarnings() %>%
        distinct()
    }

    return(xml_df)

  }

#' Parses Safe Search
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_vgkg_safe_search <- function(gdelt_data,
                                   filter_na = T,
                                   return_wide = T) {
  parse_xml_safe_search_safe <-
    purrr::possibly(parse_xml_safe_search, tibble())

  all_data <-
    gdelt_data$idVGKG %>%
    future_map_dfr(function(x) {
      parse_xml_safe_search_safe(data = gdelt_data, id_vgkg = x)
    }) %>%
    suppressWarnings()

  if (filter_na) {
    all_data <-
      all_data %>%
      dplyr::filter(!idSafeSearchImage %>% is.na)
  }

  if (!return_wide) {
    all_data <-
      all_data %>%
      .resolve_long_names()
  }

  return(all_data)
}

#' Parses XML Faces
#'
#' @param data
#' @param id_vgkg
#'
#' @return
#'
#' @examples
parse_xml_faces <-
  function(data, id_vgkg = 1) {
    xmlData <-
      data %>%
      dplyr::filter(idVGKG == id_vgkg) %>%
      .$xmlFaces

    if (xmlData %>% is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      xmlData <-
        xmlData %>%
        str_split('<RECORD>') %>%
        flatten_chr()

      xml_df <-
        tibble(xmlData) %>%
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
        ) %>%
        separate('boxBounding', sep = ';', c('xy1', 'xy2', 'xy3', 'xy4')) %>%
        separate('xy1', sep = ',', c('faceX1', 'faceY1')) %>%
        separate('xy2', sep = ',', c('faceX2', 'faceY2')) %>%
        separate('xy3', sep = ',', c('faceX3', 'faceY3')) %>%
        separate('xy4', sep = ',', c('faceX4', 'faceY4'))

      xml_df <-
        xml_df %>%
        mutate_at(.vars =
                    xml_df %>% dplyr::select(dplyr::matches("score|face|angle")) %>% names(),
                  .funs = as.numeric) %>%
        mutate(idVGKG = id_vgkg,
               idFace = 1:n()) %>%
        dplyr::select(idVGKG, idFace, everything()) %>%
        suppressWarnings() %>%
        distinct()
    }

    return(xml_df)

  }

#' Parses Cloud Vision Faces
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_vgkg_faces <- function(gdelt_data,
                             filter_na = T,
                             return_wide = T) {
  parse_xml_faces_search_safe <-
    purrr::possibly(parse_xml_faces, tibble())

  all_data <-
    gdelt_data$idVGKG %>%
    future_map_dfr(function(x) {
      parse_xml_faces_search_safe(data = gdelt_data, id_vgkg = x)
    }) %>%
    suppressWarnings()

  if (filter_na) {
    all_data <-
      all_data %>%
      dplyr::filter(!scoreDetectionConfidence %>% is.na())
  }

  all_data <-
    all_data %>%
    get_clean_count_vkg_data(count_col = 'idFace', return_wide = return_wide)

  if (!return_wide) {
    all_data <-
      all_data %>%
      .resolve_long_names()
  }

  return(all_data)
}

#' Parses XML OCR
#'
#' @param data
#' @param id_vgkg
#'
#' @return
#'
#' @examples
parse_xml_ocr <-
  function(data, id_vgkg = 1)  {
    xmlData <-
      data %>%
      dplyr::filter(idVGKG == id_vgkg) %>%
      .$xmlOCR

    if (xmlData %>% is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      xmlData <-
        xmlData %>%
        str_split('<RECORD>') %>%
        flatten_chr() %>%
        gsub('\\n', '', .)

      xml_df <-
        tibble(textOCR = xmlData)

      xml_df <-
        xml_df %>%
        mutate(idVGKG = id_vgkg,
               idItemOCR = 1:n()) %>%
        dplyr::select(idVGKG, idItemOCR, everything()) %>%
        suppressWarnings() %>%
        distinct()
    }
    return(xml_df)

  }

#' Parses Cloud Vision OCR
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_vgkg_ocr <- function(gdelt_data,
                           filter_na = T,
                           return_wide = T) {
  parse_xml_ocr_safe <-
    purrr::possibly(parse_xml_ocr, tibble())

  all_data <-
    gdelt_data$idVGKG %>%
    future_map_dfr(function(x) {
      parse_xml_ocr_safe(data = gdelt_data, id_vgkg = x)
    }) %>%
    suppressWarnings()

  if (filter_na) {
    all_data <-
      all_data %>%
      dplyr::filter(!idItemOCR %>% is.na())
  }

  all_data <-
    all_data %>%
    get_clean_count_vkg_data(count_col = 'idItemOCR', return_wide = return_wide)

  if (!return_wide) {
    all_data <-
      all_data %>%
      .resolve_long_names()
  }

  return(all_data)
}


#' Parses XML Language Type
#'
#' @param data
#' @param id_vgkg
#'
#' @return
#'
#' @examples
parse_language_types <-
  function(data, id_vgkg = 1)  {
    xmlData <-
      data %>%
      dplyr::filter(idVGKG == id_vgkg) %>%
      .$codesLanguages

    if (xmlData %>% is.na()) {
      xml_df <-
        tibble(idVGKG = id_vgkg)
    } else {
      xmlData <-
        xmlData %>%
        str_split('\\,') %>%
        flatten_chr()

      xml_df <-
        tibble(idLanguage = xmlData) %>%
        mutate(idVGKG = id_vgkg,
               idItemLanguage = 1:n()) %>%
        dplyr::select(idVGKG, idItemLanguage, everything()) %>%
        suppressWarnings()
    }

    return(xml_df)

  }

#' Parses Cloud Vision Languages
#'
#' @param gdelt_data
#' @param filter_na
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
parse_vgkg_languages <- function(gdelt_data,
                                 filter_na = T,
                                 return_wide = T) {
  parse_language_types_safe <-
    purrr::possibly(parse_language_types, tibble())

  all_data <-
    gdelt_data$idVGKG %>%
    future_map_dfr(function(x) {
      parse_language_types_safe(data = gdelt_data, id_vgkg = x)
    }) %>%
    suppressWarnings()

  if (filter_na) {
    all_data <-
      all_data %>%
      dplyr::filter(!idItemLanguage %>% is.na())
  }

  all_data <-
    all_data %>%
    get_clean_count_vkg_data(count_col = 'idItemLanguage', return_wide = return_wide)

  if (!return_wide) {
    all_data <-
      all_data %>%
      .resolve_long_names()
  }

  return(all_data)
}


#' Gets most recent GKG TV log URLs
#'
#' @return
#' @import stringr
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @importFrom tidyr separate
#' @export
#'
#' @examples get_urls_gkg_most_recent_log()
get_urls_gkgtv_most_recent_log <- function() {
  urlData <-
    'http://data.gdeltproject.org/gdeltv2_iatelevision/lastupdate.txt' %>%
    readr::read_tsv(col_names = F) %>%
    dplyr::select(-1) %>%
    suppressMessages()

  names(urlData) <-
    c('idHash', 'urlData')

  urlData <-
    urlData %>%
    mutate(
      dateData = urlData %>% str_replace_all(
        'http://data.gdeltproject.org/gdeltv2_iatelevision/|.gkg.csv.gz',
        ''
      ) %>% as.Date('%Y%m%d')
    ) %>%
    dplyr::select(dateData, everything())

  return(urlData)
}

#' Retrieves GKG TV Schema
#'
#' @return
#' @importFrom dplyr tibble
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
#' @param remove_count_files
#' @param return_message
#'
#' @return
#' @export
#' @import stringr
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
#' @importFrom lubridate ymd
#' @importFrom tidyr separate
#' @examples
#' get_urls_gkg_daily_summaries(remove_count_files = T)
get_urls_gkg_tv_daily_summaries <-
  function(return_message = T) {
    url <-
      'http://data.gdeltproject.org/gdeltv2_iatelevision/masterfilelist.txt'

    urlData <-
      url %>%
      readr::read_tsv(col_names = F) %>%
      dplyr::select(-1) %>%
      suppressMessages()

    names(urlData) <-
      c('idHash', 'urlData')

    urlData <-
      urlData %>%
      mutate(
        dateData = urlData %>% str_replace_all(
          'http://data.gdeltproject.org/gdeltv2_iatelevision/|.gkg.csv.gz',
          ''
        ) %>% as.Date('%Y%m%d')
      ) %>%
      dplyr::select(dateData, everything())

    if (return_message) {
      count.files <-
        urlData %>%
        nrow()

      min.date <-
        urlData$dateData %>% min(na.rm = T)

      max.date <-
        urlData$dateData %>% max(na.rm = T)

      "You got " %>%
        paste0(count.files,
               ' GDELT TV Global Knowledge Graph URLS from ',
               min.date,
               ' to ',
               max.date) %>%
        cat(fill = T)
    }

    return(urlData)
  }

.get_data_gkg_tv <-
  function(url = 'http://data.gdeltproject.org/gdeltv2_iatelevision/20160609.gkg.csv.gz',
           return_message = T) {
    ok_url <-
      url %>% httr::url_ok() %>% suppressWarnings()
    if (ok_url == FALSE) {
      stop("Invalid url")
    }

    gkg_tv_data <-
      url %>%
      curl::curl() %>%
      gzcon() %>%
      read_tsv(col_names = F) %>%
      suppressWarnings() %>%
      suppressMessages()

    names(gkg_tv_data) <-
      get_tv_schema() %>% .$nameActual

    gkg_tv_data <-
      gkg_tv_data %>%
      dplyr::mutate(
        idSourceCollectionIdentifier = idSourceCollectionIdentifier %>% as.numeric(),
        isDocumentURL = ifelse(documentSource %>% str_detect('http'), T, F)
      ) %>%
      dplyr::select(idGKG:idSourceCollectionIdentifier,
                    isDocumentURL,
                    everything()) %>%
      dplyr::rename(dateTimeDocument = dateDocument) %>%
      dplyr::mutate(dateTimeDocument = dateTimeDocument %>% lubridate::ymd_hms() %>% with_tz(Sys.timezone())) %>%
      suppressMessages() %>%
      suppressWarnings()

    gkg_tv_data <-
      gkg_tv_data %>%
      mutate(docDetails = documentSource %>% sub('\\_', '\\-', .) %>% sub('\\_', '\\-', .)  %>% sub('\\_', '\\-', .)) %>%
      separate(
        docDetails,
        sep = '\\-',
        into = c('idTVNetwork', 'date', 'time', 'nameTVShow')
      ) %>%
      dplyr::select(-c(date, time)) %>%
      mutate(
        nameTVShow = nameTVShow %>% str_replace_all('\\_', ' '),
        urlArchiveVideo = documentSource %>% paste0('https://archive.org/details/', .)
      ) %>%
      dplyr::select(idGKG:documentSource,
                    urlArchiveVideo,
                    idTVNetwork,
                    nameTVShow,
                    everything())

    if (return_message) {
      "Downloaded, parsed and imported " %>%
        paste0(url) %>%
        cat(fill = T)

    }
    return(gkg_tv_data)
  }

.get_data_gkg_tv_day <- function(date_data = "2016-06-01",
                                only_most_recent = F,
                                return_message = T) {
  if (only_most_recent) {
    date_data <-
      Sys.Date() - 2
  }

  if (!date_data %>% substr(5, 5) == "-") {
    stop("Sorry data must be in YMD format, ie, 2016-06-01")
  }

  date_data <-
    date_data %>%
    ymd %>% as.Date()


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
      gkg_recent %>%
      .$urlData
  } else {
    if (!'gkg_tv_urls' %>% exists()) {
      gkg_tv_urls <-
        get_urls_gkg_tv_daily_summaries()

      assign(x = 'gkg_tv_urls', eval(gkg_tv_urls), env = .GlobalEnv)
    }

    urls <-
      gkg_tv_urls %>%
      dplyr::filter(dateData == date_data) %>%
      .$urlData
  }

  get_data_gkg_tv_safe <-
    purrr::possibly(.get_data_gkg_tv, tibble())

  all_data <-
    urls %>%
    future_map_dfr(function(x) {
      get_data_gkg_tv_safe(url = x,
                      return_message = return_message)
    }) %>%
    distinct() %>%
    suppressMessages() %>%
    suppressWarnings()

  if (return_message) {
    "You retrieved " %>%
      paste0(all_data %>% nrow, " gkg tv events for ", date_data) %>%
      cat(fill = T)
  }

  return(all_data)
}


#' Gets GKG TV data for specified days
#'
#' @param dates specified dates, year, month day format
#' @param only_most_recent returns only the most recent period
#' \code{c(T, F)}
#' @param return_message
#'
#' @return
#' @export
#' @import purrr
#' @importFrom furrr future_map_dfr
#' @examples
get_data_gkg_tv_days <-
  function(dates = c("2016-06-01", "2016-02-01"),
           only_most_recent = F,
           return_message = T) {
    get_data_gkg_tv_day_safe <-
      purrr::possibly(.get_data_gkg_tv_day, tibble())

    all_data <-
      dates %>%
      future_map_dfr(
        function(x)
          get_data_gkg_tv_day_safe(
            date_data = x,
            only_most_recent = only_most_recent,
            return_message = return_message
          )
      ) %>%
      suppressWarnings()

    return(all_data)
  }


#' Convert date columns to numeric
#'
#' @param data
#' @param column_keyword Keyword of the data frame's columns to look for that signifies dates
#' \code{c('date')}
#' @param exclude_col_type The types of column to exclude from converting to numeric
#' \code{c('character')}
#' @param return_message  return a message
#' @importFrom tibble rownames_to_column
#' @return
#' @export
#'
#' @examples
date_columns_to_numeric <-
  function(data,
           column_keyword = 'date',
           exclude_col_type = c('character'),
           return_message = T) {
    name_df <-
      data %>%
      dplyr::select(dplyr::matches(column_keyword)) %>%
      future_map(class) %>%
      unlist() %>%
      data.frame(col_class = .) %>%
      tibble::rownames_to_column()

    exclude_cols <-
      name_df %>%
      dplyr::filter(col_class %in% col_type) %>%
      .$rowname

    mutate_cols <-
      data %>%
      dplyr::select(dplyr::matches(column_keyword)) %>%
      dplyr::select(-one_of(exclude_cols)) %>% names

    data <-
      data %>%
      mutate_at(mutate_cols, as.numeric)

    if (return_message) {
      "You convereted " %>%
        paste0(paste0(mutate_cols, collapse = ', '), ' to numeric') %>%
        cat(fill = T)
    }

    return(data)

  }


# trelliscope -------------------------------------------------------------
check_for_trelliscope_js <-
  function() {
    missing <-
      installed.packages() %>% dplyr::as_tibble() %>%
      dplyr::filter(Package == 'trelliscopejs') %>%
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
    if (!random_domains %>% purrr::is_null()) {
      randoms <-
        data$domainSource %>% unique() %>% sample(random_domains)
      search_domains <-
        c(search_domains, randoms) %>% unique()
    }

    if (!domains %>% purrr::is_null()) {
      search_domains <-
        c(search_domains, domains)
    }

    search_domains <-
      search_domains %>%
      unique() %>%
      paste(collapse = '|')

    data <-
      data %>%
      filter(domainSource %>% str_detect(search_domains))

    if (only_pictures) {
      data <-
        data %>%
        filter(!urlImage %>% is.na())
    }

    return(data)
  }

#' Visualize Cloudvision trelliscope
#'
#' @param data
#' @param extra_columns
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
#' @param domains
#' @param random_domains
#' @param only_pictures
#'
#' @return
#' @export
#' @import dplyr purrr tidyr trelliscopejs stringr
#' @examples
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

    if (!extra_columns %>% purrr::is_null()) {
      df_extra <-
        data %>%
        dplyr::select(idVGKG, one_of(extra_columns))
    }
    has_domains <-
      !domains %>% purrr::is_null() |
      !random_domains %>% purrr::is_null()
    if (has_domains) {
      data <-
        data %>%
        .filter_domain_data(domains = domains, random_domains = random_domains)
    }
    if (!vgkg_parse %>% purrr::is_null()) {
      vgkg_options <-
        c('faces',
          'labels',
          'landmarks',
          'languages',
          'logos',
          'ocr')
      vgkg_parse <-
        vgkg_parse %>% str_to_lower()
      if (!vgkg_parse %in% vgkg_options) {
        stop(list(
          "Parse options can only be:\n",
          paste(vgkg_options, collapse = '\n')
        ) %>% purrr::reduce(paste0))
      }

      is_faces <-
        vgkg_parse == 'faces'

      if (is_faces) {
        data <-
          data %>%
          parse_vgkg_faces(filter_na = TRUE, return_wide = TRUE) %>%
          left_join(data %>% select(idVGKG:urlImage, dateDocument)) %>%
          suppressMessages() %>%
          suppressWarnings()

        main_name <-
          c('scoreDetectionConfidence',
            'scoreEmotionAngerLikelihood')
      }

      is_labels <-
        vgkg_parse == 'labels'

      if (is_labels) {
        data <-
          data %>%
          parse_vgkg_labels(filter_na = TRUE, return_wide = FALSE) %>%
          left_join(data %>% select(idVGKG:urlImage, dateDocument)) %>%
          suppressMessages() %>%
          suppressWarnings()

        main_name <-
          c('nameLabel', 'scoreConfidenceLabel')
      }

      is_landmark <-
        vgkg_parse == 'landmarks'

      if (is_landmark) {
        data <-
          data %>%
          parse_vgkg_landmarks(filter_na = TRUE, return_wide = FALSE) %>%
          left_join(data %>% select(idVGKG:urlImage, dateDocument)) %>%
          suppressMessages() %>%
          suppressWarnings()

        main_name <-
          c('nameLandmark', 'scoreConfidenceLandmark')
      }

      is_language <-
        vgkg_parse == 'languages'

      if (is_language) {
        data <-
          data %>%
          parse_vgkg_languages(filter_na = TRUE, return_wide = FALSE) %>%
          left_join(data %>% select(idVGKG:urlImage, dateDocument)) %>%
          suppressMessages() %>%
          suppressWarnings()
        main_name <-
          c('idLanguage')
      }

      is_ocr <-
        vgkg_parse == 'ocr'

      if (is_ocr) {
        data <-
          data %>%
          parse_vgkg_ocr(filter_na = TRUE, return_wide = FALSE) %>%
          left_join(data %>% select(idVGKG:urlImage, dateDocument)) %>%
          suppressMessages() %>%
          suppressWarnings()
        main_name <-
          c('textOCR')
      }

      is_logos <-
        vgkg_parse == 'logos'

      if (is_logos) {
        data <-
          data %>%
          parse_vgkg_logos(filter_na = TRUE, return_wide = FALSE) %>%
          left_join(data %>% select(idVGKG:urlImage, dateDocument)) %>%
          suppressMessages() %>%
          suppressWarnings()
        main_name <-
          c('nameLogo')
      }

    } else {
      main_name <-
        ''
    }

    if (!vgkg_parse %>% purrr::is_null()) {
      parse_title <-
        vgkg_parse %>% str_to_title()
    } else {
      parse_title <- ''
    }

    dates <-
      data$dateTimeDocument %>% as.Date("%Y-%m-%d") %>% unique() %>%
      suppressWarnings()

    dates <-
      dates[!dates %>% is.na()] %>% .[[1]]


    title <-
      list('GDELT VGKG Explorer for ',  parse_title, ' ', dates) %>%
      purrr::reduce(paste0)


    if (!extra_columns %>% purrr::is_null()) {
      data <-
        data %>%
        left_join(df_extra) %>%
        suppressMessages()
    }

    data <-
      data %>%
      mutate_if(is.logical,
                funs(ifelse(. %>% is.na(), FALSE, .)))

    if (!trelliscope_parameters$title %>% purrr::is_null()) {
      title <-
        trelliscope_parameters$title
    }

    if (trelliscope_parameters$rows %>% purrr::is_null()) {
      rows <- 1
    } else {
      rows <-
        trelliscope_parameters$rows
    }

    if (trelliscope_parameters$columns %>% purrr::is_null()) {
      columns <- 2
    } else {
      columns <-
        trelliscope_parameters$columns
    }
    no_path <-
      trelliscope_parameters$path %>% purrr::is_null()

    if (no_path) {
      viz <-
        data %>%
        dplyr::select(which(colMeans(is.na(.)) < .5)) %>%
        mutate(
          panel = trelliscopejs::img_panel(urlImage),
          urlArticle = trelliscopejs::cog_href(documentSource),
          idItem = 1:n()
        ) %>%
        select(idItem, idVGKG, everything()) %>%
        arrange(idItem) %>%
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
        data %>%
        dplyr::select(which(colMeans(is.na(.)) < .5)) %>%
        mutate(
          panel = trelliscopejs::img_panel(urlImage),
          urlArticle = trelliscopejs::cog_href(documentSource),
          idItem = 1:n()
        ) %>%
        select(idItem, idVGKG, everything()) %>%
        arrange(idItem) %>%
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
#' @param data
#' @param domains
#' @param trelliscope_parameters \itemize{
#' \item title: Trelliscope title
#' \item rows: Trelliscope rows
#' \item columns: Trelliscope columns
#' \item path: file path to save trelliscope, if NULL no where
#' }
#' @param extra_columns
#' @param random_domains
#' @param only_pictures
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
#' @return
#' @export
#' @import dplyr purrr tidyr trelliscopejs stringr
#' @examples
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
      !domains %>% purrr::is_null() |
      !random_domains %>% purrr::is_null()
    if (has_domains) {
      data <-
        data %>%
        .filter_domain_data(domains = domains, random_domains = random_domains)
    }

    if (!gkg_parse %>% purrr::is_null()) {
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
        gkg_parse %>% str_to_lower()
      if (!gkg_parse %in% gkg_options) {
        stop(list(
          "Parse options can only be:\n",
          paste(gkg_options, collapse = '\n')
        ) %>% purrr::reduce(paste0))
      }

      is_tone <-
        gkg_parse == 'tone'

      if (is_tone) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_article_tone(filter_na = TRUE, return_wide = FALSE) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('scoreTone')
      }

      is_xml <-
        gkg_parse == 'xml'

      if (is_xml) {
        plot_data <-
          data %>%
          filter(!xmlExtras %>% is.na()) %>%
          parse_xml_extras(return_wide = FALSE) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('item', 'value')
      }

      is_date <-
        gkg_parse == 'date'

      if (is_date) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_dates(filter_na = TRUE, return_wide = FALSE) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('dateResolution')
      }

      is_events <-
        gkg_parse == 'events'

      if (is_events) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_event_counts(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(get_codes_gkg_themes()) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
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
          data %>%
          parse_gkg_mentioned_gcams(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(get_codes_gcam()) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('idGCAM', 'dimensionHumanName')
      }

      is_locations <-
        gkg_parse == 'locations'

      if (is_locations) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_locations(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(get_codes_stability_locations()) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('idADM1Code', 'location')
      }

      is_names <-
        gkg_parse == 'names'

      if (is_names) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_names(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('nameMentionedName')
      }

      is_numerics <-
        gkg_parse == 'numerics'

      if (is_numerics) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_numerics(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('amountTerm', 'amountValue')
      }

      is_organizations <-
        gkg_parse == 'organizations'

      if (is_organizations) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_organizations(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('nameOrganization')
      }

      is_people <-
        gkg_parse == 'people'

      if (is_people) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_people(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('namePerson')
      }

      is_quotes <-
        gkg_parse == 'quotes'

      if (is_quotes) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_quotes(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('lengthQuote', 'textQuote')
      }

      is_social <-
        gkg_parse == 'social'

      if (is_social) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_social_embeds() %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('domainSocialMediaImageEmbed')
      }

      is_themes <-
        gkg_parse == 'themes'

      if (is_themes) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_themes(return_wide = F) %>%
          left_join(get_codes_gkg_themes() %>% dplyr::rename(codeTheme = codeGKGTheme)) %>%
          left_join(data %>% select(idGKG:domainSource, urlImage)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('codeTheme')
      }

    } else {
      main_name <-
        ''
    }

    if (!gkg_parse %>% purrr::is_null()) {
      parse_title <-
        gkg_parse %>% str_to_title()
    } else {
      parse_title <- ''
    }
    dates <-
      data$dateTimeDocument %>% as.Date("%Y-%m-%d") %>% unique()

    dates <-
      dates[!dates %>% is.na()] %>% .[[1]]

    title <-
      list('GDELT GKG Explorer for ',  parse_title, ' ', dates) %>%
      purrr::reduce(paste0) %>% str_trim()

    if (!extra_columns %>% purrr::is_null()) {
      plot_data <-
        plot_data %>%
        left_join(
          data %>% dplyr::select(one_of(c('idGKG', extra_columns)))
        ) %>%
        suppressMessages()
    }

    plot_data <-
      plot_data %>%
      mutate_if(is.logical,
                funs(ifelse(. %>% is.na(), FALSE, .)))

    if (!trelliscope_parameters$title %>% purrr::is_null()) {
      title <-
        trelliscope_parameters$title
    }

    if (trelliscope_parameters$rows %>% purrr::is_null()) {
      rows <- 1
    } else {
      rows <-
        trelliscope_parameters$rows
    }

    if (trelliscope_parameters$columns %>% purrr::is_null()) {
      columns <- 2
    } else {
      columns <-
        trelliscope_parameters$columns
    }
    no_path <-
      trelliscope_parameters$path %>% purrr::is_null()

    if (no_path) {
    viz <-
      plot_data %>%
      mutate(
        panel = trelliscopejs::img_panel(urlImage),
        urlArticle = trelliscopejs::cog_href(documentSource),
        idItem = 1:n()
      ) %>%
      select(idItem, idGKG, everything()) %>%
      arrange(idItem) %>%
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
        plot_data %>%
        mutate(
          panel = trelliscopejs::img_panel(urlImage),
          urlArticle = trelliscopejs::cog_href(documentSource),
          idItem = 1:n()
        ) %>%
        select(idItem, idGKG, everything()) %>%
        arrange(idItem) %>%
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
#' @param data
#' @param domains
#' @param random_domains
#' @param only_pictures
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
#' @return
#' @export
#' @import dplyr purrr tidyr trelliscopejs stringr
#' @examples
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
      !random_stations %>% purrr::is_null()
    if (has_random_stations) {
      random_tv <-
        data$nameSource %>%
        unique() %>%
        sample(random_stations)

      station_search <-
        c(station_search, random_tv)
    }

    if (!stations %>% purrr::is_null()) {
      station_search <-
        c(station_search, stations) %>%
        stringr::str_to_upper()
    }

    station_search <-
      station_search %>% stringr::str_to_upper() %>%
      unique() %>% paste0(collapse = '|')

    data <-
      data %>%
      filter(nameSource %>% str_detect(station_search))

    if (!gkg_parse %>% purrr::is_null()) {
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
        gkg_parse %>% str_to_lower()
      if (!gkg_parse %in% gkg_options) {
        stop(list(
          "Parse options can only be:\n",
          paste(gkg_options, collapse = '\n')
        ) %>% purrr::reduce(paste0))
      }

      is_tone <-
        gkg_parse == 'tone'

      if (is_tone) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_article_tone(filter_na = TRUE, return_wide = FALSE) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('scoreTone')
      }

      is_xml <-
        gkg_parse == 'xml'

      if (is_xml) {
        plot_data <-
          data %>%
          filter(!xmlExtras %>% is.na()) %>%
          parse_xml_extras(return_wide = FALSE) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('item', 'value')
      }

      is_date <-
        gkg_parse == 'date'

      if (is_date) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_dates(filter_na = TRUE, return_wide = FALSE) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('dateResolution')
      }

      is_events <-
        gkg_parse == 'events'

      if (is_events) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_event_counts(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(get_codes_gkg_themes()) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
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
          data %>%
          parse_gkg_mentioned_gcams(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(get_codes_gcam()) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('idGCAM', 'dimensionHumanName')
      }

      is_locations <-
        gkg_parse == 'locations'

      if (is_locations) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_locations(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(get_codes_stability_locations()) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('idADM1Code', 'location')
      }

      is_names <-
        gkg_parse == 'names'

      if (is_names) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_names(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('nameMentionedName')
      }

      is_numerics <-
        gkg_parse == 'numerics'

      if (is_numerics) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_numerics(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('amountTerm', 'amountValue')
      }

      is_organizations <-
        gkg_parse == 'organizations'

      if (is_organizations) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_organizations(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('nameOrganization')
      }

      is_people <-
        gkg_parse == 'people'

      if (is_people) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_people(people_column = 'personsCharLoc', return_wide = FALSE, filter_na = TRUE) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('namePerson')
      }

      is_quotes <-
        gkg_parse == 'quotes'

      if (is_quotes) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_quotes(return_wide = FALSE, filter_na = TRUE) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('lengthQuote', 'textQuote')
      }

      is_social <-
        gkg_parse == 'social'

      if (is_social) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_social_embeds() %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('domainSocialMediaImageEmbed')
      }

      is_themes <-
        gkg_parse == 'themes'

      if (is_themes) {
        plot_data <-
          data %>%
          parse_gkg_mentioned_themes(return_wide = F) %>%
          left_join(get_codes_gkg_themes() %>% dplyr::rename(codeTheme = codeGKGTheme)) %>%
          left_join(data %>% select(idGKG:nameTVShow)) %>%
          suppressMessages() %>%
          suppressWarnings() %>%
          select(-c(isDocumentURL, idSourceCollectionIdentifier))

        main_name <-
          c('codeTheme')
      }

    } else {
      main_name <-
        ''
    }

    if (!gkg_parse %>% purrr::is_null()) {
      parse_title <-
        gkg_parse %>% str_to_title()
    } else {
      parse_title <- ''
    }
    dates <-
      data$dateTimeDocument %>% as.Date("%Y-%m-%d") %>% unique()

    dates <-
      dates[!dates %>% is.na()] %>% .[[1]]

    title <-
      list('GDELT GKG TV Explorer for ',  parse_title, ' ', dates) %>%
      purrr::reduce(paste0) %>% str_trim()

    viz <-
      plot_data %>%
      mutate(urlImage = "http://www.novelupdates.com/img/noimagefound.jpg") %>%
      mutate(
        panel = trelliscopejs::img_panel(urlImage),
        urlArchiveVideo = trelliscopejs::cog_href(urlArchiveVideo),
        idItem = 1:n()
      ) %>%
      select(idItem, idGKG, everything()) %>%
      arrange(idItem) %>%
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