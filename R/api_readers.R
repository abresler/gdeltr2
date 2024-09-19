
# https://blog.gdeltproject.org/gdelt-2-0-television-api-debuts/


# utils -------------------------------------------------------------------

#' Widen API URLs
#'
#' @param data A `tibbile`
#' @param url_gdelt Name of GDELT API url variables
#' @param widen_variables Vector of variables to unite and widen
#'
#' @return
#' @export
#'
#' @examples
widen_gdelt_url_api_parameters <-
  function(data, url_gdelt = NULL, widen_variables = NULL) {
    if (is.null(url_gdelt)) {
      message("Enter GDELT API URL")
      return(data)
    }
    if (is.null(widen_variables)) {
      message("Enter Widen Variables")
      return(data)
    }
    key_cols <- data |> select(-one_of(url_gdelt)) |> names()
    unite_columns <- c("url_type", widen_variables)
    data <-
      data |> pivot_longer(cols = url_gdelt,
                           names_to = "url_type",
                           values_to  = "url") |>
      unite(url_type, !!!syms(unite_columns)) |>
      spread(url_type, url) |>
      janitor::clean_names()

    data
  }

# dictionary --------------------------------------------------------------

dictionary_gdelt_api_names <-
  function() {
    tibble(
      name_gdelt = c(
        "station_id",
        "description",
        "market",
        "network",
        "start_date",
        "end_date",
        "date",
        "series",
        "value",
        "url",
        "match_date_time",
        "station",
        "show",
        "ia_show_id",
        "ia_preview_thumb",
        "snippet",
        "label",
        "count",
        "est_perc",
        "preview_url",
        "show_date",
        "preview_thumb",

        "url_mobile",
        "title",
        "seendate",
        "socialimage",
        "domain",
        "language",
        "sourcecountry",
        "mobile_url",
        "imageurl",
        "sourcearticleurl",
        "image_url",
        "source_article_url",
        "norm",
        "toparts",
        "top_art_url1",
        "top_art_title1",
        "top_art_url2",
        "top_art_title2",
        "top_art_url3",
        "top_art_title3",
        "top_art_url4",
        "top_art_title4",
        "top_art_url5",
        "top_art_title5",
        "top_art_url6",
        "top_art_title6",
        "top_art_url7",
        "top_art_title7",
        "top_art_url8",
        "top_art_title8",
        "top_art_url9",
        "top_art_title9",
        "top_art_url10",
        "top_art_title10",
        "bin"
      ),
      name_actual = c(
        "idStation",
        "nameStation",
        "marketStation",
        "slugNetwork",
        "datetimeStartData",
        "datetimeEndData",
        "date",
        "id_series",
        "value",
        "url",
        "datetime_match",
        "id_station",
        "name_show",
        "id_show_ia",
        "url_image_video_thumbnail_internet_archive",
        "text_snippet",
        "label",
        "count",
        "pct_estimated_coverage",
        "url_video_internet_archive",
        "datetime_show",
        "url_image_video_thumbnail_internet_archive",
        "url_mobile_link",
        "title_article",
        "datetime_article",
        "url_image",
        "domain_article",
        "language_article",
        "source_country_article",
       "url_mobile_link",
       "url_image",
       "url_source_article",
       "url_image",
       "url_source_article",
       "count_articles_monitored",
       "data_top_articles",

       "url_top_article_001",
       "title_top_article_001",
       "url_top_article_002",
       "title_top_article_002",
       "url_top_article_003",
       "title_top_article_003",
       "url_top_article_004",
       "title_top_article_004",
       "url_top_article_005",
       "title_top_article_005",
       "url_top_article_006",
       "title_top_article_006",
       "url_top_article_007",
       "title_top_article_007",
       "url_top_article_008",
       "title_top_article_008",
       "url_top_article_009",
       "title_top_article_009",
       "url_top_article_010",
       "title_top_article_010",
       "number_bin"
      )
    )
  }

.munge_gdelt_names <-
  function(data, snake_names = FALSE) {
    names_dict <- names(data)

    dict <- dictionary_gdelt_api_names()
    actual_names <-
      names_dict %>%
      map_chr(function(name) {
        df_row <-
          dict %>% filter(name_gdelt == name)
        if (nrow(df_row) == 0) {
          glue("Missing {name}") %>% message()
          return(name)
        }

        df_row$name_actual
      })

    data <- data |>
      setNames(actual_names)

    if (snake_names) {
      data <- data |> janitor::clean_names()
    }

    data
  }


# readers -----------------------------------------------------------------


.read_gdelt_api_json <- function(url = "https://api.gdeltproject.org/api/v2/tv/tv?query=%20%Walz%20%market:%22National%22&mode=ClipGallery&format=json&datacomb=sep&datanorm=percent&maxrecords=3000", url_type = "url_gdelt_tv_api") {
  out <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE, flatten = FALSE)
  actual_tables <- names(out)[names(out) %in% "query_details"]
  actual_tables <- names(out)[!names(out) %in% actual_tables]

  data <-
    out[[actual_tables]] |> as_tibble() |> unnest()

  data <- .munge_gdelt_names(data)

  data <- data |> mutate(UQ(url_type) := url)

  if (data |> hasName("date")) {
    data <- data |>
      mutate(date = lubridate::ymd_hms(date) |>  lubridate::with_tz(Sys.timezone())) |>
      rename(datetime_data = date) |>
      filter(!datetime_data |> str_detect("00:00:00"))
  }

  data
}

.read_gdelt_api_csv <- function(url = "https://api.gdeltproject.org/api/v2/tv/tv?query=%20%25Walz%20%25market:%22National%22&mode=TimelineVol&format=csv&datacomb=sep&datanorm=percent&maxrecords=3000", url_type = "url_gdelt_tv_api") {
  data <-
    data.table::fread(url, verbose = FALSE) |> as_tibble()

  is_doc <- url |> str_detect("doc?")
  is_timeline_vol <-
    url |> str_detect("mode=timeline")
  gdelt_names <- names(data) |>
    map_chr(list(function(x){
      x |> .remove_parentheses() |> str_squish()
    })) |> janitor::make_clean_names()

  names(data) <- gdelt_names

  data <-
    .munge_gdelt_names(data = data, snake_names = FALSE)

  not_timeline_vol <- !url |> str_to_lower() |> str_detect("mode=timelinevolheatmap")
  is_clip <- url |> str_detect("mode=clipgallery")


  if (is_doc & is_timeline_vol) {
    data <- data |>
      rename(datetime_data = date)

    data <- data |> mutate(UQ(url_type) := url)
    return(data)
  }

  if (data |> hasName("date") & not_timeline_vol) {
    data <-
      data |>
      mutate(date = lubridate::my(date)) |>
      rename(date_data = date)
  }

  if (data |> hasName("url") & is_clip) {
    data <-
      data |>
      rename(url_video_internet_archive = url)
  }


  is_timeline_vol <- url |> str_to_lower() |> str_detect("mode=timelinevolheatmap")
  if (is_timeline_vol) {
    data <- data |>
      rename(date_data = date) |>
      mutate(date_data = ymd(date_data))
  }

  data <- data |> mutate(UQ(url_type) := url)

  data
}