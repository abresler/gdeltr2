# https://blog.gdeltproject.org/gdelt-2-0-television-api-debuts/

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
        "preview_thumb"
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
        "url_image_video_thumbnail_internet_archive"
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

  gdelt_names <- names(data) |>
    map_chr(list(function(x){
      x |> .remove_parentheses() |> str_squish()
    })) |> janitor::make_clean_names()

  names(data) <- gdelt_names

  data <-
    .munge_gdelt_names(data = data, snake_names = FALSE)

  not_timeline_vol <- !url |> str_to_lower() |> str_detect("mode=timelinevolheatmap")
  is_clip <- url |> str_detect("mode=clipgallery")

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