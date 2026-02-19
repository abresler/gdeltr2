

#' GDELT Outlets
#'
#' Returns a dictionary of GDELT's tracked outlets
#' including website, logo, icon and English name
#'
#' @return A `tibble` with columns: slugWebsite, nameWebsite, urlWebsite, urlLogo, urlIcon
#' @export
#'
#' @examples
#' dictionary_outlets()
dictionary_outlets <- memoise::memoise(function() {
  tryCatch({
    data <-
      "http://data.gdeltproject.org/blog/2018-news-outlets-domain-info-may2018/MASTER-GDELTDOMAINSINFO-MAY2018.TXT" |>
      read_tsv(col_names = FALSE) |>
      suppressWarnings() |>
      suppressMessages()

    data <-
      data |>
      set_names(c(
        "slugWebsite",
        "nameWebsite",
        "urlWebsite",
        'urlLogo',
        "urlIcon"
      )) |>
      mutate(urlWebsite  = ifelse(urlWebsite |> is.na(), glue("https://{slugWebsite}"), urlWebsite) |> as.character()) |>
      select(slugWebsite, nameWebsite, urlWebsite, everything())

    data
  }, error = function(e) {
    cli::cli_alert_danger("Failed: {e$message}")
    return(tibble())
  })
})
