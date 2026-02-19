
#' Visual Knowledge Graph label count
#'
#' Counts classified CloudVision labels
#'
#' @return A `tibble` with columns: typeEntity, midGoogle, countClassifications
#' @export
#'
#' @examples
#' \dontrun{
#' vgkg_label_count()
#' }
vgkg_label_count <-
  function() {
    tryCatch({
      data <-
        "http://data.gdeltproject.org/blog/2019-vgkg-entitieslists/20191018-vgkg-labelslist.csv.gz" |>
        read_csv() |>
        as_tibble()

      data <-
        data |>
        mutate(entity = str_to_upper(entity)) |>
        setNames(c("typeEntity", "midGoogle", "countClassifications"))

      data
    }, error = function(e) {
      cli::cli_alert_danger("Failed: {e$message}")
      return(tibble())
    })
  }


#' VGKG Classified Entity Count
#'
#' Counts classified entities from 2017
#' to 2019
#'
#' @return A `tibble` with columns: typeEntity, midGoogle, countClassifications
#' @export
#'
#' @examples
#' \dontrun{
#' vgkg_entity_count()
#' }
vgkg_entity_count <-
  function(){
    tryCatch({
      data <-
        fread("http://data.gdeltproject.org/blog/2019-vgkg-entitieslists/20191018-vgkg-webentitieslist.csv.gz") |>
        as_tibble()

      data <-
        data |>
        mutate(entity = str_to_upper(entity)) |>
        setNames(c("typeEntity", "midGoogle", "countClassifications"))

      data
    }, error = function(e) {
      cli::cli_alert_danger("Failed: {e$message}")
      return(tibble())
    })
  }
