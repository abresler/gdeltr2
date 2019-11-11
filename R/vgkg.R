
#' Visual Knowledge Graph label count
#'
#' Counts classified CloudVision labels
#'
#' @return
#' @export
#'
#' @examples
vgkg_label_count <-
  function() {
  data <-
  "http://data.gdeltproject.org/blog/2019-vgkg-entitieslists/20191018-vgkg-labelslist.csv.gz" %>%
    read_csv() %>%
    as_tibble()

  data <-
    data %>%
    mutate(entity = str_to_upper(entity)) %>%
    setNames(c("typeEntity", "midGoogle", "countClassifications"))

  data

}


#' VGKG Classified Entity Count
#'
#' Counts classified entities from 2017
#' to 2019
#'
#' @return
#' @export
#'
#' @examples
vgkg_entity_count <-
  function(){
    data <-
      fread("http://data.gdeltproject.org/blog/2019-vgkg-entitieslists/20191018-vgkg-webentitieslist.csv.gz") %>%
      as_tibble()

    data <-
      data %>%
      mutate(entity = str_to_upper(entity)) %>%
      setNames(c("typeEntity", "midGoogle", "countClassifications"))

    data
  }