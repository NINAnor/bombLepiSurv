#' getTransectWalks
#'
#' Retrieve date and time for all recorded transect walks.
#'
#' @param include_id
#'
#' @return A tibble of transect walks
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' humleSommerfuglConnect
#' humlesommerfConnect()
#'
#' all_transect_walks <- getTransectWalks()
#' }
#'

getTransectWalks <- function(include_id = FALSE){

  transects_raw <- dplyr::tbl(con,
                          DBI::Id(schema = "views",
                             table = "transect_walks_metadata"))

  transects <- transects_raw |>
    dplyr::select(
      id,
      Region = region_short,
      Flate = flate,
      Transekt = flate_transect,
      Habitattype = habitat_type,
      Blomsterdekke = blomsterdekke,
      Year = year,
      Dato = dato,
      Periode = periode)  |>
    dplyr::mutate(Region = ifelse(Region == "ost", "Øst", Region)) |>
    dplyr::mutate(Region = ifelse(Region == "sor", "Sør", Region)) |>
    dplyr::mutate(Region = ifelse(Region == "trond", "Trøndelag", Region)) |>
    dplyr::mutate(Region = ifelse(Region == "vest", "Vest", Region))


  if(!include_id){
    transects <- transects |>
      dplyr::select(-id)
  }

  return(transects)

}
