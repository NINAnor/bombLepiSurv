#' getExpValues
#'
#' Get expectation communities for the NI indicator calculations from the database.
#'
#'
#' @import dplyr
#'
#' @export
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'


getExpValues <- function(type = c("bumblebees", "butterflies"),
                         region_short = c("trond", "ost", "sor"),
                         habitat = c("gressmark", "skogsmark")){

  type_cat <- match.arg(type, c("bumblebees", "butterflies"))
  region_short_cat <- match.arg(region_short, c("trond", "ost", "sor"))
  habitat_cat <- match.arg(habitat, c("gressmark", "skogsmark"))

  dataRawQ <- "SELECT n.type, e.*
  FROM species.expectation e LEFT JOIN species.names n
  ON n.species_latin = e.species_latin
  "
  dataRaw <- DBI::dbGetQuery(con, dataRawQ)  %>%
    tibble::as_tibble()

  out <- dataRaw %>%
    dplyr::select(-id) %>%
    dplyr::filter(type == type_cat,
           region == region_short_cat,
           habitat == habitat_cat) %>%
    dplyr::select(species = species_latin,
           amount)

  return(out)

}
