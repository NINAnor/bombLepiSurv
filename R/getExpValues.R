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


getExpValues <- function(type = c("Humler", "Sommerfugler"),
                         region_short = c("Trond", "Ost", "Sor"),
                         habitat = c("Gressmark", "Skogsmark")){

  #Temporary as long as database has lowecase values. Could update database but prob breaks something else.
  type_cat <- switch(type,  "Humler" = "bumblebees", "Sommerfugler" = "butterflies")
  region_short_cat <- switch(region_short, "Trond" = "trond", "Ost" = "ost", "Sor"  = "sor")
  habitat_cat <- switch(habitat, "Gressmark" = "gressmark","Skogsmark" = "skogsmark")

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
    dplyr::select(species_latin,
                  amount)

  return(out)

}
