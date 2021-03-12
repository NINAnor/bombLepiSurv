#' getComplData
#'
#' Get observation data from complete surveys of butterflies and bumblebees from the database
#'
#' This is suitable for calculating the Nature Index indicators
#'
#'
#' @param type What data to get, "bumblebees" or "butterflies"
#' @param region_short Get data from what region?
#' @param habitat Get data from what habitat type?
#' @param year Get data from what year?
#'
#'
#'
#' @import dplyr
#'
#' @export
#'


getComplData <- function(type = c("Humler", "Sommerfugler"),
                         region_short = c("Trond", "Ost", "Sor"),
                         habitat = c("Gressmark", "Skogsmark"),
                         year = 2010,
                         dataConnection = "con"){

  type <- match.arg(type)
  region_short <- match.arg(region_short)
  habitat <- match.arg(habitat)
  #Temporary as long as database has lowecase values. Could update database but prob breaks something else.
  type_cat <- switch(type,  "Humler" = "bumblebees", "Sommerfugler" = "butterflies")
  region_short_cat <- switch(region_short, "Trond" = "trond", "Ost" = "ost", "Sor"  = "sor")
  habitat_cat <- switch(habitat, "Gressmark" = "gressmark","Skogsmark" = "skogsmark")


  if(type_cat == "bumblebees"){
    source <- "views.compl_bombus_agg"
  } else    source <- "views.compl_lepidoptera_agg"

  dataRawQ <- paste0("SELECT * FROM ", source,
                     "\n WHERE region_short = '", region_short_cat,
                     "' \n AND habitattype = '", habitat_cat,
                     "' \n AND year = ", year)

  dataRaw <- DBI::dbGetQuery(get(dataConnection), dataRawQ)

  out <- dataRaw  %>%
    dplyr::select(Flate = flate,
           Transekt = flate_transect,
           species_latin,
           amount) %>%
    tidyr::spread(key = "species_latin",
           value = "amount") %>%
    dplyr::arrange(Flate, Transekt) %>% as_tibble

  return(out)

}
