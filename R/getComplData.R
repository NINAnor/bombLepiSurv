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


getComplData <- function(type = c("bumblebees", "butterflies"),
                         region_short = c("trond", "ost", "sor"),
                         habitat = c("gressmark", "skogsmark"),
                         year = 2010,
                         dataConnection = "con"){

  if(type == "bumblebees"){
    source <- "views.compl_bombus_agg"
  } else    source <- "views.compl_lepidoptera_agg"

  dataRawQ <- paste0("SELECT * FROM ", source,
                     "\n WHERE region_short = '", region_short,
                     "' \n AND habitattype = '", habitat,
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
