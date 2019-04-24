#' getAllData
#'
#' Get every record of bumblebees or butterflies from the database
#'
#' @param type bumblebees or butterflies
#' @param habitat gressmark or skogsmark
#' @param year which year to get data from. Single year or span of years
#' @param dataConnection name of the database connection
#'
#'
#' @export



getAllData <- function(type = NULL,
                       habitat = NULL,
                       year = 2009:2018,
                       dataConnection = "con",
                       language = "norsk"){

  language <- match.arg(language, c("latin", "norsk"))

  language <- switch(language,
         "latin" = "species_latin",
         "norsk" = "species_norsk")

  type <- match.arg(type, c("bumblebees", "butterflies"))
  habitat <- match.arg(habitat, c("gressmark", "skogsmark"))

  if(type == "bumblebees"){
    source <- "views.all_bombus"
  } else    source <- "views.all_lepidoptera"

  dataRawQ <- paste0("SELECT * FROM ", source,
                     " \n WHERE habitattype = '", habitat,
                     "'")

  dataRaw <- DBI::dbGetQuery(get(dataConnection), dataRawQ)

  out <- dataRaw  %>%
    dplyr::select(Region = region_short,
                  Flate = flate,
                  Transekt = flate_transect,
                  Habitattype = habitattype,
                  Blomsterdekke = blomsterdekke,
                  Year = year,
                  Dato = dato,
                  Periode = periode,
                  language,
                  amount) %>%
    mutate(Region = ifelse(Region == "ost", "Øst", Region)) %>%
    mutate(Region = ifelse(Region == "sor", "Sør", Region)) %>%
    mutate(Region = ifelse(Region == "trond", "Trøndelag", Region)) %>%
    tidyr::spread(key = language,
                  value = "amount",
                  fill = 0) %>%
    dplyr::arrange(Region, Flate, Transekt, Year, Periode) %>% as_tibble

  out <- out %>%
    filter(Year %in% year)
  return(out)

}


