#' getAllData
#'
#' Get every record of bumblebees or butterflies from the database
#'
#' @param type What data to get, "Bumblebees" or "Butterflies"
#' @param habitat Get data from what habitat type? Gressmark or Skogsmark
#' @param year which year to get data from. Single year or span of years
#' @param dataConnection name of the database connection
#' @param language What language for the species names? "Latin" or "Norsk"
#'
#' @export



getAllData <- function(type = c("Bumblebees", "Butterflies"),
                       habitat = c("Gressmark", "Skogsmark"),
                       year = 2009:2018,
                       dataConnection = "con",
                       language = "Norsk") {
  language <- match.arg(language, c("Latin", "Norsk"))

  language <- switch(language,
    "Latin" = "species_latin",
    "Norsk" = "species_norsk"
  )

  type_cat <- switch(type,
                     "Bumblebees" = "bumblebees",
                     "Butterflies" = "butterflies"
  )

  habitat_cat <- switch(habitat,
                        "Gressmark" = "gressmark",
                        "Skogsmark" = "skogsmark"
  )

  subset_year = year

  if (type_cat == "bumblebees") {
    dataTab <- dplyr::tbl(get(dataConnection),
                          Id(schema = "views",
                             table = "all_bombus"))
  } else {
    dataTab <- dplyr::tbl(get(dataConnection),
                          Id(schema = "views",
                             table = "all_lepidoptera"))
  }


  dataRaw <- dataTab %>%
    filter(habitat_type == habitat_cat) %>%
    filter(year %in% subset_year)


  out <- dataRaw %>%
    dplyr::select(
      Region = region_short,
      Flate = flate,
      Transekt = flate_transect,
      Habitattype = habitat_type,
      Blomsterdekke = blomsterdekke,
      Year = year,
      Dato = dato,
      Periode = periode,
      language,
      amount
    ) %>%
    mutate(Region = ifelse(Region == "ost", "Øst", Region)) %>%
    mutate(Region = ifelse(Region == "sor", "Sør", Region)) %>%
    mutate(Region = ifelse(Region == "trond", "Trøndelag", Region)) %>%
    mutate(Region = ifelse(Region == "vest", "Vest", Region)) %>%
    mutate(amount = as.integer(amount)) %>%
    collect() %>%
    tidyr::pivot_wider(
      names_from = language,
      values_from = "amount",
      values_fill = 0
    ) %>%
    dplyr::arrange(Region, Flate, Transekt, Year, Periode) %>%
    as_tibble()


  return(out)
}
