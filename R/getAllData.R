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
#'
#' @examples
#'
#' #' \dontrun{
#'
#' humleSommerfuglConnect
#' humlesommerfConnect()
#'
#' bumblebees_grass <- getTransectWalks(type = "Bumblebees",
#'                                      habitat = "Gressmark",
#'                                      includeEmptyTransects = TRUE)
#' }


getAllData <- function(type = c("Bumblebees", "Butterflies"),
                       habitat = c("Gressmark", "Skogsmark"),
                       year = 2009:2024,
                       dataConnection = "con",
                       language = "Norsk",
                       includeEmptyTransects = FALSE) {
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
                          DBI::Id(schema = "views",
                             table = "all_bombus"))
  } else {
    dataTab <- dplyr::tbl(get(dataConnection),
                          DBI::Id(schema = "views",
                             table = "all_lepidoptera"))
  }


  dataRaw <- dataTab |>
    dplyr::filter(habitat_type == habitat_cat)  |>
    dplyr::filter(year %in% subset_year)


  temp_out <- dataRaw |>
    dplyr::select(
      Region = region_short,
      Flate = flate,
      Transekt = flate_transect,
      Habitattype = habitat_type,
      Blomsterdekke = blomsterdekke,
      Year = year,
      Dato = dato,
      Periode = periode,
      all_of(language),
      amount) |>
    dplyr::mutate(Region = ifelse(Region == "ost", "Øst", Region)) |>
    dplyr::mutate(Region = ifelse(Region == "sor", "Sør", Region)) |>
    dplyr::mutate(Region = ifelse(Region == "trond", "Trøndelag", Region)) |>
    dplyr::mutate(Region = ifelse(Region == "vest", "Vest", Region)) |>
    dplyr::mutate(amount = as.integer(amount)) |>
    dplyr::collect() |>
    tidyr::pivot_wider(
      names_from = language,
      values_from = "amount",
      values_fill = 0
    )

  if(includeEmptyTransects){

    transects <- getTransectWalks(include_id = FALSE) |>
      dplyr::collect()

    temp_out <- temp_out |>
      dplyr::full_join(transects,
                       by = c("Region" = "Region",
                              "Flate" = "Flate",
                              "Transekt" = "Transekt",
                              "Habitattype" = "Habitattype",
                              "Blomsterdekke" = "Blomsterdekke",
                              "Year" = "Year",
                              "Dato" = "Dato",
                              "Periode" = "Periode"),
                       keep = FALSE) |>
      mutate(across(everything(), ~replace_na(., 0)))
  }

  out <- temp_out |>
    dplyr::arrange(Region, Flate, Transekt, Year, Periode) |>
    dplyr::as_tibble()

  return(out)
}
