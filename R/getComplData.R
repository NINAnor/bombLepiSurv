#' getComplData
#'
#' Get observation data from complete surveys of butterflies and bumblebees from the database
#'
#' This is suitable for calculating the Nature Index indicators
#'
#'
#' @param type What data to get, "Bumblebees" or "Butterflies"
#' @param region_short Get data from what region? "Trond", "Ost", "Sor", "Vest", "Nord"
#' @param habitat Get data from what habitat type? Gressmark or Skogsmark
#' @param year Get data from what year?
#'
#'
#'
#' @import dplyr
#'
#' @export
#'


getComplData <- function(type = c("Bumblebees", "Butterflies"),
                         region_short = c("Trond", "Ost", "Sor", "Vest", "Nord"),
                         habitat = c("Gressmark", "Skogsmark"),
                         year = 2010,
                         dataConnection = "con") {
  type <- match.arg(type)
  region_short <- match.arg(region_short)
  habitat <- match.arg(habitat)
  subset_year = year
  # Temporary as long as database has lowecase values. Could update database but prob breaks something else.
  type_cat <- switch(type,
    "Bumblebees" = "bumblebees",
    "Butterflies" = "butterflies"
  )
  region_short_cat <- switch(region_short,
    "Trond" = "trond",
    "Ost" = "ost",
    "Sor" = "sor",
    "Vest" = "vest"
  )
  habitat_cat <- switch(habitat,
    "Gressmark" = "gressmark",
    "Skogsmark" = "skogsmark"
  )


  if (type_cat == "bumblebees") {
    data_source <- dplyr::tbl(get(dataConnection),
                              Id(schema = "views",
                                 table = "compl_bombus_agg")
    )
  } else {
    data_source <- dplyr::tbl(get(dataConnection),
                              Id(schema = "views",
                                 table = "compl_lepidoptera_agg")
    )
  }


  dataRaw <- data_source %>%
    filter(region_short == region_short_cat,
           habitat_type == habitat_cat,
           year == subset_year)

  out <- dataRaw %>%
    dplyr::select(
      Flate = flate,
      Transekt = flate_transect,
      species_latin,
      amount
    ) %>%
    collect() %>%
    mutate(amount = as.integer(amount)) %>%
    tidyr::pivot_wider(
      names_from = "species_latin",
      values_from = "amount",
      values_fill = 0
    ) %>%
    dplyr::arrange(Flate, Transekt) %>%
    as_tibble()

  return(out)
}
