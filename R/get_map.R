#' get_map
#' Fetch a terrestrial map from the database
#'
#' @param region_subset optional region(s) to subset
#'
#' @return an sf object with fylke name and multipolygon geometry
#' @export
#'
#' @examples
#' \dontrun{
#'
#' map_from_db <- get_map(region_subset = c("Ost", "Sørlandet"))
#' }
#'
get_map <- function(dataConnection = "con",
                    region_subset = NULL) {
  norway_terr <- sf::read_sf(get(dataConnection),
    layer = DBI::Id(schema = "backgrounds", table = "norway_terrestrial")
  ) %>%
    select(fylke = navn)


  region_def <- tibble(
    region = c(
      "Tr\u00f8ndelag",
      "\u00d8stlandet",
      "\u00d8stlandet",
      "\u00d8stlandet",
      "\u00d8stlandet",
      "S\u00f8r",
      "S\u00f8r",
      "Vestlandet",
      "Vestlandet",
      "Nord-Norge",
      "Nord-Norge",
      "\u00d8st",
      "\u00d8st"
    ),
    fylke = c(
      "Tr\u00f8ndelag",
      "Innlandet",
      "Oslo",
      "Vestfold og Telemark",
      "Viken",
      "Rogaland",
      "Agder",
      "Vestland",
      "M\u00f8re og Romsdal",
      "Troms og Finnmark",
      "Nordland",
      "Vestfold",
      "Østfold"
    )
  )

  norway_terr <- norway_terr %>%
    left_join(region_def,
      by = c("fylke" = "fylke")
    )

  if (!is.null(region_subset)) {
    norway_terr <- norway_terr %>%
      filter(region %in% region_subset)
  }

  return(norway_terr)
}
