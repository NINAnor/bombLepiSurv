#' collectIndData
#'
#' Bundle indicator values into a form that is easy to plot
#'
#' This function assumes that the environment holds a bunch of bumblebee or butterfly indicator objects with a specific naming format (e.g. hInd2014SorGress)
#'
#' @param taxa bumblees or butterflies
#' @param habita gressmark or skogsmark
#' @param years what years to collect?
#'
#' @return a tibble with indicator point estimates and lower and upper uncertainty bounds.
#'
#' @examples
#'
#' \dontrun{
#'
#' data("hInd2010OstGress")
#' data("hInd2011OstGress")
#'
#' tt <- collectIndData(type = "Bumblebees",
#'                      habitat = "Gressmark",
#'                      years = 2010:2011)
#'
#' }
#'
#'
#' @export

collectIndData <- function(type = c("Bumblebees", "Butterflies"),
                           habitat = c("Gressmark", "Skogsmark"),
                           years = 2009:2018) {

  type <- match.arg(type, c = c("Bumblebees", "Butterflies"))
  habitat <- match.arg(habitat, c("Gressmark", "Skogsmark"))


  taxa <- switch(type,
    "Bumblebees" = "h",
    "Butterflies" = "s"
  )


  habitat_cat <- switch(habitat,
                        "Gressmark" = "Gress",
                        "Skogsmark" = "Skog"
  )

  comb <- expand.grid(taxa, "Ind", years, c("Ost", "Sor", "Trond", "Vest"), habitat_cat)

  toLoad <- apply(comb, 1, paste0, collapse = "")
  # remove stuff not in environment
  toLoad <- cbind(comb, toLoad)
  toLoad <- toLoad[toLoad$toLoad %in% data(package = "bombLepiSurv")$results[,"Item"], ]
  colnames(toLoad) <- c("Taxa", "ind", "Year", "Region", "Habitat", "toLoad")

  rawVals <- lapply(as.vector(toLoad$toLoad), function(x) get(x)[[1]])
  valFrame <- do.call("rbind", rawVals)
  colnames(valFrame) <- c("lower95", "lower90", "Indicator_value", "upper90", "upper95")
  valFrame <- valFrame %>%
    as_tibble() %>%
    cbind(toLoad) %>%
    arrange(Region, Year) %>%
    mutate(Region = as.character(Region)) %>%
    mutate(Region = ifelse(Region == "Ost", "Øst", Region)) %>%
    mutate(Region = ifelse(Region == "Sor", "Sør", Region)) %>%
    mutate(Region = ifelse(Region == "Trond", "Trøndelag", Region)) %>%
    mutate(Region = ifelse(Region == "Vest", "Vestlandet", Region))

  return(valFrame)
}
