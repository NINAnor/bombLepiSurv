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
#' @export

collectIndData <- function(taxa = c("bumblebees", "butterflies"),
                           habitat = c("gressmark", "skogsmark"),
                           years = 2009:2018) {
  taxa <- match.arg(taxa, c = c("bumblebees", "butterflies"))
  habitat <- match.arg(habitat, c("gressmark", "skogsmark"))


  taxa <- switch(taxa,
    "bumblebees" = "h",
    "butterflies" = "s"
  )

  habitat <- switch(habitat,
    "gressmark" = "Gress",
    "skogsmark" = "Skog"
  )


  comb <- expand.grid(taxa, "Ind", years, c("Ost", "Sor", "Trond", "Vest"), habitat)

  toLoad <- apply(comb, 1, paste0, collapse = "")
  # remove stuff not in environment
  toLoad <- cbind(comb, toLoad)
  toLoad <- toLoad[toLoad$toLoad %in% ls(envir = parent.frame()), ]
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
