#' surveyBarplot
#'
#' Barplot of total number of survey records, divided by region
#'
#'
#' @param input An input from getAllData
#' @param ylab character, default = "Art".
#' @param xlab character, default = "Antall"
#'
#'
#'
#'
#' @export


surveyBarplot <- function(input,
                      ylab = "",
                      xlab = "Antall"
){

  agg <- input %>%
    select(-c(Flate, Transekt, Habitattype, Dato, Year, Periode, Blomsterdekke)) %>%
    group_by(Region) %>%
    summarize_all(list(~sum(., na.rm = T))) %>%
    select_if(function(col) is.character(col) || sum(col) > 0) %>%
    gather(key = "Species", value = "Amount", -Region) %>%
    group_by(Species) %>%
    mutate(Sum = sum(Amount)) %>%
    arrange(Sum, Region)

  #Set the ordering of species factor
  aggSum <- agg %>%
    ungroup() %>%
    arrange(desc(Sum), Species) %>%
    mutate(Species = factor(Species, levels = unique(Species)))

  g <- ggplot(aggSum) +
    geom_bar(aes(x = Species, y = Amount,
                 fill = Region),
             stat = "identity",
             position = "dodge") +
    coord_flip() +
    xlab(ylab) +
    ylab(xlab) +
    NinaR::scale_fill_nina()


  g

}
