#' indPlot
#'
#' Plot bumblebee and butterfly indicators
#'
#' This function assumes an input object from the function "collectIndData"
#'

#' @param input data object, from "collectIndData"
#' @param confLevel  Confidence level of error bars. "90\%" or "95\%".
#' @param errorWidth  widht of error bars
#' @param lineWidth width of lines
#' @param ylab default = "Indeks"
#' @param xlab default "År"
#' @param dodge How much should the points dodge? Default =  0.1
#' @param pointSize default = 0.2
#' @param errorColour color of errorbars. Default =  "black"
#'
#'
#'
#' @export



indPlot <- function(input = NULL,
                    confLevel = "90%",
                    errorWidth = 0.2,
                    lineWidth = 2,
                    ylab = "Indeks",
                    xlab = "År",
                    dodge = 0.1,
                    pointSize = 0.2,
                    errorColour = "black",
                    errorbarAlpha = 0.4){

  confLevel <- match.arg(confLevel, c("90%", "95%"))

  lower <- switch(confLevel,
                  "90%" = "lower90",
                  "95%" = "lower95")
  upper <- switch(confLevel,
                  "90%" = "upper90",
                  "95%" = "upper95"
  )

  pd <- position_dodge(dodge)

  input <- input %>%
    group_by(Year) %>%
    mutate(width = errorWidth * n()) %>%
    ungroup()

  customWidth <- "width"

  g <- ggplot(input, aes(x = Year,
                         y = Indicator_value,
                         colour = Region,
                         group = Region)) +
    geom_errorbar(aes_string(ymin = lower,
                             ymax = upper,
                             width = customWidth),
                  colour = errorColour,
                  lwd = lineWidth,
                  position = pd,
                  alpha = errorbarAlpha) +
    geom_line(lwd = lineWidth,
              position = pd) +
    geom_point(position = pd,
               size = pointSize) +
    ylab(ylab) +
    xlab(xlab)

  g <- g + NinaR::scale_color_nina()

  g

}
