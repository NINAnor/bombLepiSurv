#' plotArt
#'
#' Plot bumblebee and butterfly occurrences in the shiny app style
#'
#'
#'
#' @param obs observed community (Its species names will be used)
#' @param exp expected community
#' @param addZeroes Add non-observed species, boolean
#' @param addLegend Add legend, boolean
#'
#'
#'
#' @export


plotArt <- function(obs,
                    exp,
                    addZeroes = FALSE,
                    addLegend = FALSE,
                    ...) {

  sum.obs <- sort(colSums(obs[, 4:length(obs)] > 0), decreasing = T)

  if(!addZeroes){
    sum.obs <- sum.obs[sum.obs > 0]
  }

  perc.obs <- sum.obs / nrow(obs)

  colors <- tibble(amount = c(NA,
                           "s",
                           "m",
                           "v",
                           "g"),
                   color = c("black",
                              "#E57200",
                              "#008C95",
                              "#7A9A01",
                              "grey")
  )

  exp <- exp %>%
    left_join(colors,
              by = c("amount" = "amount")) %>%
    mutate(amount = ifelse(is.na(amount), "Ikke forv.", amount),
           amount = ifelse(amount == "s", "Sjelden", amount),
           amount = ifelse(amount == "m", "Middels v.", amount),
           amount = ifelse(amount == "v", "Vanlig", amount),
           amount = ifelse(amount == "g", "Gjest", amount)
    )

  scale_fill_bombLepi <- function(){
    scale_fill_manual(values = c("Ikke forv." = "black",
                                 "Sjelden" = "#E57200",
                                 "Middels v." = "#008C95",
                                 "Vanlig" = "#7A9A01",
                                 "Gjest" = "grey"),
                      name = "")
  }



  plot_data <- tibble(species_latin = names(perc.obs),
                      perc_obs = perc.obs) %>%
    left_join(exp,
              by = c("species_latin" = "species_latin")) %>%
    mutate(perc_obs = perc_obs * 100) %>%
    select(species_latin,
           perc_obs,
           exp = amount,
           color) %>%
    mutate(xaxis_col = glue::glue("<i style='color:{color}'>{species_latin}</i>")) %>%
    arrange(perc.obs)

 p <- ggplot2::ggplot(plot_data,
                      aes(y = perc_obs,
                          x = reorder(xaxis_col, desc(perc_obs)),
                          fill = exp)) +
   coord_flip() +
   ggplot2::theme_classic() +
   ggplot2::theme(axis.text.y = ggtext::element_markdown()) +
   ggplot2::xlab("") +
   ggplot2::ylab("Prosentvis transektforekomst") +
   geom_blank()

  p <- p +
    annotate("rect",
             xmin = 0.5,
             xmax = nrow(plot_data) + 0.5,
             ymin = 0,
             ymax = 1,
             fill = NinaR::addAlpha("#E5720080", 0.5)) +
    annotate("rect",
             xmin = 0.5,
             xmax = nrow(plot_data) + 0.5,
             ymin = 1,
             ymax = 5,
             fill = NinaR::addAlpha("#0000FF64", 0.5)) +
    annotate("rect",
           xmin = 0.5,
           xmax = nrow(plot_data) + 0.5,
           ymin = 5,
           ymax = max(plot_data$perc_obs),
           fill = NinaR::addAlpha("#00FF0064", 0.5))

    p <-  p +
      ggplot2::geom_bar(stat = "identity") +
      scale_fill_bombLepi()

    if(!addLegend){
      p <- p +
        theme(legend.position = "none")

    }


  return(p)

}
