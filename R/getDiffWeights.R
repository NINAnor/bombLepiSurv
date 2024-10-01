#' getDiffWeights
#'
#' Get a matrix of difference weights for NI indicator calculations.
#'
#' @import dplyr
#'
#' @export
#'
#'


getDiffWeights <- function() {
  dataRaw <- DBI::dbReadTable(con, DBI::Id(schema = "species", table = "diff_weights"))

  out <- dataRaw %>%
    dplyr::select(-id) %>%
    tidyr::spread(
      key = exp_cat,
      value = weight
    ) %>%
    dplyr::arrange(match(obs_cat, c("v", "m", "s", "t"))) %>%
    `rownames<-`(.[, 1]) %>%
    dplyr::select(
      v,
      m,
      s
    )

  return(out)
}
