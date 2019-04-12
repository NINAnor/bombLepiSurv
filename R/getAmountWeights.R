#' getAmountWeights
#'
#' Get amount weights from the database
#'
#'
#'
#'
#' @import dplyr
#'
#'
#' @export
#'
#'


getAmountWeights <- function(){

  dataRaw <- DBI::dbReadTable(con, Id(schema = "species", table = "amount_weights"))

  out <- dataRaw %>%
    dplyr::select(-id) %>%
    tidyr::spread(key = exp_cat,
           value = weight) %>%
    dplyr::select(v,
           m,
           s)

  return(out)
}
