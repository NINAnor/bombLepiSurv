#' refreshViews
#'
#' Refresh the materialized views
#'
#'
#' Use this after each insert or update of the basic data. You need to be owner of parent tables.
#'
#'
#'
#'
#'
#' @export

refreshViews <- function() {
  if (!exists("con")) {
    warning("No connection!")
    return(NULL)
  }

  bombus <- "REFRESH MATERIALIZED VIEW views.all_bombus;"
  lepidoptera <- "REFRESH MATERIALIZED VIEW views.all_lepidoptera;"

  dbSendQuery(con, bombus)
  dbSendQuery(con, lepidoptera)
}
