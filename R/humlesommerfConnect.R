#' humlesommerfConnect
#'
#' Connect to the humlesommerf database at ninradardata01
#'
#'
#'
#'
#' @export
#'
#'


humlesommerfConnect <- function(
    username = NULL,
    password = NULL,
    host = "T2lippgsql02.nina.no",
    dbname = "humlesommerf",
    connectionName = "con",
    ...) {
  tmp <- DBI::dbConnect(RPostgres::Postgres(),
    host = host,
    dbname = dbname,
    user = username,
    password = password,
    ...
  )

  assign(connectionName, tmp, .GlobalEnv)
}
