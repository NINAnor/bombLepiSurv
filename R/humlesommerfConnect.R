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
  host = "ninradardata01.nina.no",
  dbname = "humlesommerf",
  connectionName = "con",
  bigint = "integer",
  ...){

  tmp <- DBI::dbConnect(RPostgres::Postgres(),
                        host = host,
                        dbname = dbname,
                        user = username,
                        password = password,
                        bigint = bigint,
                        ...)

  assign(connectionName, tmp, .GlobalEnv)

}
