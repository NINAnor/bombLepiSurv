#' humlesommerfConnect
#'
#' Connect to the humlesommerf database at ninpgsql02
#'
#'
#'
#'
#' @export
#'
#'


humlesommerfConnect <- function(username = "humlereader",
         password = "naturindeks",
         host = "ninpgsql02.nina.no",
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
