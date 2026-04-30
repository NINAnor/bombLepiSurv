#' humlesommerfConnect
#'
#' Connect to the humlesommerf database at ninradardata01
#'
#' @param username Optionally provide a username if you don't have that set up in ~/.pgpass
#' @param password Optionally provide a password if you don't have that set up in ~/.pgpass. Don't store this scripts (or outputs).
#'
#' @return A connection to the DB called 'con'
#'
#' @export
humlesommerfConnect <- function(
    username = NULL,
    password = NULL,
    host = "T2lippgsql03.nina.no",
    dbname = "humle_sommerf",
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

#' @rdname humlesommerfConnect
#' @export
connect_to_humlesommerf_db <- humlesommerfConnect

