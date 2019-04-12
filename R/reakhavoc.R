
reakHavoc <- function(){

  answer <- menu(c("Yes (1)", "No (2)"), title ="You are about to delete all records!!! Are you sure?")

  havoc1 <- "TRUNCATE TABLE species.names RESTART IDENTITY CASCADE;"
  havoc2 <- "TRUNCATE TABLE species.amount_weights RESTART IDENTITY CASCADE;"
  havoc3 <- "TRUNCATE TABLE species.diff_weights RESTART IDENTITY CASCADE;"
  havoc4 <- "TRUNCATE TABLE transects.habitattype RESTART IDENTITY CASCADE;"
  havoc5 <- "TRUNCATE TABLE transects.region RESTART IDENTITY CASCADE;"




  if(answer == 1){
    dbSendStatement(con, havoc1)
    dbSendStatement(con, havoc2)
    dbSendStatement(con, havoc3)
    dbSendStatement(con, havoc4)
    dbSendStatement(con, havoc5)

    return("Things are gone, database should be clean!")
  } else return("Nothing")

}
