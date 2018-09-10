# Query LL meta table to see if the file is in the db
file_query_meta <- function(U, db) {
  q <- RMySQL::dbSendQuery(
    db,
    paste0(
      'SELECT COUNT(file) as n FROM kraken.instqcllmeta where file="',
      U$meta$file,
      '";'
    )
  )
  out <- RMySQL::dbFetch(q)

  RMySQL::dbClearResult(q)
  out$n
}
