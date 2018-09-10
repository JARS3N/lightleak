
#### upload LightLeak Data
upload <- function(LLdat) {
  require(RMySQL)
  my_db <-  adminKraken::con_mysql()

  files_not_in_db <-
    unlist(lapply(LLdat, file_query_meta, db = my_db)) ==
    1

  go <- lapply(LLdat[files_not_in_db], process_upload, db)

  dbDisconnect(my_db)
  go
}
