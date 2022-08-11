
#### upload LightLeak Data
upload <- function(LLDat) {
  require(RMySQL)
  upload_per_file <- function(obj, db) {
    not_in_db <- file_query_meta(obj, db) == 1
    if (not_in_db) {
      process_upload(obj, db)
    }
  }
  my_db <- adminKraken::con_mysql()
  res <- lapply(LLDat, upload_per_file, db = my_db)
  dbDisconnect(my_db)
}
