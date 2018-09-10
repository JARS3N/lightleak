process_upload <- function(U, db) {
  # upload meta data which gives it an ID#
  dbWriteTable(
    my_db,
    name = "instqcllmeta",
    value = U$meta,
    append = T,
    overwrite = F,
    row.names = FALSE
  )
  # get ID number
  query <- RMySQL::dbSendQuery(db,
                               paste0('SELECT ID from instqcllmeta where file="', U$meta$file, ';"'))
  runID <- RMySQL::dbFetch(query)
  RMySQL::dbClearResult(query)
  #add Ide number to data.frame
  U$data$MetaID <- runID$ID
  #uploadinstQClldata
  dbWriteTable(
    my_db,
    name = "instqclldata",
    value = U$data,
    append = T,
    overwrite = F,
    row.names = FALSE
  )

}
