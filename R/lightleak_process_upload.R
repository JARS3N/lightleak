process_upload <- function(U,db)
{
  #cleanse file path
  U$meta$file <- basename(U$meta$file)
  # remove leading zeros from instrument serial
  U$meta$Serialnumber <- gsub("^00","",U$meta$Serialnumber)
  #copy meta data over
  dbWriteTable(db, name = "instqcllmeta", value = U$meta, 
               append = T, overwrite = F, row.names = FALSE)
  # query to get auto enumerated ID
 qstring <- gsub("%%%",shQuote(U$meta$file),
                 "SELECT ID from instqcllmeta where file=%%%;")
  query <- RMySQL::dbSendQuery(con=my_db,
                               statement = qstring)
  runID <- RMySQL::dbFetch(query)
  RMySQL::dbClearResult(query)
  # add meta ID to data
  U$data$MetaID <- runID$ID
  # add LL data to LL data table
  dbWriteTable(my_db, name = "instqclldata", value = U$data, 
               append = T, overwrite = F, row.names = FALSE)
}
