upload <- function(LLdat){
  require(RMySQL)
  require(dplyr)
  dmy_db <-  adminKraken::con_dplyr()
  n <-tbl(dmy_db,'instqcllmeta') %>%
    select(.,file) %>%
    mutate(check = LLdat$meta$file == file) %>%
    filter(check == T) %>%
    summarise(n=n()) %>%
    collect() %>% unlist
  # then if n we can upload,else skip

  if(n == 0){
    my_db <-  adminKraken::con_mysql()
    dbWriteTable(my_db, name = "instqcllmeta",value = LLdat$meta,
                 append = T,overwrite = F,row.names=FALSE)


    runID <-
      tbl(dmy_db,'instqcllmeta') %>%
      filter(file == LLdat$meta$file) %>%
      select(ID) %>%
      collect()

    #uploadinstQClldata
      dbWriteTable(my_db, name = "instqclldata",value = mutate(LLdat$data,MetaID = runID$ID),
                   append = T,overwrite = F,row.names = FALSE)

    dbDisconnect(my_db)
  }#end if bracket
  rm(dmy_db)
  gc()
}#final bracket


