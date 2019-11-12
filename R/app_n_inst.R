app_n_inst<- function(DB) {
require(dplyr)
  out <- tbl(DB, 'instqcllmeta') %>%
    select(Serialnumber) %>%
    distinct() %>%
    mutate(., plat = substr(Serialnumber, 1, 2)) %>%
    group_by(plat) %>%
    count() %>%
    collect() 
   setNames(out$n,out$plat)
}
