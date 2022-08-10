app_n_inst<- function(DB) {
require(dplyr)
out <- tbl(DB, "instqcllmeta") %>% 
    select(Serialnumber) %>%
    distinct() %>%
    collect() %>%
    mutate(., plat = substr(gsub("^00","",Serialnumber), 1, 2)) %>%
    group_by(plat) %>% 
    count() 
   setNames(out$n,out$plat)
}
