app_rank_serials <- function(DB) {
require(dplyr)
  tbl(DB, 'instqcllmeta') %>%
    select(., Serialnumber) %>%
    distinct() %>%
    mutate(., plat = substr(Serialnumber, 1, 2)) %>%
    select(., Serialnumber, plat) %>%
    arrange(Serialnumber) %>%
    collect() %>%
    group_by(plat) %>%
    mutate(x = dense_rank(Serialnumber)) %>%
    ungroup()
}
