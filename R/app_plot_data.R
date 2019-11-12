app_plot_data<-function(RANKS,SLIDES,PLAT,DB,ANALYTE){
require(dplyr)
  filter(RANKS,plat == PLAT) %>%
    filter(x >= SLIDES[1], x <= SLIDES[2]) %>%
    left_join(.,
              tbl(DB, 'instqcllmeta'),
              by = 'Serialnumber',
              copy = T) %>%
    select(MetaID = ID, Serialnumber, x) %>%
    left_join(.,
              tbl(DB, 'instqclldata'),
              by = 'MetaID',
              copy = T) %>%
    collect() %>%
    mutate(TE = c(O2 = 12500, pH = 30000)[ANALYTE]) %>%
    select(., Serialnumber, ID = MetaID, Well, contains(ANALYTE), TE) %>%
    select(
      .,
      Serialnumber,
      ID,
      Well,
      LED = matches("O2LED|pHLED"),
      LL = matches("O2LL|pHLL"),
      TE
    ) %>%
    mutate(perLLTE = LL / TE * 100,
           perLLLED = LL / LED * 100) %>% 
    group_by(Serialnumber) %>% 
    mutate(test=as.numeric(factor(ID))) %>% 
    ungroup()
}
