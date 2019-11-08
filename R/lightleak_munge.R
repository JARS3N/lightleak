munge<-function(xlfile){
    require(readxl)
  require(dplyr)
  require(tidyr)

  XL<- as.data.frame(readxl::read_excel(xlfile,sheet= 'Results',col_names = F),stringsAsFactors=F)
  Meta<-lightleak::pull_meta(XL,xlfile)
  w<-lightleak::index_analyte(XL)
  data<-lightleak::split_analytes(XL,indexAnalyte(XL))
  NCOL<-lightleak::nofCol(Meta$Serialnumber)
  list(data =
         Reduce('inner_join',
                list(
                  lightleak::get_table(data$O2,"Emissions",NCOL) %>%
                    rename(O2LL=Val),
                  lightleak::get_table(data$O2,"Calibration LED",NCOL) %>%
                    rename(O2LED=Val),
                  lightleak::get_table(data$pH,"Emissions",NCOL) %>%
                    rename(pHLL=Val),
                  lightleak::get_table(data$pH,"Calibration LED",NCOL) %>%
                    rename(pHLED=Val)
                )
         )
       ,meta = Meta)
}


