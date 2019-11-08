app_server<-function(){
u_inst <- function(conn, PLAT, SLIDE) {
  tbl(conn, 'instqcllmeta') %>%
    select(., Inst = Serialnumber) %>%
    distinct() %>%
    mutate(., plat = floor(Inst / 10000)) %>%
    filter(., plat == PLAT) %>%
    select(., Inst) %>%
    arrange(Inst) %>%
    collect() %>%
    mutate(x = dense_rank(Inst)) %>%
    filter(., between(row_number(Inst), SLIDE[1], SLIDE[2]))
}

library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(magrittr)
my_db <- src_mysql(
  'Kraken',
  user = 'poseidon',
  password = 'poseidon',
  host = 'magnix.lkq.agilent.com',
  port = 3306
)
shinyServer(function(input, output, session) {
  nr_start <- my_db %>%
    tbl('instqcllmeta') %>%
    select(., Inst = Serialnumber) %>%
    #mutate(.,plat = floor(Inst / 10000)) %>%
    mutate(., plat = substr(Inst, 1, 2)) %>%
    filter(., plat == 42) %>%
    summarise(nd = n_distinct(Inst)) %>%
    collect() %>% c(.$nd)
  
  updateSliderInput(
    session,
    inputId = "SLIDE",
    min = 1,
    max = nr_start$nd,
    step = 1,
    value = c(nr_start$nd - 15, nr_start$nd)
  )
  
  
  observeEvent(input$plat, {
    nr <- my_db %>%
      tbl('instqcllmeta') %>%
      select(., Inst = Serialnumber) %>%
      mutate(., plat = floor(Inst / 10000)) %>%
      filter(., plat == input$plat) %>%
      summarise(nd = n_distinct(Inst)) %>%
      collect() %>% c(.$nd)
    updateSliderInput(
      session,
      inputId = "SLIDE",
      min = 1,
      max = nr$nd,
      step = 1
    )
  })
  
  
  observeEvent(
    c(
      input$select,
      input$plat,
      input$analyte,
      input$cktest,
      input$disttype,
      input$SLIDE
    ),
    {
      slides <- c(min(input$SLIDE), max(input$SLIDE))
      
      uInst <- u_inst(my_db, input$plat, slides)
      df0 <- my_db %>%
        tbl('instqcllmeta') %>%
        select(., Serialnumber, ID, file) %>%
        mutate(., plat = floor(Serialnumber / 10000)) %>%
        filter(., plat == input$plat) %>%
        rename(., Inst = Serialnumber) %>%
        semi_join(., uInst, by = 'Inst', copy = T)
      
      df1 <-  my_db %>%
        tbl('instqclldata') %>%
        mutate(., teO2 = 12500, tepH = 30000) %>%
        select(., ID = MetaID, Well, contains(input$analyte)) %>%
        select(
          .,
          ID,
          Well,
          LED = matches("O2LED|pHLED"),
          LL = matches("O2LL|pHLL"),
          TE = matches("teO2|tepH")
        ) %>%
        mutate(perLLTE = LL / TE * 100,
               perLLLED = LL / LED * 100)
      df2 <- inner_join(df0, df1, by = 'ID') %>%
        select(., Inst, B = matches(paste0("^", input$select)), file, Inst) %>%
        collect() %>%
        mutate(fn = as.numeric(factor(file))) %>%
        group_by(., Inst) %>%
        mutate(., test = factor(dense_rank(fn)))
      
      
      ##########
      # PLOTS
      output$distPlot <- renderPlot({
        P <- ggplot(df2, aes(factor(Inst), B)) +
          theme_light() +
          scale_fill_tableau() +
          xlab(input$analyte) +
          ylab("") +
          ggtitle(
            c(
              "LL" = "Light Leak",
              "LED" = "LED",
              "perLLTE" = "Light Leak % of TargetEmission",
              "perLLLED" = "Light Leak % of LED"
            )[input$select]
          ) +
          theme(text = element_text(size = 20),
                axis.text.x = element_text(angle = 90, hjust =
                                             1))
        
        if (input$cktest == TRUE) {
          switch(
            input$disttype,
            "boxplot" = P + geom_boxplot(
              aes(fill = factor(test)),
              width = .2,
              varwidth = F,
              lwd = .2,
              outlier.size = .7,
              # outlier.alpha = .4,
              outlier.shape = 21,
              outlier.color = 1,
              # outlier.fill = 'red',
              outlier.stroke =
                .5
            ),
            "violinplot" = P + geom_violin(aes(fill = factor(test)), trim =
                                             FALSE, size = .5),
            "TufteBoxPlot" = P + ggthemes::geom_tufteboxplot(aes(col =
                                                                   factor(test)))
          ) +
            ggthemes::scale_colour_excel()
          
          
        } else{
          switch(
            input$disttype,
            "boxplot" = P + geom_boxplot(
              lwd = 0.35,
              fill = 'white',
              width = 0.3,
              varwidth = F ,
              outlier.size = .7,
              # outlier.alpha = .4,
              outlier.shape = 21,
              outlier.color = 1,
              # outlier.fill = 'red',
              outlier.stroke =
                .5
            ),
            "violinplot" = P  + geom_violin(
              trim = FALSE,
              fill = '#A4A4A4',
              color = "darkred",
              size = 0.5
            ),
            "TufteBoxPlot" = P + ggthemes::geom_tufteboxplot(col = 'black')
            
          )
          
          
        }
        
        
      })
      
      
      
    }
  )
})
}
