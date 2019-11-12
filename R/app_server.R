app_server<-function(){
library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)
my_db <- adminKraken::con_dplyr()
E <- new.env()
E$n <- lightleak::app_n_inst(my_db)
E$ranks <- lightleak::app_rank_serials(my_db)

shinyServer(function(input, output, session) {
  observeEvent(input$plat, {
    nr <- unname(E$n[input$plat])
    updateSliderInput(
      inputId = "SLIDE",
      min = 1,
      max = nr,
      step = 1,
      value = c(nr - 15, nr),
      session
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
      df2 <- app_plot_out(E$ranks, slides, input$plat, my_db, input$analyte)
      
      ##########
      # PLOTS
      output$distPlot <-
        renderPlot({
          lightleak::app_make_plot(df2, input$select, input$analyte, input$disttype)
        })# end render plot
      
      
    }
  )
  
})
}
