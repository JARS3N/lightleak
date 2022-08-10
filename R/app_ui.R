app_ui<-function(){
library(shiny)
shinyUI(fluidPage(fluidRow(
  mainPanel(width = 15,height = 100,
            column(width = 1,selectInput("plat", "Platform", c("e24" = 42,"e96" = 41,"XFp" = 43,"XFPro" = 45),
                                         selected = 42, multiple = FALSE,width = 100)),
            column(width = 1,selectInput("analyte", "Analyte", c("O2","pH"),
                                         selected = NULL, multiple = FALSE,width = 100)),
            column(width = 2,selectInput("select", "Selection", c("LightLeak" = "LL","LED" = "LED",
                                                                  "%ofTE" = 'perLLTE',"%ofLED" = 'perLLLED'),
                                         selected = NULL, multiple = FALSE,width = 125)),
            column(width = 4,sliderInput("SLIDE", label = h5("Range"), min = 1,
                                         max = 1, value = c(1, 2),step = 1)),
            column(width = 2,shiny::checkboxInput("cktest","individual tests")),
            column(width = 2,shiny::radioButtons("disttype","Box/Violin",c("boxplot","violinplot","TufteBoxPlot")))
  ),#end fluid row #1
  fluidRow(plotOutput('distPlot',width ="95%",height = "500px"))
)
))
}
