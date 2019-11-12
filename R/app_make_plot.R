app_make_plot <- function(DATA,VAR,ANALYTE,DISTB,CHECK) {
  P <- 
    rename(DATA,B = VAR) %>%
    ggplot(., aes(factor(Serialnumber),B)) +
    theme_light() +
    scale_fill_tableau() +
    xlab(ANALYTE) +
    ylab("") +
    ggtitle(
      c(
        "LL" = "Light Leak",
        "LED" = "LED",
        "perLLTE" = "Light Leak % of TargetEmission",
        "perLLLED" = "Light Leak % of LED"
      )[VAR]
    ) +
    theme(text = element_text(size = 20),
          axis.text.x = element_text(angle = 90, hjust =
                                       1))
  
  
  if (CHECK == TRUE) {
    switch(
      DISTB,
      "boxplot" = P + geom_boxplot(
        aes(fill = factor(test)),
        width = .2,
        varwidth = F,
        lwd = .2,
        outlier.size = .7,
        outlier.shape = 21,
        outlier.color = 1,
        outlier.stroke =.5
      ),
      "violinplot" = P + geom_violin(aes(fill = factor(test)), trim =
                                       FALSE, size = .5),
      "TufteBoxPlot" = P + ggthemes::geom_tufteboxplot(aes(col =
                                                             factor(test)))
    ) +
      ggthemes::scale_color_colorblind()
    
    
  } else{
    switch(
      DISTB,
      "boxplot" = P + geom_boxplot(
        lwd = 0.35,
        fill = 'white',
        width = 0.3,
        varwidth = F ,
        outlier.size = .7,
        outlier.shape = 21,
        outlier.color = 1,
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
    
    
  }#end else
}
