library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
##Create Ui ouputs for page 1=============

#Create a reactive function to store data for both LA's selected  
  selectedDta1 <- reactive({
    dta <- filter(CPPdta, CPP %in% c(input$LA1, input$CompLA1) & Type != "Projected")
  })

  
#Create a list of all the indicators 
  Indicators1 <- unique(CPPdta$Indicator)
  
#add new column to data so that line type can be specified
  CPPdta <- CPPdta %>% mutate(Grouping=  paste(CPP, Type))
  
 xmax1 <- Inf - 1
 ymax1 <- Inf -1 
  
#Create a loop that creates a plot for the indicators selected  
  for(i in seq_along(Indicators1)){
    local({
      my.i <- i
      plotname <- paste("plot", my.i, sep ="_")
      output[[plotname]] <- renderPlot({
        selectedDta1 <- selectedDta1()
        dtaAll <- selectedDta1[selectedDta1$Type != "Projected",]
        dtaRaw <- selectedDta1[selectedDta1$Type == "Raw data",]
        ggplot()+
          geom_line(data = subset(dtaAll, dtaAll$Indicator == Indicators1[my.i]),
                  aes(x = Year, y = value, group = CPP, colour = CPP, linetype = "2"), lwd = 1, show.legend = FALSE)+
          geom_line(data = subset(dtaRaw, dtaRaw$Indicator == Indicators1[my.i]),
                  aes(x = Year, y = value, group = CPP, colour = CPP, linetype = "1"), lwd = 1, show.legend = FALSE)+
          ggtitle(Indicators1[my.i])+
          annotate("text", x = Inf, y = Inf, label = sprintf('\U25CF'), size = 10, colour = "Red", hjust = 1, vjust = 1) +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                panel.background = element_blank(), axis.line = element_line(colour="black"),
                axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.3))
      })
   
    })  
  }
  
#create single plot based on what indicator is selected
  output$Indi1Plot <- renderPlot({
    selectedDta1 <- selectedDta1()
    dtaAll<- selectedDta1[selectedDta1$Type != "Projected",]
    dtaRaw <- selectedDta1[selectedDta1$Type == "Raw data",]
    ggplot()+
      geom_line(data = dtaAll[dtaAll$Indicator == input$Indi1,],
                aes(x = Year, y = value, group = CPP, colour = CPP, linetype = "2"), lwd = 1, show.legend = FALSE)+
      geom_line(data = dtaRaw[dtaRaw$Indicator == input$Indi1,],
                aes(x = Year, y = value, group = CPP, colour = CPP, linetype = "1"), lwd = 1, show.legend = FALSE)+
      ggtitle(input$Indi1)+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour="black"),
            axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.3))
  })
  
 
  
})
