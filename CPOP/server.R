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
    dta <- filter(CPPdta, CPP %in% c(input$LA1, input$CompLA1))
  })
  
  
#Create a list of all the indicators 
  Indicators1 <- unique(CPPdta$Indicator)
  
#add new column to data so that line type can be specified
  CPPdta <- CPPdta %>% mutate(Grouping=  paste(CPP, Type))
  
#Create a loop that creates a plot for the indicators selected  
  for(i in seq_along(Indicators1)){
    local({
      my.i <- i
      plotname <- paste("plot", my.i, sep ="_")
      output[[plotname]] <- renderPlot({
        selectedDta1 <- selectedDta1()
        ggplot(subset(selectedDta1, selectedDta1$Indicator == Indicators1[my.i]),
               aes(x = Year, y = value, group = Grouping, colour = CPP, linetype = Type))+
          geom_line(show.legend = FALSE)+
          ggtitle(Indicators1[my.i])+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                panel.background = element_blank(), axis.line = element_line(colour="black"),
                axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.3))
      })
   
    })  
  }
  
#create single plot based on what indicator is selected
  output$Indi1Plot <- renderPlot({
    selectedDta1 <- selectedDta1()
    ggplot(selectedDta1[selectedDta1$Indicator == input$Indi1,],
           aes(x = Year, y= value, group = Grouping, colour = CPP, linetype = Type))+
      geom_line(show.legend = FALSE)+
      ggtitle(input$Indi1)+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour="black"),
            axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.3))
  })
  
})
