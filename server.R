shinyServer(function(input, output) {
   


  
##Create Ui ouputs for page 1=============
  
  ##########
  #Compute improvement rates and store data for plots on page 1
  
  #add new column to data so that line type can be specified
  CPPdtaCurrent <- CPPdta %>% mutate(Grouping=  paste(CPP, Type))  
  
  #add new column to show rate of improvement 
  CPPdtaCurrent <- filter(CPPdtaCurrent, Type != "Projected")
  CPPdtaCurrent <- ddply(CPPdtaCurrent,. (CPP, Indicator), transform, Diff = (last(value) - first(value)))
  CPPdtaCurrent <- ddply(CPPdtaCurrent,. (CPP, Indicator), transform, Improvement_Rate = ((Diff/first(value))*100))
  CPPdtaCurrent <- select(CPPdtaCurrent, -Diff)
  
  #add new column to show whether a high value represents a positive outcome
  CPPdtaCurrent <- CPPdtaCurrent %>% mutate(`High is Positive?` = "Yes")
  CPPdtaCurrent$`High is Positive?`[CPPdtaCurrent$Indicator %in% c("Dwelling Fires", "Unplanned Hospital Attendances",
                                                                   "Fuel Poverty", "Fragility", "Carbon Emissions",
                                                                   "Child Poverty", "Out Of Work Benefits",
                                                                   "Crime Rate", "Emergency Admissions",
                                                                   "Early Mortality")] <- "No"
  
  #Create a reactive function to store data for both LA's selected  
  selectedDta1 <- reactive({
    dta <- filter(CPPdtaCurrent, CPP %in% c(input$LA1, input$CompLA1))
  })

  #Create a list of all the indicators 
  Indicators1 <- unique(CPPdtaCurrent$Indicator)

  
  ##########
  #Create a loop that creates a plot for the indicators selected 
  
  for(i in seq_along(Indicators1)){
    local({
      my.i <- i
      plotname <- paste("plot", my.i, sep ="_")
      output[[plotname]] <- renderPlot({
        
        selectedDta1 <- selectedDta1()
        dtaAll <- selectedDta1
        
    #create a subset of the data for the particular indicator in the loop
    loopdata <- subset(dtaAll, dtaAll$Indicator == Indicators1[my.i])
  
    #split this data into the two LAs selected
    loopdataCPP1 <- filter(loopdata, CPP == input$LA1)
    loopdataCPP2 <- filter(loopdata, CPP == input$CompLA1)
        
    #store unique values of "high is positive?" to use in test later
    HighValue <- unique(loopdata$`High is Positive?`)
        
    #create an if statement to determine the colour of the dot
    #need to create 2 statements to distinguish "high is positive"
    #compares whether the value of the authority is higher than the comparator and whether the improvement rate is higher
    coloursDotPos <- if_else(((last(loopdataCPP1$value)) > (last(loopdataCPP2$value)) & 
                            (last(loopdataCPP1$Improvement_Rate)) > (last(loopdataCPP2$Improvement_Rate))),
                            "green",
                     if_else(((last(loopdataCPP1$value)) > (last(loopdataCPP2$value)) &
                            (last(loopdataCPP1$Improvement_Rate)) < (last(loopdataCPP2$Improvement_Rate))),
                            "yellow",
                     if_else(((last(loopdataCPP1$value)) < (last(loopdataCPP2$value)) &
                           (last(loopdataCPP1$Improvement_Rate)) > (last(loopdataCPP2$Improvement_Rate))),
                            "yellow",
                     if_else(((last(loopdataCPP1$value)) < (last(loopdataCPP2$value)) &
                           (last(loopdataCPP1$Improvement_Rate)) < (last(loopdataCPP2$Improvement_Rate))),
                           "red",
                           "black"))))
        
        
    coloursDotNeg <- if_else(((last(loopdataCPP1$value)) > (last(loopdataCPP2$value)) & 
                            (last(loopdataCPP1$Improvement_Rate)) > (last(loopdataCPP2$Improvement_Rate))),
                            "red",
                     if_else(((last(loopdataCPP1$value)) > (last(loopdataCPP2$value)) &
                            (last(loopdataCPP1$Improvement_Rate)) < (last(loopdataCPP2$Improvement_Rate))),
                            "yellow",
                     if_else(((last(loopdataCPP1$value)) < (last(loopdataCPP2$value)) &
                            (last(loopdataCPP1$Improvement_Rate)) > (last(loopdataCPP2$Improvement_Rate))),
                            "yellow",
                    if_else(((last(loopdataCPP1$value)) < (last(loopdataCPP2$value)) &
                            (last(loopdataCPP1$Improvement_Rate)) < (last(loopdataCPP2$Improvement_Rate))),
                            "green",
                            "black"))))
        
        
    #add new "year2" column to the data to store numeirc values for year
    loopdata <- arrange(loopdata, CPP)
    loopdata <- ddply(loopdata,. (CPP), transform, Year2 = (seq(1 : length(Year))))

    #add new "year3" column to store x axis labels
    loopdata <- ddply(loopdata,. (CPP), transform, Year3 = Year)
    loopdata$Year3 <- as.character(loopdata$Year3)
    Years2 <- unique(loopdata$Year2)
        
    #change year3 values so that labels will only show the 1st and last year
    loopdata$Year3[loopdata$Year2 > 1 & loopdata$Year2 < last(Years2)] <- ""
        
    #store unique year2 values and list of year3 values so that data length can be specified using these later
    Years3 <- filter(loopdata, CPP == input$LA1)
    Years3 <- Years3$Year3    
        
    #store raw data to be used for solid line
    dtaRaw <- loopdata[loopdata$Type == "Raw data",]        
               
            ggplot()+
            geom_line(data = loopdata,
                    aes(x = Year2, y = value, group = CPP, colour = CPP, linetype = "2"), lwd = 1, show.legend = FALSE)+
            geom_line(data = dtaRaw,
                    aes(x = Year2, y = value, group = CPP, colour = CPP, linetype = "1"), lwd = 1, show.legend = FALSE)+
            ggtitle(Indicators1[my.i])+
            annotate("text", x = Inf, y = Inf, label = sprintf('\U25CF'), size = 10, 
                    colour = (if_else(HighValue == "Yes", coloursDotPos, coloursDotNeg))
                    , hjust = 1, vjust = 1) +
            scale_x_continuous(breaks = c(1: length(Years2)), labels = Years3)+
            xlab("Year")+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                  panel.background = element_blank(), axis.line = element_line(colour="black"),
                  axis.text.x = element_text(vjust = 0.3))
      
                                       })
            })  
  }
  
  ##########
  #create single plot based on what indicator is selected
  
  output$Indi1Plot <- renderPlot({
    
    selectedDta1 <- selectedDta1()
    dtaAll<- selectedDta1
    dtasubset <- dtaAll[dtaAll$Indicator == input$Indi1,]
    
    #split this data into the two LAs selected
    dtasubsetCPP1 <- filter(dtasubset, CPP == input$LA1)
    dtasubsetCPP2 <- filter(dtasubset, CPP == input$CompLA1)
    
    #store unique values of "high is positive?" to use in test later
    HighValue <- unique(dtasubset$`High is Positive?`)
    
    #create an if statement to determine the colour of the dot
    #need to create 2 statement to distinguish "between positve high values"High is Positive"
    #compares whether the value of the authority is higher than the comparator and whether the improvement rate is higher
    coloursDotPos <- if_else(((last(dtasubsetCPP1$value)) > (last(dtasubsetCPP2$value)) & 
                            (last(dtasubsetCPP1$Improvement_Rate)) > (last(dtasubsetCPP2$Improvement_Rate))),
                            "green",
                     if_else(((last(dtasubsetCPP1$value)) > (last(dtasubsetCPP2$value)) &
                            (last(dtasubsetCPP1$Improvement_Rate)) < (last(dtasubsetCPP2$Improvement_Rate))),
                            "yellow",
                     if_else(((last(dtasubsetCPP1$value)) < (last(dtasubsetCPP2$value)) &
                            (last(dtasubsetCPP1$Improvement_Rate)) > (last(dtasubsetCPP2$Improvement_Rate))),
                            "yellow",
                     if_else(((last(dtasubsetCPP1$value)) < (last(dtasubsetCPP2$value)) &
                            (last(dtasubsetCPP1$Improvement_Rate)) < (last(dtasubsetCPP2$Improvement_Rate))),
                            "red",
                            "black"))))
    
    coloursDotNeg <- if_else(((last(dtasubsetCPP1$value)) > (last(dtasubsetCPP2$value)) & 
                            (last(dtasubsetCPP1$Improvement_Rate)) > (last(dtasubsetCPP2$Improvement_Rate))),
                            "red",
                     if_else(((last(dtasubsetCPP1$value)) > (last(dtasubsetCPP2$value)) &
                            (last(dtasubsetCPP1$Improvement_Rate)) < (last(dtasubsetCPP2$Improvement_Rate))),
                            "yellow",
                     if_else(((last(dtasubsetCPP1$value)) < (last(dtasubsetCPP2$value)) &
                            (last(dtasubsetCPP1$Improvement_Rate)) > (last(dtasubsetCPP2$Improvement_Rate))),
                            "yellow",
                     if_else(((last(dtasubsetCPP1$value)) < (last(dtasubsetCPP2$value)) &
                            (last(dtasubsetCPP1$Improvement_Rate)) < (last(dtasubsetCPP2$Improvement_Rate))),
                            "green",
                            "black"))))

    #add new "year2" column to the data to store numeirc values for year
    dtasubset <- arrange(dtasubset, CPP)
    dtasubset <- ddply(dtasubset,. (CPP), transform, Year2 = (seq(1 : length(Year))))
    
    #add new "year3" column to store x axis labels
    dtasubset <- ddply(dtasubset,. (CPP), transform, Year3 = Year)
    dtasubset$Year3 <- as.character(dtasubset$Year3)
    Years2 <- unique(dtasubset$Year2)
    
    #change year3 values so that labels will only show the 1st and last year
    dtasubset$Year3[dtasubset$Year2 > 1 & dtasubset$Year2 < last(Years2)] <- ""
    
    #store unique year3 values so that data length can be specified using these later
    Years3 <- filter(dtasubset, CPP == input$LA1)
    Years3 <- Years3$Year3    
   
    #store raw data to be used for solid line
    dtaRaw <- dtasubset[dtasubset$Type == "Raw data",]
 
            ggplot()+
            geom_line(data = dtasubset,
                      aes(x = Year2, y = value, group = CPP, colour = CPP, linetype = "2"), lwd = 1, show.legend = FALSE)+
            geom_line(data = dtaRaw,
                      aes(x = Year2, y = value, group = CPP, colour = CPP, linetype = "1"), lwd = 1, show.legend = FALSE)+
            ggtitle(input$Indi1)+
            annotate("text", x = Inf, y = Inf, label = sprintf('\U25CF'), size = 10,
                     colour = (if_else(HighValue == "Yes", coloursDotPos, coloursDotNeg))
                    , hjust = 1, vjust = 1)+
            scale_x_continuous(breaks = c(1: length(Years2)), labels = Years3)+
            xlab("Year")+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                  panel.background = element_blank(), axis.line = element_line(colour="black"),
                  axis.text.x = element_text(vjust = 0.3))
  })

  
  ##Create Ui Outputs for page 2 & 3 =================    
  
  output$Plot2 <- renderPlot({
    dat <- filter(CPPdta, Indicator == "Unplanned Hospital Attendances" & Year == "2016/17")
    dat$slct <- ifelse(dat$CPP == input$LA2, "Sel1", "Other") 
    cmp <- filter(dat, CPP == input$CompLA2)$value
    
    ggplot(data = dat) +
      geom_bar(aes(x = reorder(CPP,-value), y = value, fill = slct), stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("blue","red"), breaks = c("Other", "Sel1")) +
      guides(fill = FALSE) +
      geom_hline(aes(yintercept = cmp))
  })
  
  for(i in 1:18){
    local({
      my.i <- i
      nms <- gsub(" ", "",unique(CPPdta$Indicator))[[my.i]]
      plotname <- paste("plot", nms, sep ="_")
      output[[plotname]] <- renderPlot({
        indis <- unique(CPPdta$Indicator)
        slInd <- indis[[my.i]]
        ##Need to get this to select most recent year, since indicators have different periods  
        dat <- filter(CPPdta, Indicator == slInd & Year %in% c("2016/17", "2014-2016"))
        dat$slct <- ifelse(dat$CPP == input$LA3, "Sel1", "Other") 
        cmp <- filter(dat, CPP == input$CompLA3)$value
        ggplot(data = dat) +
          geom_bar(aes(x = reorder(CPP,-value), y = value, fill = slct), stat = "identity", position = "dodge") +
          scale_fill_manual(values = c("blue","red"), breaks = c("Other", "Sel1")) +
          guides(fill = FALSE) +
          ggtitle(slInd)+
          geom_hline(aes(yintercept = cmp))
      })
    })  
  }


  ##Render a UI with a certain number of rows and columns based on selected graphs
  output$uiPage3 <- renderUI({
    slctd <- length(input$grphs3)
    #number of columns is 4, unless there are less than 3 graphs
    cls <- if(slctd>3){4} else{slctd}
    pctCols <- 100/cls
    pctCols <- paste0(pctCols, "%")
    #number of rows is the number of graphs divided by 4 and rounded up eg 7 = 2 rows
    rows <- ceiling(slctd/4)
    ##Dynamically create plot height  
    pltheight <- paste0(800/rows, "px")
    inptLst <- as.list(gsub(" ", "",input$grphs3))
    ##Create however many
    fluidRow(
      column(12/cls,map(1, function(nc){
        plot_output_list1<- map(seq(from = nc,to = slctd,by = cls), function(x){
          tstNm1 <- inptLst[[x]]
          plotname <- paste("plot", tstNm1, sep = "_")
          plotOutput(plotname, height = pltheight)
        })
        do.call(tagList, plot_output_list1)         
      }) ),  
      column(12/cls,map(2, function(nc){
        plot_output_list2<- tryCatch(map(seq(from = nc,to = slctd,by = cls), function(x){
          tstNm2 <- inptLst[[x]]
          plotname <- paste("plot", tstNm2, sep = "_")
          plotOutput(plotname, height = pltheight)
        }), 
        error=function(cond) {
          
          return(list())
        }
        )
        do.call(tagList, plot_output_list2)         
      })
      ),
      column(12/cls,map(3, function(nc){
        plot_output_list3<- tryCatch(map(seq(from = nc,to = slctd,by = cls), function(x){
          tstNm3 <- inptLst[[x]]
          plotname <- paste("plot", tstNm3, sep = "_")
          plotOutput(plotname, height = pltheight)
        }), 
        error=function(cond) {
          
          return(list())
        }
        )
        do.call(tagList, plot_output_list3)         
      })
      ),
      column(12/cls,map(4, function(nc){
        plot_output_list4<- tryCatch(map(seq(from = nc,to = slctd,by = cls), function(x){
          tstNm4 <- inptLst[[x]]
          plotname <- paste("plot", tstNm4, sep = "_")
          plotOutput(plotname, height = pltheight)
        }), 
        error=function(cond) {
          
          return(list())
        }
        )
        do.call(tagList, plot_output_list4)         
      })
      )
      
    )  
  })
 

})




