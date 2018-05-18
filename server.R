shinyServer(function(input, output, session) {
   


  
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
    CPPdtaCurrent$colourscheme <- ifelse(CPPdtaCurrent$CPP == input$LA1,"A","B")
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
        
        dtaAll <- selectedDta1()
        
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
                    aes(x = Year2, y = value, group = colourscheme, colour = colourscheme, linetype = "2"), lwd = 1, show.legend = FALSE)+
            geom_line(data = dtaRaw,
                    aes(x = Year2, y = value, group = colourscheme, colour = colourscheme, linetype = "1"), lwd = 1, show.legend = FALSE)+
            scale_color_manual(values = c("red", "blue"))+
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

##Events for Button inputs page 2 and 3======================  
  observeEvent(input$selAll2,
    handlerExpr = {
      updateCheckboxGroupInput(session = session,
                               inputId = "grphs2",
                               selected = unique(CPPdta$Indicator))
    } 
               )
  observeEvent(input$selNone2,
      handlerExpr = {
       updateCheckboxGroupInput(session = session,
                                 inputId = "grphs2",
              selected = NA)
               }     
               )
  observeEvent(input$selAll3,
               handlerExpr = {
                 updateCheckboxGroupInput(session = session,
                                          inputId = "grphs3",
                                          selected = unique(CPPdta$Indicator))
               } 
  )
  observeEvent(input$selNone3,
               handlerExpr = {
                 updateCheckboxGroupInput(session = session,
                                          inputId = "grphs3",
                                          selected = NA)
               }     
  )
  
  #create single plot based on what indicator is selected===  
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
  ##create all graphs that can be shown in Pages 3
  #These are then pulled through in the uiOutputs
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
          geom_bar(aes(x = reorder(CPP,-value), y = value, fill = slct), 
                   stat = "identity", position = "dodge", width = 0.5) +
          scale_fill_manual(values = c("blue","red"), breaks = c("Other", "Sel1")) +
          guides(fill = FALSE) +
          ggtitle(slInd)+
          xlab("")+
          ylab("")+
          geom_hline(aes(yintercept = cmp)) +
          theme_bw()+
          theme(axis.text.x = element_text(angle =90, hjust =1, vjust = 0))
      })
    })  
  }

  ##Create Graphs for Page 2 - Similar Councils Only
  for(i in 1:18){
    local({
      my.i <- i
      nms <- gsub(" ", "",unique(CPPdta$Indicator))[[my.i]]
      plotnameFG <- paste("FGplot", nms, sep ="_")
      output[[plotnameFG]] <- renderPlot({
        indis <- unique(CPPdta$Indicator)
        slInd <- indis[[my.i]]
        #get family group of LA for looped indicator
        FGNo <- unique(filter(CPPdta, Indicator == slInd &  CPP == input$LA2)[[6]])
        ##Need to get this to select most recent year, since indicators have different periods  
        dat <- filter(CPPdta, Indicator == slInd & Year %in% c("2016/17", "2014-2016"))
        dat$slct <- ifelse(dat$CPP == input$LA2, "Sel1", "Other") 
        cmp <- filter(dat, CPP == input$CompLA2)$value
        dat <- filter(dat, FG == FGNo)
        ggplot(data = dat) +
          geom_bar(aes(x = reorder(CPP,-value), y = value, fill = slct), 
                   stat = "identity", position = "dodge", width = 0.5) +
          scale_fill_manual(values = c("blue","red"), breaks = c("Other", "Sel1")) +
          guides(fill = FALSE) +
          ggtitle(slInd)+
          geom_hline(aes(yintercept = cmp))+
          xlab("")+
          ylab("")+
          theme_bw()+
          theme(axis.text.x = element_text(angle =90, hjust = 1, vjust = 0))
      })
    })  
  }
  

  ##Render a UI with a certain number of rows and columns based on selected graphs
  output$uiPage3 <- renderUI({
    slctd <- length(input$grphs3)
    #number of columns is 4, unless there are less than 3 graphs
    cls <- if(slctd>3){4} else{slctd}
    #The percentage fo the space each columns will occupy
    pctCols <- 100/cls
    pctCols <- paste0(pctCols, "%")
    #number of rows is the number of graphs divided by 4 and rounded up eg 7 = 2 rows
    rows <- ceiling(slctd/4)
    ##Dynamically create plot height  
    pltheight <- ifelse(rows <2, "600px",ifelse(rows>4,"275px",paste0(900/rows, "px")))
    inptLst <- as.list(gsub(" ", "",input$grphs3))
    ##Create however many columns and then rows as needed
    fluidRow(
      #split into columns based on no. selected indicators
      column(12/cls,map(1, function(nc){
      #This part selects graphs created above depending on the 
      #number of indicators e.g if 12 the map function will pull out
      #1,5,9 using the seq function
        plot_output_list1<- map(seq(from = nc,to = slctd,by = cls), function(x){
          tstNm1 <- inptLst[[x]]
          plotname <- paste("plot", tstNm1, sep = "_")
          plotOutput(plotname, height = pltheight)
        })
        do.call(tagList, plot_output_list1)         
      }) ),  
      column(12/cls,map(2, function(nc){
      #this does the same thing as above, but selectes the next set of indicators
      #e.g. with 12 it goes 2,6,10
      #tryCatch is needed because there will be an error if the number of columns
      #is less than 2 => I need it to return nothing in this case
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
 
##Render a UI with a certain number of rows and columns based on selected graphs
##Excpet this one is for page 2!  
  output$uiPage2 <- renderUI({
    slctd <- length(input$grphs2)
#number of columns is 4, unless there are less than 3 graphs
    cls <- if(slctd>3){4} else{slctd}
    #The percentage fo the space each columns will occupy
    pctCols <- 100/cls
    pctCols <- paste0(pctCols, "%")
 #number of rows is the number of graphs divided by 4 and rounded up eg 7 = 2 rows
    rows <- ceiling(slctd/4)
 ##Dynamically create plot height  
    pltheight <- ifelse(rows <2, "600px",ifelse(rows>4,"275px",paste0(900/rows, "px")))
    inptLst <- as.list(gsub(" ", "",input$grphs2))
##Create however many columns and then rows as needed
    fluidRow(
 #split into columns based on no. selected indicators
      column(12/cls,map(1, function(nc){
  #This part selects graphs created above depending on the 
   #number of indicators e.g if 12 the map function will pull out
  #1,5,9 using the seq function
        plot_output_list1<- map(seq(from = nc,to = slctd,by = cls), function(x){
          tstNm1 <- inptLst[[x]]
          plotname <- paste("FGplot", tstNm1, sep = "_")
          plotOutput(plotname, height = pltheight)
        })
        do.call(tagList, plot_output_list1)         
      }) ),  
      column(12/cls,map(2, function(nc){
   #this does the same thing as above, but selectes the next set of indicators
   #e.g. with 12 it goes 2,6,10
   #tryCatch is needed because there will be an error if the number of columns
  #is less than 2 => I need it to return nothing in this case
        plot_output_list2<- tryCatch(map(seq(from = nc,to = slctd,by = cls), function(x){
          tstNm2 <- inptLst[[x]]
          plotname <- paste("FGplot", tstNm2, sep = "_")
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
          plotname <- paste("FGplot", tstNm3, sep = "_")
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
          plotname <- paste("FGplot", tstNm4, sep = "_")
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

  ##Create Leaflet Maps=============================
  output$IZUI <- renderUI({
    selectizeInput("IZ", "", choices = sort(unique(CPPMapDta[CPPMapDta$council == input$CPP, 11])),
                options = list(placeholder = "Select a Community",
                               onInitialize = I('function() { this.setValue(""); }')))
  })
  
  clrs<-brewer.pal(7, "RdYlGn")
  povPal <- colorBin(rev(clrs), SpPolysDF@data$povDecs)
  tariffPal <- colorBin(clrs, SpPolysDF@data$tariffDecs)
  posPal <- colorBin(clrs, SpPolysDF@data$posDecs)
  benPal <- colorBin(rev(clrs), SpPolysDF@data$benDecs)
  crimePal <- colorBin(rev(clrs), SpPolysDF@data$crimeDecs)
  admisPal <- colorBin(rev(clrs), SpPolysDF@data$admisDecs)
  
  plydata<-reactive({
    desIZ<- which(CPPMapDta$council %in% input$CPP & CPPMapDta$IZname %in% input$IZ)
    IZ_dzs<-SpPolysDF[desIZ,]
  })
  
  #create the map
  output$newplot<-renderLeaflet({
    p<-leaflet(plydata())%>%
      # addProviderTiles("OpenStreetMap.HOT")%>% #Humanitarian OpenStreetMap if desired
      addTiles()%>%
      addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                  layerId = ~DataZone, fillColor = ~povPal(`povDecs`), color = "black")
    return(p)
  })
  output$newplot2<-renderLeaflet({
    p<-leaflet(plydata())%>%
      addTiles()%>%
      addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                  layerId = ~DataZone, fillColor = ~tariffPal(`tariffDecs`),  color = "black")
    return(p)
  })
  output$newplot3<-renderLeaflet({
    p<-leaflet(plydata())%>%
      addTiles()%>%
      addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                  layerId = ~DataZone, fillColor = ~posPal(`posDecs`), color = "black")
    return(p)
  })
  output$newplot4<-renderLeaflet({
    p<-leaflet(plydata())%>%
      addTiles()%>%
      addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                  layerId = ~DataZone, fillColor = ~benPal(`benDecs`), color = "black")
    return(p)
  })
  output$newplot5<-renderLeaflet({
    p<-leaflet(plydata())%>%
      addTiles()%>%
      addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                  layerId = ~DataZone, fillColor = ~crimePal(`crimeDecs`), color = "black")
    return(p)
  })
  output$newplot6<-renderLeaflet({
    p<-leaflet(plydata())%>%
      addTiles()%>%
      addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                  layerId = ~DataZone, fillColor = ~admisPal(`admisDecs`), color = "black")
    return(p)
  })
  
  ##Clickable popups for map1
  showDZPopup <- function(group, lat, lng) {
    selectedDZ <- CPPMapDta[CPPMapDta$DataZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedDZ$DataZone))),
      sprintf("%s: %s",
              "Children in Poverty (%)", round(unique(selectedDZ[13]),2)), tags$br()
    ))
    leafletProxy("newplot") %>% addPopups(lng, lat, content, layerId = group)
  }
  
  #Makes the popups appear and clears old popups
  observe({
    leafletProxy("newplot") %>% clearPopups()
    event <- input$newplot_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZPopup(event$id, event$lat, event$lng)
    })
  })
  
  ##Clickable popups for map2
  showDZPopup2 <- function(group, lat, lng) {
    selectedDZ <- CPPMapDta[CPPMapDta$DataZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedDZ$DataZone))),
      sprintf("%s: %s\n",
              "Tariff Score", round(unique(selectedDZ[14]),2)), tags$br()
    ))
    leafletProxy("newplot2") %>% addPopups(lng, lat, content, layerId = group)
  }
  
  #Makes the popups appear and clears old popups
  observe({
    leafletProxy("newplot2") %>% clearPopups()
    event <- input$newplot2_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZPopup2(event$id, event$lat, event$lng)
    })
  })
  ##Clickable popups for map3
  showDZPopup3 <- function(group, lat, lng) {
    selectedDZ <- CPPMapDta[CPPMapDta$DataZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedDZ$DataZone))),
      sprintf("%s: %s\n",
              "Positive Destinations (%)", round(unique(selectedDZ[15]),2)), tags$br()
    ))
    leafletProxy("newplot3") %>% addPopups(lng, lat, content, layerId = group)
  }
  
  #Makes the popups appear and clears old popups
  observe({
    leafletProxy("newplot3") %>% clearPopups()
    event <- input$newplot3_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZPopup3(event$id, event$lat, event$lng)
    })
  })
  ##Clickable popups for map4
  showDZPopup4 <- function(group, lat, lng) {
    selectedDZ <- CPPMapDta[CPPMapDta$DataZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedDZ$DataZone))),
      sprintf("%s: %s\n",
              "Out of Work Benefits (%)", round(unique(selectedDZ[16]),2)), tags$br()
    ))
    leafletProxy("newplot4") %>% addPopups(lng, lat, content, layerId = group)
  }
  
  #Makes the popups appear and clears old popups
  observe({
    leafletProxy("newplot4") %>% clearPopups()
    event <- input$newplot4_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZPopup4(event$id, event$lat, event$lng)
    })
  })
  ##Clickable popups for map5
  showDZPopup5 <- function(group, lat, lng) {
    selectedDZ <- CPPMapDta[CPPMapDta$DataZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedDZ$DataZone))),
      sprintf("%s: %s\n",
              "SIMD Crimes per 10,000", round(unique(selectedDZ[17]),2)), tags$br()
    ))
    leafletProxy("newplot5") %>% addPopups(lng, lat, content, layerId = group)
  }
  
  #Makes the popups appear and clears old popups
  observe({
    leafletProxy("newplot5") %>% clearPopups()
    event <- input$newplot5_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZPopup5(event$id, event$lat, event$lng)
    })
  })
  ##Clickable popups for map6
  showDZPopup6 <- function(group, lat, lng) {
    selectedDZ <- CPPMapDta[CPPMapDta$DataZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedDZ$DataZone))),
      sprintf("%s: %s\n",
              "Emergency Admissions per 100,000", round(unique(selectedDZ[18]),2)), tags$br()
    ))
    leafletProxy("newplot6") %>% addPopups(lng, lat, content, layerId = group)
  }
  
  #Makes the popups appear and clears old popups
  observe({
    leafletProxy("newplot6") %>% clearPopups()
    event <- input$newplot6_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZPopup6(event$id, event$lat, event$lng)
    })
  })
  
  #Colours for Community Map
  communityPal <- colorBin(clrs, SpPolysIZ@data$rank_decs)
  
  #Subset IZ Data
  IZPlys <- reactive({
    sbst <- which(SpPolysIZ@data$council %in% input$CPPIZ)
    dt <- SpPolysIZ[sbst,]
  })
  
  #Create Community Map
  output$communityMap <- renderLeaflet({
    cp <- leaflet(IZPlys()) %>%
      addTiles() %>%
      addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                  layerId = ~InterZone, fillColor = ~communityPal(`rank_decs`), color = "black")
  })
  #Add click function
  showIZPopup <- function(group, lat, lng){
    selectedIZ <- SpPolysIZ@data[SpPolysIZ@data$InterZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedIZ$`IGZ name`))),
      paste("Intermediate Geography Ranking:", as.character(unique(selectedIZ[12]))),
      tags$br()
    ))
    leafletProxy("communityMap") %>% addPopups(lng, lat, content, layerId = group)
  }
  #Make popup appear and clear old popups
  observe({
    leafletProxy("communityMap") %>% clearPopups()
    event <- input$communityMap_shape_click
    if(is.null(event)){
      return()}
    isolate({
      showIZPopup(event$id, event$lat, event$lng)
    })
    
  })
  
##All communities per indicator==================================
##Firstly render all of the plots for filling in the UI rendered below 
  myheight <- function(){
    nrow(unique(IGZdta[IGZdta$CPP== input$`CPP-AllC`,"InterZone_Name"]))*60
  }
  output$AllCPlots <- renderPlot({
    dta <- IGZdta[IGZdta$CPP== input$`CPP-AllC` & IGZdta$Indicator==input$`Indi-AllC`&IGZdta$Type != "Projected",c(2,8,9)]
    nComs <- length(unique(dta$InterZone_Name))
    comList <- unique(dta$InterZone_Name)
    dta2 <- CPPdta[CPPdta$CPP %in% input$`CPP-AllC`& CPPdta$Indicator==input$`Indi-AllC`&CPPdta$Type != "Projected",c(1,4,5)]
    dta3 <- CPPdta[CPPdta$CPP %in% "Scotland"& CPPdta$Indicator==input$`Indi-AllC`&CPPdta$Type != "Projected",c(1,4,5)]
    colnames(dta2) <- colnames(dta)
    colnames(dta3) <- colnames(dta)
    dta <- rbind(dta, dta2, dta3)
    dta$colourscheme <-ifelse(dta$InterZone_Name == "Scotland","Scot",ifelse(dta$InterZone_Name == input$`CPP-AllC`,"CPP","Com"))
    yrs <- c(dta$Year[[1]], dta$Year[[length(dta$Year)]])
    ##lapply to generate plots
    plts <- list()
    plts <-lapply(1:nComs, FUN = function(.x){
      ggplot(data = dta[dta$InterZone_Name %in% c(comList[.x], input$`CPP-AllC`, "Scotland"),])+
        geom_line(aes(x = Year, y = value, group = colourscheme, colour = colourscheme), size = 1.5)+
        theme_bw()+
        ggtitle(comList[.x])+
        theme(axis.text.x =  element_text(angle = 90, vjust = 0, hjust = 1))+
        ylab("")+xlab("")+
        scale_x_discrete(breaks = yrs, expand = c(0.01,0.01))+
      scale_color_manual(breaks = c("Com", "CPP", "Scot") ,values = c("red", "green","blue"))+
      guides(colour = FALSE)
    })
    do.call("plot_grid", c(plts, ncol = 4))
  }, height = myheight)
})
