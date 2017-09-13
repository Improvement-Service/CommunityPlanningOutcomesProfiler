shinyServer(function(input, output) {
   
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
    plotname <- paste("plot", my.i, sep ="_")
    output[[plotname]] <- renderPlot(
      plot(x = my.i, y = 6, main = plotname)
    )
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
  for(i in 1: cls){
    column((12/cls),
   plot_output_list<- lapply(seq(i,length(input$grphs3),cls), function(x){
             plotname <- paste("plot", x, sep = "_")
           plotOutput(plotname)
     }) 
    )
    do.call(tagList, plot_output_list)
           
 #          for(j in rows){
#             pnum <- i+(j*cls)
#             plotOutput(paste("plot", pnum, sep = "_"))
 #          })
  }
  })
  
  
})
