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
  
})
