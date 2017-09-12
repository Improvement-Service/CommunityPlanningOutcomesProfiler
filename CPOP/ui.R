library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage("CPOP",
 tabPanel("Cover Page/Contents",
          includeHTML("C:/Users/cassidy.nicholas/OneDrive - IS/CommunityPlanningOutcomesProfiler/CoverPage.html"),
          img(src = "http://www.improvementservice.org.uk/benchmarking/images/islogo.png", align = "top")),
 tabPanel("CPP - Page1"),
 tabPanel("CPP -Page2"),
 tabPanel("CPP - Page3",
  fluidPage(
    absolutePanel(fixed = FALSE, draggable = FALSE, top = "40px", left = 0, right = 0,
                  bottom = "10px", width = "100%", height = "0px", 
        wellPanel(
      selectInput("LA", "Select A Local Authority", c("Aberdeen City", "Some Other Council"))
        )
    ),
    # Show a plot of the generated distribution
    fluidRow(
    mainPanel(
       p("PUT A PLOT HERE!")
      )
    )
  )
 )
))
