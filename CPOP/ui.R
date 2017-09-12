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
    fluidRow(
      column(6,
      selectInput("LA", "Select A Local Authority", c("Aberdeen City", "Some Other Council"))
      ),
      column(6,
             selectInput("CompLA", "Comparator", c("Edinburgh", "Glasgow")
              )
             )
    ),
    hr(),
    # Show a plot of the generated distribution
    fluidRow(
    mainPanel(
       p("PUT PLOT(S) HERE!")
      )
    )
  )
 )
))
