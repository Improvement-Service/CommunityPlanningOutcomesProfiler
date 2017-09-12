library(shiny)

shinyUI(navbarPage("CPOP",
 tabPanel("Cover Page/Contents",
          includeHTML("C:/Users/cassidy.nicholas/OneDrive - IS/CommunityPlanningOutcomesProfiler/CoverPage.html"),
          img(src = "http://www.improvementservice.org.uk/benchmarking/images/islogo.png", align = "top")),
 tabPanel("CPP - Page1",
          fluidPage(
            fluidRow(
              column(6,
                     selectInput("LA1", "Select A Local Authority", c("Aberdeen City", "Some Other Council"))
              ),
              column(6,
                     selectInput("CompLA1", "Comparator", c("Edinburgh", "Glasgow")
                     )
              )
            ),
            hr(),
            fluidRow(
              mainPanel(
                p("PUT PLOT(S) HERE!")
              )
            )
          )),
 tabPanel("CPP -Page2",
          fluidPage(
            fluidRow(
              column(6,
                     selectInput("LA2", "Select A Local Authority", c("Aberdeen City", "Some Other Council"))
              ),
              column(6,
                     selectInput("CompLA2", "Comparator", c("Edinburgh", "Glasgow")
                     )
              )
            ),
            hr(),
            fluidRow(
              mainPanel(
                p("PUT PLOT(S) HERE!")
              )
            )
          )),
 tabPanel("CPP - Page3",
  fluidPage(
    fluidRow(
      column(6,
      selectInput("LA3", "Select A Local Authority", c("Aberdeen City", "Some Other Council"))
      ),
      column(6,
             selectInput("CompLA3", "Comparator", c("Edinburgh", "Glasgow")
              )
             )
    ),
    hr(),
    fluidRow(
    mainPanel(
       p("PUT PLOT(S) HERE!")
      )
    )
  )
 )
))
