library(shiny)

shinyUI(navbarPage("CPOP",
 tabPanel("Cover Page/Contents",
          includeHTML("C:/Users/connachan.cara/Documents/CommunityPlanningOutcomesProfiler/CoverPage.html"),
          img(src = "http://www.improvementservice.org.uk/benchmarking/images/islogo.png", align = "top")),
 tabPanel("CPP - Page1",
          fluidPage(
            fluidRow(
              column(4,
                     selectInput("LA1", "Select A Local Authority", unique(filter(CPPdta, CPP != "Scotland"))[[1]], 
                                 selected = 1)
                     ),
              column(4,
                     selectInput("CompLA1", "Select Comparator", unique(CPPdta$CPP), selected = "Scotland")
                     ),
              column(4,
                     selectInput("Indi1", "Select Indicator", c("All", unique(CPPdta$Indicator)), selected = "All")
                     ),
              column(2, tags$hr(style="border-color: red;")
              ),
              column(2
                     ),
              column(2, tags$hr(style="border-color: #48CCCD;")
                     )
            ),
            hr(),
            conditionalPanel(
              condition = "input.Indi1 == 'All'",
              fluidRow(
                column(3, plotOutput("plot_1")),
                column(3, plotOutput("plot_2")),
                column(3, plotOutput("plot_3")),
                column(3, plotOutput("plot_4"))
              ),
            fluidRow(
              column(3, plotOutput("plot_5")),
              column(3, plotOutput("plot_6")),
              column(3, plotOutput("plot_7")),
              column(3, plotOutput("plot_8"))
            ),
            fluidRow(
              column(3, plotOutput("plot_9")),
              column(3, plotOutput("plot_10")),
              column(3, plotOutput("plot_11")),
              column(3, plotOutput("plot_12"))
            ),
            fluidRow(
              column(3, plotOutput("plot_13")),
              column(3, plotOutput("plot_14")),
              column(3, plotOutput("plot_15")),
              column(3, plotOutput("plot_16"))
            ),
            fluidRow(
              column(3, plotOutput("plot_17")),
              column(3, plotOutput("plot_18"))
            )
            ),
            conditionalPanel(
              condition = "input.Indi1 != 'All'",
              mainPanel(
              plotOutput("Indi1Plot")
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
