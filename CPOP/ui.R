shinyUI(navbarPage("CPOP",
 tabPanel("Cover Page/Contents",
          includeHTML("C:/Users/cassidy.nicholas/OneDrive - IS/CommunityPlanningOutcomesProfiler/CoverPage.html"),
          img(src = "http://www.improvementservice.org.uk/benchmarking/images/islogo.png", align = "top")),
 tabPanel("CPP - Page1",
          fluidPage(
            fluidRow(
              column(6,
                     selectInput("LA1", "Select A Local Authority", unique(filter(CPPdta, CPP != "Scotland"))[[1]], 
                                 selected = "Aberdeen City")
              ),
              column(6,
                     selectInput("CompLA1", "Select Comparator", unique(CPPdta$CPP), selected = "Scotland"
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
                     selectInput("LA2", "Select A Local Authority", unique(filter(CPPdta, CPP != "Scotland")[[1]]), 
                                 selected = "Aberdeen City")
              ),
              column(6,
                     selectInput("CompLA2", "Select Comparator", unique(CPPdta$CPP), selected = "Scotland"
                     )
              )
            ),
            hr(),
            fluidRow(
              mainPanel(
                plotOutput("Plot2")
              )
            )
          )),
 tabPanel("CPP - Page3",
  fluidPage(
    fluidRow(
      column(4,
      selectInput("LA3", "Select A Local Authority", unique(filter(CPPdta, CPP != "Scotland")[[1]]), 
                  selected = "Aberdeen City")
      ),
      column(4,
             selectInput("CompLA3", "Select Comparator", unique(CPPdta$CPP), selected = "Scotland"
              )
             ),
      column(4,
             div(style = "column-count:2;-webkit-column-count:2; -moz-column-count:2",
             checkboxGroupInput("grphs3","", unique(CPPdta$Indicator))
             )
      )
    ),
    hr(),
    uiOutput("uiPage3")
  )
 )
))
