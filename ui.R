shinyUI(navbarPage("CPOP",
 tabPanel("Cover Page/Contents",
          includeHTML("CoverPage.html"),
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
             #div(style = "column-count:2;-webkit-column-count:2; -moz-column-count:2",
             checkboxGroupInput("grphs3","", unique(CPPdta$Indicator))
             #)
      )
    ),
    hr(),
    uiOutput("uiPage3")
  )
 ),
 tabPanel("My Communities Page - Page 4",
  fluidPage(
    tags$head(
      tags$style(HTML("
                      
                      .multicol {
                      
                      -webkit-column-count: 3; /* Chrome, Safari, Opera */
                      
                      -moz-column-count: 3; /* Firefox */
                      
                      column-count: 3;
                      
                      }
                      
                      "))
      
      ),
    fluidRow(
      column(6,
             selectInput("LA4", "Select a Local Authority", unique(filter(IGZdta, CPP != "Scotland"))[[3]], 
                         selected = 1, width = "600"),
             radioButtons("View","Select Display",c("All", "Top/bottom 10", "Top/bottom 5"),inline = TRUE)),
      column(5,
             tags$div(class = "multicol",checkboxGroupInput("Indi4","Select Indicators", unique(IGZdta$Indicator),selected = unique(IGZdta$Indicator)))),
      column(1,
             actionButton("IndiAll","Select All"),
             actionButton("IndiClear", "Clear All"))
    ),
    fluidPage(
      fluidRow(
        column(1,
               tags$img(src = "Arrow1.png")),
        column(10,
               DT::dataTableOutput("MyCommunitiesTbl")
               ),
        column(1,
               tags$img(src = "Arrow2.png"))
      )
      
    )
    
  )        
          


   
 )
))
