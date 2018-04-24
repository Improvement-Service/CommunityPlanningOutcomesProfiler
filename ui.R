shinyUI(navbarPage("CPOP",
                   
 tabPanel("Cover Page/Contents",
          #css for checkbox
              tags$head(tags$style(
              HTML(".checkbox-inline {margin-left:0px !important; margin-right:10px}
                   .chckBx{-webkit-column-count:6;
                   -moz-column-count:6; 
                   column-count:6;
                   text-align:left;
                   display:block}
                   .col-sm-6{padding-left:0px};"))),
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
              ),
              column(12,div(class = "chckBx",
                     checkboxGroupInput("grphs2", unique(CPPdta$Indicator), inline = TRUE, label = NULL)
                      )
                     ) 
            ),
            hr(),
            uiOutput("uiPage2")
          )),
 tabPanel("CPP - Page3",
  fluidPage(
    fluidRow(
      column(6,
      selectInput("LA3", "Select A Local Authority", unique(filter(CPPdta, CPP != "Scotland")[[1]]), 
                  selected = "Aberdeen City")
      ),
      column(6,
             selectInput("CompLA3", "Select Comparator", unique(CPPdta$CPP), selected = "Scotland"
              )
             ),
      column(12,
             div(class = "chckBx",
             checkboxGroupInput("grphs3",label = NULL, unique(CPPdta$Indicator), inline = TRUE)
             )
      )
    ),
    hr(),
    uiOutput("uiPage3")
  )
 )
))
