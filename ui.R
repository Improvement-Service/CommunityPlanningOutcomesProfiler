shinyUI(navbarPage("CPOP",
                   
 tabPanel("Cover Page/Contents",
          #css for checkbox
              tags$head(tags$style(
                ".leaflet{height:38vh !important; border-style:solid; border-width:1px; margin-top:1px}",
                ".well {background-color:white; padding-bottom:0px; height:0vh;}",
                ".row-fluid {padding-top:7vh;}",
                ".span4 {display: inline-block; vertical-align: text-top; width: 35vw;}",
                "#communityMap{height:85vh !important;border-style:solid;border-width:1px; padding-top:8px}",
              HTML(".checkbox-inline {margin-left:0px !important; margin-right:10px}
                   .chckBx{-webkit-column-count:6;
                   -moz-column-count:6; 
                   column-count:6;
                   text-align:left;
                   display:block}
                   .col-sm-6{padding-left:0px};
                   h5{height: 18px;
                       margin-top:2px;
                       margin-bottom:0px;
                       text-align:center;
                       font-weight: bold;}
                   h4 {
                      font-size:12px;
                      height: 18px;
                      margin-top:2px;
                      margin-bottom:0px;
                      text-align:center;
                      font-weight: bold
                      }"))),
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
              column(4,
                     selectInput("LA2", "Select A Local Authority", unique(filter(CPPdta, CPP != "Scotland")[[1]]), 
                                 selected = "Aberdeen City")
              ),
              column(4,
                     selectInput("CompLA2", "Select Comparator", unique(CPPdta$CPP), selected = "Scotland"
                     )
              ),
              column(4,
                     actionButton("selAll2", "All"),
                     actionButton("selNone2", "None")),
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
      column(4,
      selectInput("LA3", "Select A Local Authority", unique(filter(CPPdta, CPP != "Scotland")[[1]]), 
                  selected = "Aberdeen City")
      ),
      column(4,
             selectInput("CompLA3", "Select Comparator", unique(CPPdta$CPP), selected = "Scotland"
              )
             ),
      column(4,
             actionButton("selAll3", "All"),
             actionButton("selNone3", "None")),
      column(12,
             div(class = "chckBx",
             checkboxGroupInput("grphs3",label = NULL, unique(CPPdta$Indicator), inline = TRUE)
             )
      )
    ),
    hr(),
    uiOutput("uiPage3")
  )
 ),
 navbarMenu("Maps", icon = icon("globe"),
            tabPanel("CPP Communities",
                     fluidPage(
  absolutePanel(fixed = FALSE, draggable = FALSE, top = "28px", left = 0, right = 0,
                                     bottom = 0, width = "100%", height = "0px", 
                                     wellPanel(div(class = "span4",style = "padding-left:6vh", 
                         selectizeInput("CPPIZ", "", unique(CPPMapDta$council),
                                        options = list(placeholder = "Select a CPP",
                                        onInitialize = I('function() { this.setValue(""); }')), 
                                        width = "350px")))),
    fluidRow(div(class = "row-fluid", leafletOutput("communityMap")))
                     )
            ),
            tabPanel("Data Zones",
        fluidPage(
                       absolutePanel(fixed = FALSE,
                            draggable = FALSE, top = "28px", left = 0, right = 0, bottom = 0,
                            width = "100%", height = "0px", style = "opacity:1",
                            wellPanel(
                              div(class = "row",
                             div(class = "span4",style = "padding-right:6vh; padding-left:6vh", 
                         selectizeInput("CPP", "",
                                choices = unique(CPPMapDta$council), options = list(placeholder = "Select a CPP",
                               onInitialize = I('function() { this.setValue(""); }')))
                             ),
                                div(class = "span4",
                                   uiOutput("IZUI")))
                                     )),
    conditionalPanel("input.CPP != 'Select a CPP'", div(class = "row-fluid",
      fluidRow(splitLayout(cellWidths = c("33%", "33%", "33%"),
          h4("Percentage of Children in Poverty"), h4("S4 Average Tariff Score"), h4("% School Leavers Entering Positive Destinations"))
                                         ),
      fluidRow(splitLayout(cellWidths = c("33%", "33%", "33%"),
              leafletOutput("newplot"), leafletOutput("newplot2"), 
              leafletOutput("newplot3")))
                       ), 
       hr(style = "margin-bottom:0.3vh; margin-top:0.5vh")),
    conditionalPanel("input.CPP != 'Select a CPP'",
        fluidRow(
         splitLayout(cellWidths = c("33%", "33%", "33%"),
              h4("% Aged 16-64 Receiving Out of Work Benefits"), 
              h4("Number of SIMD Crimes per 10,000 People"), 
              h4("Emergency Admissions (65+) per 100,000 People"))
            ),  
         fluidRow(
             splitLayout(cellWidths = c("33%", "33%", "33%"),
                      leafletOutput("newplot4"), 
                      leafletOutput("newplot5"), 
                      leafletOutput("newplot6")))
      )
                                      
         )
            ))
))