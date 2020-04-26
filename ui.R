
fluidPage(
  theme=shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
          .navbar .navbar-header {float: right}
        ")) 
  ),
  navbarPage(
    title="Sustainable Energy Around the World; 1990-2015",
    id="nav",
    position="fixed-top",
    collapsible=TRUE,
    
    tabPanel("WORLD MAP", icon=icon('globe'),
             br(),
             br(),
             br(),
             fluidRow(h1("Global Renewable Energy")),
             fluidRow(
               column(3,
                      br(),
                      "Explore data from the World Bank's",
                      br(),
                      "Sustainable Energy for All Initiative!",
                      br(),
                      br(),
                      "Browse by Region, Country and Income Group.",
                      br(),
                      "Hover over maps and plots for details."
                      ),
               column(2,
                      br(),
                      wellPanel(h4(strong("Top Countries By")),
                                h4(strong(htmlOutput("worldmap_stats_data"))))
                      ),
               column(2,
                      br(),
                      wellPanel(h4(strong(htmlOutput("worldmap_stats_1"))),
                                h4(strong(htmlOutput("worldmap_stats_12")))
                                )
                      ),
               column(2,
                      br(),
                      wellPanel(h4(strong(htmlOutput("worldmap_stats_2"))),
                                h4(strong(htmlOutput("worldmap_stats_22")))
                                )
                      ),
               column(2,
                      br(),
                      wellPanel(h4(strong(htmlOutput("worldmap_stats_3"))),
                                h4(strong(htmlOutput("worldmap_stats_32")))
                                )
                      )
               ),
             fluidRow(
               column(3,
                      br(),
                      sliderInput(
                        inputId="worldmap_year",
                        label="Select Year:",
                        min=1990, max=2015,
                        value=2015,
                        sep=""
                      ),
                      radioButtons(
                        inputId="worldmap_data",
                        label="Select Data:",
                        choices=list("Renewable Share, Electricity Output (%)"="Share.Output",
                                     "Renewable Share, TFEC (%)"="Share.TFEC",
                                     "Renewable Electricity Output (GWh)"="Renewable.Output",
                                     "Renewable TFEC (GWh)"="Renewable.TFEC")
                      ),
                      "TFEC = Total Final Energy Consumption"),
               column(8,
                      plotlyOutput("worldmap2"))
               )

             ), #end of HOME tabPanel
    
    navbarMenu("EXPLORE", icon=icon('compass'),
               
               tabPanel("Region",
                        br(),
                        br(),
                        br(),
                        fluidRow(h1("Renewable Energy Insights by Region")),
                        fluidRow(
                          column(2,
                                 br(),
                                 radioButtons(
                                   inputId="region_data",
                                   label="Select Data:",
                                   choices=list("Share of Output as Renewable (%)"="Share.Output",
                                                "Share of TFEC as Renewable (%)"="Share.TFEC")
                                 ),
                                 br(),
                                 selectizeInput(
                                   inputId="region_region",
                                   label="Select Region:",
                                   choices=unique(sort(clean$Subregion))
                                 )
                                 ),
                          column(10,
                                 br(),
                                 br(),
                                 plotlyOutput("region_share"))
                        )
                        ), #end of Regional Zoom tabPanel
               tabPanel("Country",
                        br(),
                        br(),
                        br(),
                        fluidRow(h1("Renewable Energy Insights by Country")),
                        fluidRow(
                          column(2,
                                 br(),
                                 selectizeInput(
                                   inputId="country",
                                   label="Select Country:",
                                   choices=unique(clean$Country)
                                 )
                                 ),
                          column(5,
                                 br(),
                                 plotlyOutput("country_share"),
                                 plotlyOutput("country_electricity")
                                 ),
                          column(5,
                                 br(),
                                 plotlyOutput("country_total"),
                                 plotlyOutput("country_gdppc")
                                 )
                          )
                        ),  #end of Country Zoom tabPanel
               tabPanel("Income Group",
                        br(),
                        br(),
                        br(),
                        fluidRow(h1("Renewable Energy Insights by Income Level")),
                        fluidRow(
                          column(3,
                                 br(),
                                 radioButtons(
                                   inputId="income_data",
                                   label="Select Data:",
                                   choices=list("Renewable Share, Electricity Output (%)"="Share.Output",
                                                "Renewable Share, TFEC (%)"="Share.TFEC",
                                                "Renewable Electricity Output (GWh)"="Renewable.Output",
                                                "Renewable TFEC (GWh)"="Renewable.TFEC")
                                 )),
                          column(2,
                                 br(),
                                 wellPanel(h4(strong("Top Countries By")),
                                           h4(strong(htmlOutput("income_stats_data"))))
                                 ),
                          column(2,
                                 br(),
                                 wellPanel(h4(strong(htmlOutput("income_stats_1"))),
                                           h4(strong(htmlOutput("income_stats_12")))
                                 )
                                 ),
                          column(2,
                                 br(),
                                 wellPanel(h4(strong(htmlOutput("income_stats_2"))),
                                           h4(strong(htmlOutput("income_stats_22")))
                                 )
                                 ),
                          column(2,
                                 br(),
                                 wellPanel(h4(strong(htmlOutput("income_stats_3"))),
                                           h4(strong(htmlOutput("income_stats_32")))
                                 )
                                 )
                        ),
                        fluidRow(
                          column(3,
                                 sliderInput(
                                   inputId="income_year",
                                   label="Select Year:",
                                   min=1990, max=2015,
                                   value=2015,
                                   sep=""
                                 ),
                                 br(),
                                 selectizeInput(
                                   inputId="income_group",
                                   label="Select Income Group:",
                                   choices=unique(clean$Income.Group)
                                 )
                                 ),
                          column(8,
                                 plotOutput("income_hist"))
                                 )
                        ),  #end of Income Zoom tabPanel
               tabPanel("Top Countries",
                        br(),
                        br(),
                        br(),
                        fluidRow(h1("Top Countries for Renewable Energy")),
                        fluidRow(
                          column(3,
                                 br(),
                                 radioButtons(
                                   inputId="top_data",
                                   label="Select Data:",
                                   choices=list("Renewable Share, Electricity Output (%)"="Share.Output",
                                                "Renewable Share, TFEC (%)"="Share.TFEC",
                                                "Renewable Electricity Output (GWh)"="Renewable.Output",
                                                "Renewable TFEC (GWh)"="Renewable.TFEC")
                                 ),
                                 br(),
                                 sliderInput(
                                   inputId="top_year",
                                   label="Select Year:",
                                   min=1990, max=2015,
                                   value=2015,
                                   sep=""
                                 ),
                                 br(),
                                 sliderInput(
                                   inputId="top_number",
                                   label="Select Number of Countries:",
                                   min=5, max=50,
                                   value=10,
                                   sep="",
                                   step=5
                                 )),
                          column(4,
                                 br(),
                                 h4("Top Countries by Region"),
                                 plotlyOutput("top_region"),
                                 ),
                          column(4,
                                 br(),
                                 h4("Top Countries by Income Group"),
                                 plotlyOutput("top_income"))
                        )
                        )   #end of Top __% tabPanel
               
               ), #end of EXPLORE navbarMenu
    
    tabPanel("DATA", icon=icon("table"),
             br(),
             br(),
             br(),
             h3("All data courtesy of the World Bank Open Data Catalog."),
             "See below for links to individual datasets, and the combined dataset used for this site.",
             br(),
             br(),
             uiOutput("renewable"),
             uiOutput("gdp"),
             uiOutput("pop"),
             br(),
             br(),
             fluidRow(
               column(12,
                      DT::dataTableOutput("table"))
             ),
             br()
             ), #end of DATA tabPanel
    
    tabPanel("ABOUT ME", icon=icon('user'),
             br(),
             fluidRow(
               column(3,
                      br(),
                      br(),
                      br(),
                      img(src="eel.jpg", width="100%")),
               column(4,
                      br(),
                      br(),
                      h3(strong("Zack Zbar")),
                      h5("zackzbar@gmail.com"),
                      tags$a(
                        href="https://www.linkedin.com/in/zackzbar/",
                        img(
                          src="linkedin.png",
                          title="linkedin",
                          height="40px"
                        )
                      ),
                      tags$a(
                        href="https://github.com/zackzbar",
                        img(
                          src="github.png",
                          title="github",
                          height="40px"
                        )
                      ),
                      br(),
                      br(),
                      "I believe insight",
                      br(),
                      "from information",
                      br(),
                      "is essential",
                      br(),
                      "if we are to keep",
                      br(),
                      "this beautiful world.")),
             br(),
             br()
             ) #end of ABOUT ME tabPanel
    
  )
)