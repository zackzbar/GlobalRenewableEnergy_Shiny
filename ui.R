library(shiny)
library(shinythemes)

clean = read.csv("./data/Energy/clean10.csv")

fluidPage(
  theme=shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
          .navbar .navbar-header {float: right}
        ")) #.navbar .navbar-nav {float: right} -- this went above the other HTML thing to in original paste
            #play around with the combo of these (and taking out everything from tags$head) to see what works
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
                      radioButtons(
                        inputId="worldmap_data",
                        label="Select Data:",
                        choices=list("Output"="Share.Output",
                                     "Consumption"="Share.Consumption")
                      ),
                      sliderInput(
                        inputId="worldmap_year",
                        label="Select Year:",
                        min=1990, max=2015,
                        value=2015,
                        sep=""
                      ),
                      br(),
                      br(),
                      "14. (STATS BOX)"),
               column(9,
                      br(),
                      htmlOutput("worldmap"))
             )
             
             ), #end of HOME tabPanel
    
    navbarMenu("EXPLORE", icon=icon('compass'),
               
               tabPanel("Regional Zoom",
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
                                   choices=list("Output"="Share.Output",
                                                "Consumption"="Share.Consumption")
                                 ),
                                 br(),
                                 sliderInput(
                                   inputId="region_year",
                                   label="Select Year for Map:",
                                   min=1990, max=2015,
                                   value=2015,
                                   sep=""
                                 ),
                                 br(),
                                 selectizeInput(
                                   inputId="region_region",
                                   label="Select Region:",
                                   choices=unique(sort(clean$Subregion))
                                 )
                                 ),
                          column(5,
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 htmlOutput("region_map")),
                          column(5,
                                 br(),
                                 br(),
                                 plotlyOutput("region_share"))
                        )
                        ), #end of Regional Zoom tabPanel
               tabPanel("Country Zoom",
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
                                 ),
                                 sliderInput(
                                   inputId="country_year",
                                   label="Select Year for Stats Box:",
                                   min=1990, max=2015,
                                   value=2015,
                                   sep=""
                                 )
                                 ),
                          column(5,
                                 br(),
                                 "4. (STATS BOX)",
                                 br(),
                                 br()
                                 ),
                          column(5,
                                 br(),
                                 plotlyOutput("country_share")
                                 )
                          ),
                          fluidRow(
                            column(2),
                            column(5,
                                   plotlyOutput("country_econ")
                                  ),
                            column(5,
                                   plotlyOutput("country_electricity"))
                          )
                        ),  #end of Country Zoom tabPanel
               tabPanel("Income Zoom",
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
                                   choices=list("Output"="Share.Output",
                                                "Consumption"="Share.Consumption")
                                 ),
                                 sliderInput(
                                   inputId="income_year",
                                   label="Select Year:",
                                   min=1990, max=2015,
                                   value=2015,
                                   sep=""
                                 )),
                          column(8,
                                 br(),
                                 "8. (STATS BOX)")
                          ),
                        fluidRow(
                          column(3,
                                 selectizeInput(
                                   inputId="income_group",
                                   label="Select Income Group:",
                                   choices=unique(clean$Income.Group)
                                 )),
                          column(8,
                                 plotlyOutput("income_hist"))
                          )
                        ),  #end of Income Zoom tabPanel
               tabPanel("Top Countries",
                        br(),
                        br(),
                        br(),
                        fluidRow(h1("Top Countries by Share of Renewable Energy")),
                        fluidRow(
                          column(3,
                                 br(),
                                 radioButtons(
                                   inputId="top_data",
                                   label="Select Data:",
                                   choices=list("Output"="Share.Output",
                                                "Consumption"="Share.Consumption")
                                 ),
                                 sliderInput(
                                   inputId="top_year",
                                   label="Select Year:",
                                   min=1990, max=2015,
                                   value=2015,
                                   sep=""
                                 ),
                                 sliderInput(
                                   inputId="top_number",
                                   label="Select Number of Countries:",
                                   min=5, max=50,
                                   value=10,
                                   sep="",
                                   step=5
                                 ),
                                 br(),
                                 br(),
                                 "11. (STATS BOX)"),
                          column(4,
                                 br(),
                                 h4("Regions"),
                                 plotlyOutput("top_region"),
                                 ),
                          column(4,
                                 br(),
                                 h4("Income Groups"),
                                 plotlyOutput("top_income"))
                        )
                        )   #end of Top __% tabPanel
               
               ), #end of EXPLORE navbarMenu
    
    tabPanel("DATA", icon=icon("table")
             
             ), #end of DATA tabPanel
    
    tabPanel("ABOUT ME", icon=icon('user'),
             br(),
             br(),
             br(),
             br(),
             "I believe insight from information is essential if we are to keep this beautiful world."
             
             ) #end of ABOUT ME tabPanel
    
  )
)