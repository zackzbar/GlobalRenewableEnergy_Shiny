library(shiny)
library(shinythemes)

clean = read.csv("./data/Energy/clean5.csv")

fluidPage(
  theme=shinytheme("darkly"),
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
                      "(SELECTION INPUTS)",
                      br(),
                      br(),
                      "14. (STATS BOX)"),
               column(8,
                      br(),
                      "15. (WORLD MAP)")
             )
             
             ), #end of HOME tabPanel
    
    navbarMenu("EXPLORE", icon=icon('compass'),
               
               tabPanel("Regional Maps",
                        br(),
                        br(),
                        br(),
                        fluidRow(h1("Regional Renewable Energy")),
                        fluidRow(
                          column(3,
                                 br(),
                                 "(SELECTION INPUTS)",
                                 br(),
                                 br(),
                                 "16. (STATS BOX)"),
                          column(8,
                                 br(),
                                 "17. (REGIONAL MAP)")
                        )
                        ), #end of Regional Maps tabPanel
               tabPanel("Regional Zoom",
                        br(),
                        br(),
                        br(),
                        fluidRow(h1("Renewable Energy Insights by Region")),
                        fluidRow(
                          column(3,
                                 br(),
                                 "(SELECTION INPUTS)",
                                 br(),
                                 br(),
                                 "1. (STATS BOX)"),
                          column(4,
                                 br(),
                                 "2. (LINE CHART)"),
                          column(4,
                                 br(),
                                 "3. (HISTOGRAM)")
                        )
                        ), #end of Regional Maps tabPanel
               tabPanel("Country Zoom",
                        br(),
                        br(),
                        br(),
                        fluidRow(h1("Renewable Energy Insights by Country")),
                        fluidRow(
                          column(3,
                                 br(),
                                 "(SELECTION INPUTS)"),
                          column(4,
                                 br(),
                                 "4. (STATS BOX)",
                                 br(),
                                 br(),
                                 "5. (GDPPC & URBAN POP OVER TIME)"),
                          column(4,
                                 br(),
                                 "6. (RENEWABLE SHARE/OUTPUT OVER TIME)",
                                 br(),
                                 br(),
                                 "7. (ACCESS TO ELECTRICITY OVER TIME)")
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
                                 "(SELECTION INPUTS)",
                                 br(),
                                 br(),
                                 "8. (STATS BOX)"),
                          column(4,
                                 br(),
                                 "9. (LINE CHART)"),
                          column(4,
                                 br(),
                                 "10. (HISTOGRAM)")
                        )
                        ),  #end of Income Zoom tabPanel
               tabPanel("Top __%",
                        br(),
                        br(),
                        br(),
                        fluidRow(h1("Top __% of Countries by Share of Renewable Energy")),
                        fluidRow(
                          column(3,
                                 br(),
                                 "(SELCTION INPUTS)",
                                 br(),
                                 br(),
                                 "11. (STATS BOX)"),
                          column(4,
                                 br(),
                                 "12. (PIE CHART - REGION)"),
                          column(4,
                                 br(),
                                 "13. (PIE CHART - INCOME LEVEL)")
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