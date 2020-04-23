
function(input, output) {
  
  
  
  ### DATA TABLE
  output$table = DT::renderDataTable({
    datatable(clean, rownames = F) 
  })
  
  
  
  ### DATA LINKS
  url_renewable = a("SE4ALL Data", href="https://datacatalog.worldbank.org/dataset/sustainable-energy-all")
  url_gdp = a("GDP Data", href="https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?end=2018&start=1990&view=chart")
  url_pop = a("Population Data", href="https://data.worldbank.org/indicator/sp.pop.totl?end=2015&start=1990")
  
  output$renewable <- renderUI({
    tagList("Renewable Data Link:", url_renewable)
  })
  
  output$gdp <- renderUI({
    tagList("GDP Data Link:", url_gdp)
  })
  
  output$pop <- renderUI({
    tagList("Population Data Link:", url_pop)
  })
  
  
  
  

  ### 2. REGION MAP
  output$region_map = renderGvis({
    
    region_code=ifelse(clean[clean$Subregion==input$region_region,22][1]>100,
                       as.character(clean[clean$Subregion==input$region_region,22][1]),
                       ifelse(clean[clean$Subregion==input$region_region,22][1]<10,
                              paste(as.character(0), as.character(clean[clean$Subregion==input$region_region,22][1]), sep="0"),
                              paste(as.character(0), as.character(clean[clean$Subregion==input$region_region,22][1]), sep=""))
                       )
    
    clean %>% filter(., Year==input$region_year, Subregion==input$region_region) %>%
      gvisGeoChart(., locationvar='Country', colorvar=input$region_data,
                   options=list(width=550,keepAspectRatio=TRUE,
                                region=region_code))
  })
  
  
  
  
  ### 3. REGION RENEWABLE SHARE OVER TIME 
  output$region_share = renderPlotly({
    ggplotly(clean %>% filter(., Subregion==input$region_region) %>% 
      ggplot(., aes_string(x="Year", y=input$region_data, color="Country")) +
      geom_line() +
      ylim(0,100) +
      ylab("Renewable Share (%)") +
      xlab("") +
      ggtitle("Renewable Share of Energy") +
      theme_bw()
    )
  })
  
  
  
  ### 5. COUNTRY GDPPC & URBAN POP OVER TIME
  output$country_econ = renderPlotly({
    ggplotly(clean %>% filter(., Country==input$country) %>% 
      select(., Year, GDP.PC, Urban) %>% 
      mutate(., `GDPPC`=rescale(GDP.PC)*100) %>% 
      gather(., key="Measure", value="Value", 2:4) %>% 
      filter(., Measure=='GDPPC' | Measure=='Urban') %>% 
      ggplot(., aes(x=Year, y=Value, color=Measure)) +
      geom_line() +
      ylim(0,100) + 
      ylab("Pop. (%); GDPPC 0-100") +
      xlab("") +
      ggtitle("Urban Population %, and GDPPC Scaled") +
      theme_bw() +
      scale_color_manual(name="", 
                           labels = c("GDPPC", "Urban"),
                           values = c("GDPPC"="blue", "Urban"="black")),
      height=300
    )
  })
  
  
  ### 6. COUNTRY RENEWABLE SHARE OVER TIME
  output$country_share = renderPlotly({
    ggplotly(clean %>% filter(., Country==input$country) %>% 
      gather(., key="Measure", value="Percent", 5:18) %>% 
      filter(., Measure=='Share.Output' | Measure=='Share.Consumption') %>% 
      ggplot(., aes(x=Year, y=Percent, color=Measure)) +
      geom_line() +
      ylim(0,100) +
      ylab("Renewable Share (%)") +
      xlab("") +
      ggtitle("Renewable Share of Energy") +
      theme_bw() +
      scale_color_manual(name="", 
                           labels = c("Share.Output", "Share.Consumption"),
                           values = c("Share.Output"="blue", "Share.Consumption"="black")),
      height=300
    )
  })
  
  
  ### 7. COUNTRY ACCESS TO ELECTRICITY OVER TIME
  output$country_electricity = renderPlotly({
    ggplotly(clean %>% filter(., Country==input$country) %>% 
      gather(., key="Pop", value="Percent", 5:18) %>% 
      filter(., Pop=='Total' | Pop=='Rural' | Pop=='Urban') %>% 
      ggplot(., aes(x=Year, y=Percent, color=Pop)) +
      geom_line() +
      ylim(0,100) +
      ylab("Population (%)") +
      xlab("") +
      ggtitle("Access to Electricity") +
      theme_bw() +
      scale_color_manual(name="", 
                         labels = c("Rural", "Total", "Urban"),
                         values = c("Rural"="blue", "Total"="black", "Urban"="darkgreen")),
      height=300
    )
    
  })
  
  
  
  ### 10. INCOME HISTOGRAM
  output$income_hist = renderPlotly({
    ggplotly(clean %>% filter(., Year==input$income_year, Income.Group==input$income_group) %>% 
      ggplot(., aes_string(x=input$income_data)) +
      geom_histogram(bins=8) +
      theme_bw()
    )
  })
  
  
  
  
  ### 12. Top Region Pie
  output$top_region = renderPlotly({
    clean %>% filter(., Year==input$top_year) %>% 
      arrange(., desc(Share.Output)) %>% 
      head(input$top_number) %>%
      plot_ly(., labels=~Region, values=~Share.Output, type='pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              text = ~paste(Country, 'AS AN EXAMPLE, probably nothing in here'),
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1)),
              showlegend = FALSE)
  })
  
  
  ### 13. Top Income Pie
  output$top_income = renderPlotly({
    clean %>% filter(., Year==input$top_year) %>% 
      arrange(., desc(Share.Output)) %>% 
      head(input$top_number) %>% 
      plot_ly(., labels=~Income.Group, values=~Share.Output, type='pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              text = ~paste(Country, 'AS AN EXAMPLE, probably nothing in here'),
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1)),
              showlegend = FALSE)
  })
  
  
  
  ### 15. World Map
  output$worldmap = renderGvis({
    clean %>% filter(., Year==input$worldmap_year) %>%
      gvisGeoChart(., locationvar='Country', colorvar=input$worldmap_data,
                   options=list(height=550,width=925,backgroundColor="white"))
                                                    #222222 is the bg color of darkly theme, just in case
  })
}