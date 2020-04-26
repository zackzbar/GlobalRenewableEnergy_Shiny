
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
  
  
  
  ### 5. COUNTRY GDPPC OVER TIME
  output$country_gdppc = renderPlotly({
    ggplotly(clean %>% filter(., Country==input$country) %>% 
      ggplot(aes(x=Year, y=GDP.PC)) +
      geom_line() +
      ylab("GDP PC (USD)") +
      xlab("") +
      ggtitle("GDP Per Capita") +
      theme_bw(),
    height = 375
      )
  })
  
  

  
  
  ### 6. COUNTRY RENEWABLE SHARES AND TOTALS OVER TIME
  output$country_share = renderPlotly({
    ggplotly(clean %>% filter(., Country==input$country) %>% 
      gather(., key="Measure", value="Percent", 5:18) %>% 
      filter(., Measure=='Share.Output' | Measure=='Share.TFEC') %>% 
      ggplot(., aes(x=Year, y=Percent, color=Measure)) +
      geom_line() +
      ylim(0,100) +
      ylab("Renewable Share (%)") +
      xlab("") +
      ggtitle("Renewable Share of Energy") +
      theme_bw() +
      scale_color_manual(name="", 
                           labels = c("Share.Output", "Share.TFEC"),
                           values = c("Share.Output"="blue", "Share.TFEC"="black")),
      height=375
    )
  })
  

  output$country_total = renderPlotly({
    ggplotly(clean %>% filter(., Country==input$country) %>% 
               gather(., key="Measure", value="GWh", 5:18) %>% 
               filter(., Measure=='Renewable.Output' | Measure=='Renewable.TFEC') %>% 
               ggplot(., aes(x=Year, y=GWh, color=Measure)) +
               geom_line() +
               ylab("Energy/Electricity (GWh)") +
               xlab("") +
               ggtitle("Renewable Output and TFEC") +
               theme_bw() +
               scale_color_manual(name="", 
                                  labels = c("Renewable.Output", "Renewable.TFEC"),
                                  values = c("Renewable.Output"="blue", "Renewable.TFEC"="black")),
             height=375
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
      height=375
    )
    
  })
  
  
  
  ### 10. INCOME HISTOGRAM
  output$income_hist = renderPlot({
    clean %>% filter(., Year==input$income_year, Income.Group==input$income_group) %>% 
      ggplot(., aes_string(x=input$income_data)) +
      geom_histogram(bins=8) +
      ggtitle("Distribution of Countries by Income Group") +
      ylab("Count of Countries") +
      theme_bw() +
      theme(plot.title = element_text(size=22, lineheight = 10),
            axis.title.y = element_text(size=18),
            axis.title.x = element_text(size=18))
  })
  
  
  ### INCOME STATS BOX
  
  output$income_stats_data = renderText(input$income_data)
  
  
  output$income_stats_1 = renderText({
    stats = clean %>% filter(., Year==input$income_year, Income.Group==input$income_group) %>% 
      arrange(desc(eval(as.symbol(input$income_data)))) %>% 
      select(., Country)
    
    paste("1. ", as.character(stats[1,1]))
    
  })
  
  output$income_stats_12 = renderText({
    stats = clean %>% filter(., Year==input$income_year, Income.Group==input$income_group) %>% 
      arrange(desc(eval(as.symbol(input$income_data))))
    
    paste("Value: ", as.character(round(stats[1,input$income_data], digits=2)))
    
  })
  
  output$income_stats_2 = renderText({
    stats = clean %>% filter(., Year==input$income_year, Income.Group==input$income_group) %>% 
      arrange(desc(eval(as.symbol(input$income_data)))) %>% 
      select(., Country)
    
    paste("2. ", as.character(stats[2,1]))
    
  })
  
  output$income_stats_22 = renderText({
    stats = clean %>% filter(., Year==input$income_year, Income.Group==input$income_group) %>% 
      arrange(desc(eval(as.symbol(input$income_data))))
    
    paste("Value: ", as.character(round(stats[2,input$income_data], digits=2)))
    
  })
  
  output$income_stats_3 = renderText({
    stats = clean %>% filter(., Year==input$income_year, Income.Group==input$income_group) %>% 
      arrange(desc(eval(as.symbol(input$income_data)))) %>% 
      select(., Country)
    
    paste("3. ", as.character(stats[3,1]))
    
  })
  
  output$income_stats_32 = renderText({
    stats = clean %>% filter(., Year==input$income_year, Income.Group==input$income_group) %>% 
      arrange(desc(eval(as.symbol(input$income_data))))
    
    paste("Value: ", as.character(round(stats[3,input$income_data], digits=2)))
    
  })
  
  
  
  
  
  
  
  
  
  ### 12. Top Region Pie
  output$top_region = renderPlotly({
    clean %>% filter(., Year==input$top_year) %>% 
      arrange(., desc(eval(as.symbol(input$top_data)))) %>% 
      head(input$top_number) %>%
      plot_ly(., labels=~Region, values=~eval(as.symbol(input$top_data)), type='pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              text = ~paste("Top Country in Region: ", Country),
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1)),
              showlegend = FALSE)
  })
  
  
  
  
  ### 13. Top Income Pie
  output$top_income = renderPlotly({
    clean13 = clean %>% filter(., Year==input$top_year) %>% 
      arrange(., desc(eval(as.symbol(input$top_data)))) %>% 
      head(input$top_number)
                                                        
    plot_ly(clean13, labels=clean13$Income.Group, values=clean13[ , input$top_data], type='pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              text = ~paste("Top Country in Income Group: ", Country),
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1)),
              showlegend = FALSE)
  })
  
  
  
  
  
  
  
  ### 15. World Map
  

  
  output$worldmap2 = renderPlotly({
   
    clean1 = clean %>% filter(., Year==input$worldmap_year)
    
    plot_geo(clean1) %>% 
      add_trace(z = clean1[, input$worldmap_data], color = clean1[, input$worldmap_data], 
                colors = 'Greens',
                text = clean1$Country, 
                locations = clean1$Code, 
                marker = list(line = list(color = toRGB("grey"), width = 0.5))) %>% 
      colorbar(title = '', ticksuffix = '') %>% 
      layout(geo = list(
        showframe = FALSE,
        showcoastlines = FALSE,
        projection = list(type = 'Mercator')
      ))
    
  })
  
  
  
  ### WORLD MAP STATS BOX
  
  
  output$worldmap_stats_data = renderText(input$worldmap_data)
  
  output$worldmap_stats_1 = renderText({
    stats = clean %>% filter(., Year==input$worldmap_year) %>% 
      arrange(desc(eval(as.symbol(input$worldmap_data)))) %>% 
      select(., Country)
    
    paste("1. ", as.character(stats[1,1]))
    
  })
  
  output$worldmap_stats_12 = renderText({
    stats = clean %>% filter(., Year==input$worldmap_year) %>% 
      arrange(desc(eval(as.symbol(input$worldmap_data))))
    
    paste("Value: ", as.character(round(stats[1,input$worldmap_data], digits=2)))
    
  })
  
  output$worldmap_stats_2 = renderText({
    stats = clean %>% filter(., Year==input$worldmap_year) %>% 
      arrange(desc(eval(as.symbol(input$worldmap_data)))) %>% 
      select(., Country)
    
    paste("2. ", as.character(stats[2,1]))
    
  })
  
  output$worldmap_stats_22 = renderText({
    stats = clean %>% filter(., Year==input$worldmap_year) %>% 
      arrange(desc(eval(as.symbol(input$worldmap_data))))
    
    paste("Value: ", as.character(round(stats[2,input$worldmap_data], digits=2)))
    
  })
  
  output$worldmap_stats_3 = renderText({
    stats = clean %>% filter(., Year==input$worldmap_year) %>% 
      arrange(desc(eval(as.symbol(input$worldmap_data)))) %>% 
      select(., Country)
    
    paste("3. ", as.character(stats[3,1]))
    
  })
  
  output$worldmap_stats_32 = renderText({
    stats = clean %>% filter(., Year==input$worldmap_year) %>% 
      arrange(desc(eval(as.symbol(input$worldmap_data))))
    
    paste("Value: ", as.character(round(stats[3,input$worldmap_data], digits=2)))
    
  })

  
  
  
}