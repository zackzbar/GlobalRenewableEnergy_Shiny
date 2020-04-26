library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(scales)
library(googleVis)

clean = read.csv("./data/Energy/clean10.csv")

write.csv(clean, "clean10.csv")





cleancopy = clean

cleancopy = cleancopy %>% mutate(., Renewable.Consumption=Renewable.Consumption*.277777778)


write.csv(cleancopy, "clean11.csv")





download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="data/world_shape_file.zip")

system("unzip data/world_shape_file.zip")


library(rgdal)
world_spdf <- readOGR( 
  dsn= paste0(getwd(),"/data/world_shape_file/") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

# Clean the data object
library(dplyr)
world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2)

library(leaflet)

# Create a color palette for the map:
mypalette <- colorNumeric( palette="viridis", domain=world_spdf@data$POP2005, na.color="transparent")
mypalette(c(45,43))

# Basic choropleth with leaflet?
m <- leaflet(world_spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( fillColor = ~mypalette(POP2005), stroke=FALSE )

m









ggplot() +
  geom_polygon(data = world_spdf, aes( x = long, y = lat, group = group)) +
  theme_void() +
  coord_map()





library(maps)
countries = map_data("world")

cleancopy = clean

cleancopy$Country = as.character(clean$Country)

cleancopy[cleancopy$Code=="USA", ] = cleancopy %>% filter(., Code=="USA") %>% mutate(., Country="USA")

ggmap = left_join(countries, filter(cleancopy, Year==2015), by=c("region"="Country"))
ggmap

ggplot(data = ggmap, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = Share.Output)) +
  scale_fill_continuous(high='darkgreen', low='white') +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.text=element_blank(), 
        axis.ticks=element_blank(), 
        axis.title=element_blank(), 
        legend.position = c(0.95,0.28), 
        legend.background=element_rect(fill="white", colour="white") ) #+ 
  #coord_map('mercator')




ggplot(data=cleancopy) +
  geom_map(map = countries, aes(map_id=Country, fill=Share.Output), color='gray40') + 
  expand_limits(x=countries$long, y=countries$lat) + 
  scale_fill_continuous(high='darkgreen', low='white') +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.text=element_blank(), 
        axis.ticks=element_blank(), 
        axis.title=element_blank(), 
        legend.position = c(0.95,0.28), 
        legend.background=element_rect(fill="white", colour="white") ) + 
  coord_map('mercator') +
  labs(title='Number of Murders Country-wide in 1973')








clean1 = clean %>% filter(., Year==2015)

plot_ly(data=clean1, type='choropleth', locations=clean1$Code, z=clean1$Share.Output, 
          text=clean1$Country, colorscale="Greens")


plot_geo(clean1) %>% 
  add_trace(z = clean1$Share.Output, color = clean1$Share.Output, 
            colors = 'Greens',
            text = clean1$Country, 
            locations = clean1$Code, 
            marker = list(line = list(color = toRGB("grey"), width = 0.5))) %>% 
  colorbar(title = '% Renewable Energy', ticksuffix = '%') %>% 
  layout(geo = list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = 'Mercator')
  ))







clean %>% filter(., Year==2015, Country=="Brazil")










library(plotly)
library(rjson)

url <- 'data/countries.geojson'
counties <- rjson::fromJSON(file=url)
url2<- "https://raw.githubusercontent.com/plotly/datasets/master/fips-unemp-16.csv"
df <- read.csv(url2, colClasses=c(fips="character"))
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
fig <- plot_ly()
fig <- fig %>% add_trace(
  type="choropleth",
  geojson=counties,
  locations=df$fips,
  z=df$unemp,
  colorscale="Viridis",
  zmin=0,
  zmax=12,
  marker=list(line=list(
    width=0)
  )
)
fig <- fig %>% colorbar(title = "Unemployment Rate (%)")
fig <- fig %>% layout(
  title = "2016 US Unemployment by County"
)

fig <- fig %>% layout(
  geo = g
)

fig











# 1. REGIONAL STATS BOX




## 3. REGIONAL RENEWABLE SHARE OVER TIME (maybe Total too)
region_ot = clean %>% filter(., Region=='Latin America & Caribbean') %>% 
  ggplot(., aes(x=Year, y=Share.Consumption, color=Country)) +
  geom_line() +
  ylim(0,100) +
  theme_bw()

ggplotly(region_ot)
#lol need to remake the regions...


## 3.2 REGIONAL HISTOGRAM
region_hist = clean %>% filter(., Year==2015, Region=='Latin America & Caribbean') %>% 
  ggplot(., aes(x=Share.Consumption)) +
  geom_histogram(bins=8) +
  theme_bw()

ggplotly(region_hist)


## 2. REGIONAL MAPS!!
region_map = clean %>% filter(., Year==2015, Subregion=='Western Europe') %>%
  gvisGeoChart(., locationvar='Country', colorvar='Share.Output',
               options=list(height=500,width=800, region='155'))


plot(region_map)


# 4. COUNTRY STATS BOX




## 5. COUNTRY GDPPC & URBAN POP OVER TIME

  ## GDP PC over time
pcgdp_ot = clean %>% filter(., Country=="Brazil") %>% 
  ggplot(aes(x=Year, y=GDP.PC)) +
  geom_line()

ggplotly(pcgdp_ot)


  ## Urban Pop % over time
urban_pop_ot = clean %>% filter(., Country=="Brazil") %>% 
  ggplot(aes(x=Year, y=Urban)) +
  geom_line() + 
  ylim(0,100)

ggplotly(urban_pop_ot)


output$country_urbanpop = renderPlotly({
  ggplotly(clean %>% filter(., Country==input$country) %>% 
             ggplot(aes(x=Year, y=Urban.Population)) +
             geom_line() +
             ylim(0,100) +
             ylab("Population (%)") +
             xlab("") +
             ggtitle("Percent of Poplation in Urban Areas") +
             theme_bw(),
           height = 375
  )
})




  ## Both
  # With normal table... legend gets messed up.
gdp_urban_ot = clean %>% filter(., Country=="Brazil") %>% 
  mutate(., scaled=rescale(GDP.PC)*100) %>% 
  ggplot() +
  geom_line(aes(x=Year, y=scaled, color='blue')) +
  geom_line(aes(x=Year, y=Urban, color="red")) +
  scale_color_identity(name='',
                       breaks=c("blue","red"),
                       labels=c("GDP PC Scaled","Urban Pop %"),
                       guide = "legend") +
  ylim(0,100) + 
  theme_bw()


#the full one....
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

  ## Both
  # With gather()... legend gets name of variable.
gdp_urban_ot = clean %>% filter(., Country=="Brazil") %>% 
  select(., Year, GDP.PC, Urban) %>% 
  mutate(., `GDP PC Scaled`=rescale(GDP.PC)*100) %>% 
  gather(., key="measure", value="value", 2:4) %>% 
  filter(., measure=='GDP PC Scaled' | measure=='Urban') %>% 
  ggplot(., aes(x=Year, y=value, color=measure)) +
  geom_line() +
  ylim(0,100) + 
  theme_bw()

ggplotly(gdp_urban_ot)




## 6. COUNTRY RENEWABLE SHARE OVER TIME (maybe Total too)

  ## Renewable Share Output over time
renewable_share_out_ot = clean %>% filter(., Country=='Brazil') %>%
  ggplot(., aes(x=Year,y=Share.Output)) +
  geom_line() +
  ylim(0,100) +
  ylab("Renewable Share of Energy Output (%)") +
  xlab("Year") +
  theme_bw()

ggplotly(renewable_share_out_ot)


  ## Renewable Share TFEC over time
renewable_share_TFEC_ot = clean %>% filter(., Country=='Brazil') %>%
  ggplot(., aes(x=Year,y=Share.Consumption)) +
  geom_line()+
  ylim(0,100) +
  ylab("Renewable Share of Energy Consumption (%)") +
  xlab("Year") +
  theme_bw()

ggplotly(renewable_share_TFEC_ot)


  ## Both
  ## With gather()... legend gets name of variable.
renewable_share_both_ot = clean %>% filter(., Country=='Brazil') %>% 
  gather(., key="measure", value="value", 5:18) %>% 
  filter(., measure=='Share.Output' | measure=='Share.Consumption') %>% 
  ggplot(., aes(x=Year, y=value, color=measure)) +
  geom_line() +
  ylim(0,100) +
  ylab("Renewable Share of Total Energy (%)") +
  theme_bw()

ggplotly(renewable_share_both_ot)






## 7. COUNTRY ACCESS TO ELECTRICITY OVER TIME

  ## With normal table... legend gets messed up.
access_ot = clean %>% filter(., Country=='Brazil') %>%
  ggplot() +
  geom_line(aes(x=Year,y=Total, color='black')) +
  geom_line(aes(x=Year,y=Rural, color='blue')) +
  geom_line(aes(x=Year,y=Urban, color='red')) +
  scale_color_identity(name='Population',
                       breaks=c("black","blue","red"),
                       labels=c("Total","Rural","Urban"),
                       guide = "legend") +
  ylim(0,100) +
  ylab("% of Pop w/ Electricity Access") +
  xlab("Year") +
  theme_bw()

  ## With gather()... legend gets name of variable.
access_ot = clean %>% filter(., Country=='Brazil') %>% 
  gather(., key="Pop", value="Percent", 5:18) %>% 
  filter(., Pop=='Total' | Pop=='Rural' | Pop=='Urban') %>% 
  ggplot(., aes(x=Year, y=Percent, color=Pop)) +
  geom_line() +
  ylim(0,100) +
  ylab("% of Pop w/ Electricity Access") +
  xlab("Year") +
  theme_bw() +
  scale_color_manual(name="", 
                     labels = c("Rural", "Total", "Urban"),
                     values = c("Rural"="blue", "Total"="black", "Urban"="red"))

ggplotly(access_ot)

  ## With plotly...
c <- clean %>% filter(., Name=='Brazil') %>% plot_ly(., x = ~Year, y = ~Total, type = 'scatter', mode = 'lines', name = 'Total')
c <- b %>% add_trace(y = ~Rural, name = 'Rural')
c <- b %>% add_trace(y = ~Urban, name = 'Urban')

c






# 8. INCOME LEVEL STATS BOX




## 9. INCOME LEVEL LINE CHART

#on second thought... not a great idea...


## 10. INCOME LEVEL HISTOGRAM
income_hist = clean %>% filter(., Year==2015, Income.Group=='High income') %>% 
  group_by(., Income.Group) %>% 
  ggplot(., aes(x=Share.Consumption)) +
  geom_histogram(bins=8) +
  theme_bw()

ggplotly(income_hist)



# 11. TOP% STATS BOX




## 12. TOP% REGION PIE
clean %>% filter(., Year==2015) %>% 
  arrange(., desc(Share.Output)) %>% 
  head(10) %>%
  plot_ly(., labels=~Region, values=~Share.Output, type='pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          hoverinfo = 'text',
          text = ~paste(Country, 'AS AN EXAMPLE, probably nothing in here'),
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1)),
          showlegend = FALSE)




## 13. TOP% INCOME PIE
clean %>% filter(., Year==2015) %>% 
  arrange(., desc(Share.Output)) %>% 
  head(10) %>% 
  plot_ly(., labels=~Income.Group, values=~Share.Output, type='pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          hoverinfo = 'text',
          text = ~paste(Country, 'AS AN EXAMPLE, probably nothing in here'),
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1)),
          showlegend = FALSE)



# 14. FRONT PAGE STATS BOX




## 15. WORLD MAP
world = clean %>% filter(., Year==2015) %>%
  gvisGeoChart(., locationvar='Country', colorvar='Share.Output',
               options=list(height=500,width=800))

plot(world)


#To fix up country names
clean$Country = as.character(clean$Country)

clean[clean$Code=="BHS", ] = clean %>% filter(., Code=="BHS") %>% mutate(., Country="Bahamas")







#output$region_map = renderGvis({
#  
#  region_code=ifelse(clean[clean$Subregion==input$region_region,22][1]>100,
#                     as.character(clean[clean$Subregion==input$region_region,22][1]),
#                     ifelse(clean[clean$Subregion==input$region_region,22][1]<10,
#                            paste(as.character(0), as.character(clean[clean$Subregion==input$region_region,22][1]), sep="0"),
#                            paste(as.character(0), as.character(clean[clean$Subregion==input$region_region,22][1]), sep=""))
#  )
#  
#  clean %>% filter(., Year==input$region_year, Subregion==input$region_region) %>%
#    gvisGeoChart(., locationvar='Country', colorvar=input$region_data,
#                 options=list(width=550,keepAspectRatio=TRUE,
#                              region=region_code))
#})




#  output$worldmap = renderGvis({
#    clean %>% filter(., Year==input$worldmap_year) %>%
#      gvisGeoChart(., locationvar='Country', colorvar=input$worldmap_data,
#                   options=list(height=550,width=925,backgroundColor="white"))
#                                                    #222222 is the bg color of darkly theme, just in case
#  })



#  output$worldmap2 = renderPlotly({
#    
#    clean1 = clean %>% filter(., Year==input$worldmap_year)
#    
#    plot_ly(data=clean1, type='choropleth', locations=clean1$Code, z=clean1[, input$worldmap_data],
#            text=clean1$Country, colorscale="Greens")
#    
#  })







# Renewable Share Output Top 10
clean %>% filter(., Year==2015) %>% 
  select(., Country, Share.Output, Renewable.Output) %>% 
  arrange(desc(Share.Output), desc(Renewable.Output)) %>% 
  head(10)


# Renewable Share Consumption Top 10
clean %>% filter(., Year==2015) %>% 
  select(., Country, Share.Consumption, Renewable.Consumption) %>% 
  arrange(desc(Share.Consumption), desc(Renewable.Consumption)) %>% 
  head(10)


# Renewable Output Top 10
clean %>% filter(., Year==2015) %>% 
  select(., Country, Share.Output, Renewable.Output) %>% 
  arrange(desc(Renewable.Output)) %>% 
  head(10)


# Renewable Consumption Top 10
clean %>% filter(., Year==2015) %>% 
  select(., Country, Share.Consumption, Renewable.Consumption) %>% 
  arrange(desc(Renewable.Consumption)) %>% 
  head(10)











