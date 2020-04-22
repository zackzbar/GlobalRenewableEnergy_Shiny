library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(scales)
library(googleVis)

clean = read.csv("./data/Energy/clean10.csv")

write.csv(clean, "clean10.csv")



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











