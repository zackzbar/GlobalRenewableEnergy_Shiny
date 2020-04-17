library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(scales)

clean = read.csv("./data/Energy/clean7.csv")


# 1. REGIONAL STATS BOX




## 2. REGIONAL RENEWABLE SHARE OVER TIME (maybe Total too)




## 3. REGIONAL HISTOGRAM




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
  gather(., key="measure", value="value", 4:17) %>% 
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
  gather(., key="measure", value="value", 4:17) %>% 
  filter(., measure=='Total' | measure=='Rural' | measure=='Urban') %>% 
  ggplot(., aes(x=Year, y=value, color=measure)) +
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




## 10. INCOME LEVEL HISTOGRAM




# 11. TOP% STATS BOX




## 12. TOP% REGION PIE




## 13. TOP% INCOME PIE




# 14. FRONT PAGE STATS BOX




## 15. WORLD MAP





## 16. REGIONAL MAP STATS BOX




# 17. REGIONAL MAP 







# Renewable Share Output Top 10, 2015
clean %>% filter(., year==2015) %>% 
  select(., name, renewable_share_output, renewable_output) %>% 
  arrange(desc(renewable_share_output), desc(renewable_output)) %>% 
  head(10)


# Renewable Share Consumption Top 10, 2015
clean %>% filter(., year==2015) %>% 
  select(., name, renewable_share_TFEC, renewable_consumption) %>% 
  arrange(desc(renewable_share_TFEC), desc(renewable_consumption)) %>% 
  head(10)


# Renewable Output Top 10, 2015
clean %>% filter(., year==2015) %>% 
  select(., name, renewable_share_output, renewable_output) %>% 
  arrange(desc(renewable_output)) %>% 
  head(10)


# Renewable Consumption Top 10, 2015
clean %>% filter(., year==2015) %>% 
  select(., name, renewable_share_TFEC, renewable_consumption) %>% 
  arrange(desc(renewable_consumption)) %>% 
  head(10)











