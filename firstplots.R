library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

clean = read.csv("clean5.csv")

#GDP Per Capita over time
clean %>% filter(., name=="Brazil") %>% 
  ggplot(aes(x=year, y=pcgdp)) +
  geom_line()





#Access to Electricity over time (country)
a = clean %>% filter(., name=='Brazil') %>%
  ggplot() +
  geom_line(aes(x=year,y=access_total, color='black')) +
  geom_line(aes(x=year,y=access_rural, color='blue')) +
  geom_line(aes(x=year,y=access_urban, color='red')) +
  scale_color_identity(name='Population',
                       breaks=c("black","blue","red"),
                       labels=c("All","Rural","Urban"),
                       guide = "legend") +
  ylim(0,100)

ggplotly(a)
a

b <- clean %>% filter(., name=='Brazil') %>% plot_ly(., x = ~year, y = ~access_total, type = 'scatter', mode = 'lines', name = 'All')
b <- b %>% add_trace(y = ~access_rural, name = 'Rural')
b <- b %>% add_trace(y = ~access_urban, name = 'Urban')

b


#Access to Electricity over time (region)
clean %>% filter(., name=='INSERT REGION HERE') %>%
  ggplot() +
  geom_line(aes(x=year, y=access_total, color='black')) +
  geom_line(aes(x=year, y=access_rural, color='blue')) +
  geom_line(aes(x=year, y=access_urban, color='red')) +
  scale_color_identity(name='Population',
                       breaks=c("black","blue","red"),
                       labels=c("All","Rural","Urban"),
                       guide="legend") +
  ylim(0,100)


#Renewable Share Output over time
clean %>% filter(., name=='Costa Rica') %>%
  ggplot(., aes(x=year,y=renewable_share_output)) +
  geom_line()

#Renewable Share TFEC over time
clean %>% filter(., name=='Costa Rica') %>%
  ggplot(., aes(x=year,y=renewable_share_TFEC)) +
  geom_line()

#Renewable Share Output Top 10, 2015
clean %>% filter(., year==2015) %>% 
  select(., name, renewable_share_output, renewable_output) %>% 
  arrange(desc(renewable_share_output), desc(renewable_output)) %>% 
  head(10)

#Renewable Share Consumption Top 10, 2015
clean %>% filter(., year==2015) %>% 
  select(., name, renewable_share_TFEC, renewable_consumption) %>% 
  arrange(desc(renewable_share_TFEC), desc(renewable_consumption)) %>% 
  head(10)

#Renewable Output Top 10, 2015
clean %>% filter(., year==2015) %>% 
  select(., name, renewable_share_output, renewable_output) %>% 
  arrange(desc(renewable_output)) %>% 
  head(10)

#Renewable Consumption Top 10, 2015
clean %>% filter(., year==2015) %>% 
  select(., name, renewable_share_TFEC, renewable_consumption) %>% 
  arrange(desc(renewable_consumption)) %>% 
  head(10)
