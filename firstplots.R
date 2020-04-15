library(ggplot2)
library(dplyr)

#Access to Electricity over time
clean %>% filter(., name=='West Bank and Gaza') %>%
  ggplot() +
  geom_line(aes(x=year,y=access_total, color='black')) +
  geom_line(aes(x=year,y=access_rural, color='blue')) +
  geom_line(aes(x=year,y=access_urban, color='red')) +
  scale_color_identity(name='Population',
                       breaks=c("black","blue","red"),
                       labels=c("All","Rural","Urban"),
                       guide = "legend")

#Renewable Share Output over time
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
