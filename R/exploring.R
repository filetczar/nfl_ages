######################
# Phil Azar 
# Topic: NFL Age 
# Explore Data 
######################

# libraries
library(tidyverse)
library(stringr)
# read in data 

data_files <- list.files(path='./data/')
data_files <- data_files[-which(data_files=='raw')]

all_data <- data.frame()
for(i in seq_along(data_files)) { 
  assign(x=paste0('data_',str_sub(data_files[i], 12,-5)), 
         value =read_csv(paste0('./data/', data_files[i]))
         )
  # For cross position analysis: Games and Games Started
subset_df <- get(paste0('data_',str_sub(data_files[i], 12,-5))) %>% 
            mutate('Position' = str_sub(data_files[i],12, -5)) %>% 
            select(key, name, Year, Position, Age, ageGroup, Tm, Round, Pick, G, GS)

all_data <- rbind(subset_df, all_data)
    
  
}

# What is the propensity ofage groups across all? 
totals <- all_data %>% 
  group_by(Year) %>% 
  summarise(total = n())

per_ageGroup <- all_data %>% 
  group_by(Year, ageGroup) %>% 
  summarise(n = n())

totals %>% 
  inner_join(per_ageGroup, by='Year') %>% 
  mutate(perc = n/total) %>% 
  ggplot(.,aes(x=Year, y=perc, color = ageGroup)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2000,2018, by=1))


# What is the propensity of age groups by position? 
total_p <- all_data %>% 
  group_by(Year, Position) %>% 
  summarise(total = n())
per_ageGroup_p <- all_data %>% 
  group_by(Year, ageGroup, Position) %>% 
  summarise(n = n())

total_p %>% 
  inner_join(per_ageGroup_p, by=c('Year'= 'Year','Position'='Position')) %>% 
  mutate(prc = n/total) %>% 
  ggplot(.,aes(x=Year, y=prc, color=ageGroup)) +
  geom_line() + 
  facet_wrap(~Position)


total_p %>% 
  inner_join(per_ageGroup_p, by=c('Year'= 'Year','Position'='Position')) %>% 
  mutate(prc = n/total) %>% 
  filter(Position == 'rb', ageGroup =='32+')





