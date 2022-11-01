# MOHAMMAD SHOAIB (Z1980095)
# ECON 691 - HW_01

library(tidyverse)
covid <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")


# create daily change function
DIF <- function(x){
  daily_change <- x-lag(x)
  return(daily_change)
}

# create percentage change function
DIF_2 <- function(y){
  pc_change <- ((y-lag(y))/lag(y))
  return(round(pc_change,2))
}

# create vectors of new cases & deaths
covid.IL <- covid %>% 
  filter(state == "Illinois" & county == "Cook" & date >= '2021-01-01') %>% 
  mutate(new_case = DIF(cases), 
         new_death = DIF(deaths)) %>% 
  mutate(new_pc_case = DIF_2(new_case), 
         new_pc_death = DIF_2(new_death)) %>% 
  mutate(new_pc_case = ifelse(is.infinite(new_pc_case), NA, new_pc_case), 
         new_pc_death = ifelse(is.infinite(new_pc_death), NA, new_pc_death),
         Date=as.Date(date,"%Y-%m-%d"))

# output
covid.IL

# plot percentage change of new cases
plot(covid.IL$Date, covid.IL$new_pc_case, 
     type = 'l', 
     xlab = "Year", 
     ylab = "Daily Changes in New Cases, %",
     main = "Covid Scenario in Cook County, IL (Jan 2021 - May 2022)")

# plot percentage change of new deaths
plot(covid.IL$Date, covid.IL$new_pc_death, 
     type = 'l', 
     xlab = "Year", 
     ylab = "Daily Changes in New Deaths, %",
     main = "Covid Death Scenario in Cook County, IL (Jan 2021 - May 2022)")



