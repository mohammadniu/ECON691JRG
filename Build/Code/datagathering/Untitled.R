
library(tidyverse)
covid <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
head(covid)
covid$state[1:6]
summary(covid)

# create percentage change function
delta <- function(x){
  temp <- ((x-lag(x))/lag(x))
  return(round(temp,4))
}

# create new variables 
covid.IL <- covid %>% 
  filter(state=="Illinois" & county=="Cook") %>% 
  mutate(pc_cases=delta(cases), pc_deaths=delta(deaths)) %>% 
  mutate(pc_deaths = ifelse(is.infinite(pc_deaths), NA, pc_deaths),
         Date=as.Date(date,"%Y-%m-%d"))
covid.IL %>% filter(pc_cases<0)
view(covid.IL)
head(covid.IL,10)
# ploting graph
plot(covid.IL$Date, covid.IL$pc_cases)

# setting #add people #
covid.IL

