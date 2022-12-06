# MOHAMMAD SHOAIB (Z1980095)
# Homework 03

library(tidyverse)
library(tidycensus)
library(stargazer)

# import vote data
votes <- read.csv("./Data/Vote Data.csv")

# county-wise total vote cast
v.cast <- votes %>%   
  filter(year == 2020) %>% 
  group_by(county_fips) %>% 
  summarise(cast = sum(totalvotes))

# data cleaning 
states <- c("ND","SD","MN")

main <- votes %>%
  filter(year == 2020) %>% 
  full_join(., v.cast, by = "county_fips") %>% 
  filter(state_po %in% states) %>% 
  filter(party == "DEMOCRAT" | party == "REPUBLICAN") %>% 
  mutate(cand = case_when(party == "DEMOCRAT" ~ "Biden",
                          party == "REPUBLICAN" ~ "Trump"),
         percent = candidatevotes / cast) %>%
  select(state, county_name, county_fips, cand, candidatevotes, percent) %>% 
  pivot_wider(names_from = cand, 
              values_from = c(percent, candidatevotes)) %>% 
  left_join(.,v.cast, by = "county_fips") %>% 
  rename(Biden = candidatevotes_Biden,
         Trump = candidatevotes_Trump,
         pctBiden = percent_Biden,
         pctTrump = percent_Trump) %>% 
  mutate(GEOID = as.character(county_fips)) %>% 
  select(-county_fips)

# working with census data
#census_api_key("928a068a048af13c091f2f4bf9a5c49c31950fbc", install = TRUE)

var <- c("B01001_001","B01001_002","B02001_002","B02001_003","B01002_002","B01002_003")
fips <- c(27, 38, 46)

# obtain ACS census data of year 2020
acs <- get_acs(geography = "county",
                variables = var,
                year = 2020,
                state = fips,
                geometry = FALSE)

# cleaning census data
census <- acs %>%
  select(GEOID, variable, estimate) %>% 
  pivot_wider(names_from = "variable", values_from = "estimate") %>% 
  rename("TotPop"="B01001_001",
         "Male"="B01001_002",
         "White"="B02001_002",
         "Black"="B02001_003",
         "Age_M"="B01002_002",
         "Age_F"="B01002_003") %>% 
  mutate(pMale = Male/TotPop,
         pWhite = White/TotPop,
         pBlack = Black/TotPop,
         geometry = NULL) %>%
  replace(is.na(.), 0) %>% 
  group_by(GEOID) %>% 
  summarise(total_pop = sum(TotPop),
            med_male = mean(Age_M),
            med_fem = mean(Age_F),
            pMale = mean(pMale),
            pWhite = mean(pWhite),
            pBlack = mean(pBlack)) %>% 
  full_join(., main, by = "GEOID")


###...........import covid data.........###
covid <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# create county-wise total cases & deaths in three states
covid_recent <- covid %>% 
  filter(state %in% c("North Dakota", "South Dakota", "Minnesota") & date >= '2021-01-01') %>% 
  group_by(fips) %>% 
  summarise(total_cases = sum(cases), total_deaths = sum(deaths)) %>% 
  mutate(GEOID = as.character(fips))
  

###.........Part 1........###

# merging datasets and create per capita cases & deaths
vote_acs_covid <- full_join(census, covid_recent, by = "GEOID") %>% 
  mutate(per_cap_cases = total_cases/total_pop, 
         per_cap_deaths = total_deaths/total_pop) #%>% 
  #arrange(desc(per_cap_deaths, per_cap_cases)) Not sure why this is needed and it is crashing the code.

core<-vote_acs_covid #Not sure where this went?

###..........Part 2.........###
summary(vote_acs_covid)

# factoring state
core2 <- core %>%  #You have not "core" to reference
  mutate(s.fips = substr(GEOID, 1, 2),
         state = case_when(s.fips == "27" ~ "North Dakota",
                           s.fips == "38" ~ "South Dakota",
                           s.fips == "46" ~ "Minnesota"))
core2$state <- factor(core2$state, levels = c("North Dakota", "South Dakota", "Minnesota"))

# pctTrump is dependent variable
(model1 <- lm(pctTrump ~ pMale + state, data = vote_acs_covid))
(model2 <- lm(pctTrump ~ pMale + pWhite + state, data = vote_acs_covid))
(model3 <- lm(pctTrump ~ pMale + pWhite + med_male + med_fem + state, data = vote_acs_covid))

# per capita cases is dependent variable
(model4 <- lm(per_cap_cases ~ pMale + state, data = vote_acs_covid))
(model5 <- lm(per_cap_cases ~ pMale + pWhite + state, data = vote_acs_covid))
(model6 <- lm(per_cap_cases ~ pMale + pWhite + med_male + med_fem + pctTrump + state, data = vote_acs_covid))

# per capita deaths is dependent variable
(model7 <- lm(per_cap_deaths ~ pMale + state, data = vote_acs_covid))
(model8 <- lm(per_cap_deaths ~ pMale + pWhite + state, data = vote_acs_covid))
(model9 <- lm(per_cap_deaths ~ pMale + pWhite + med_male + med_fem + pctTrump + state, data = vote_acs_covid))

###....Displaying results......###
stargazer(as.data.frame(vote_acs_covid), type = "html", title = "Descriptive Statistics", out = "./Analysis/Result.html")
stargazer(model1, model2, model3, type = "html", out = "./Analysis/Result1.html")
stargazer(model4, model5, model6, type = "html", out = "./Analysis/Result2.html")
stargazer(model7, model8, model9, type = "html", out = "./Analysis/Result3.html")

