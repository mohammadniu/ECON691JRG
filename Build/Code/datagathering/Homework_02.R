# MOHAMMAD SHOAIB (Z1980095)
# ECON 691 - HW_2

library(tidyverse)
library(tidycensus)

# ##.......PART 1.......## #

# import data
election <- read.csv("./Data/Vote Data.csv")

# data cleaning 
election_20 <- election %>% 
  filter(year == 2020) %>% 
  filter(state_po %in% c("ND","SD","MN")) %>% 
  filter(party == "DEMOCRAT" | party == "REPUBLICAN") %>% 
  mutate(cand = case_when(party == "DEMOCRAT" ~ "Biden",
                        party == "REPUBLICAN" ~ "Trump"),
                        percent = candidatevotes / totalvotes) %>%
  pivot_wider(id_cols = c(county_fips, state, county_name), 
              names_from = cand, 
              values_from = c(percent, candidatevotes)) %>% 
  rename(State = state,
         County = county_name,
         GEOID = county_fips,
         Biden = candidatevotes_Biden,
         Trump = candidatevotes_Trump,
         pBiden = percent_Biden,
         pTrump = percent_Trump)

# Part-1 final output
head(election_20)

# ##...........PART 2..........## #

# working with Census data
census_api_key("928a068a048af13c091f2f4bf9a5c49c31950fbc", install = TRUE)

var <- c("B01001_001","B01001_002","B02001_002","B02001_003","B01002_002","B01002_003")

# obtain ACS census data of year 2020
acs1 <- get_acs(geography = "county",
               variables = var,
               year = 2020,
               state = c(27, 38, 46),
               geometry = TRUE)

# cleaning census data
census1 <- acs1 %>%
  pivot_wider(names_from = "variable", values_from = "estimate") %>% 
  rename("total_pop"="B01001_001",
         "male"="B01001_002",
         "white"="B02001_002",
         "black"="B02001_003",
         "med_male"="B01002_002",
         "med_fem"="B01002_003") %>% 
  mutate(pMale = male/total_pop,
         pWhite = white/total_pop,
         pBlack = black/total_pop,
         GEOID = as.numeric(GEOID))

# part-2 final output
census_20 <- census1 %>% 
  select(-c("NAME", "moe", "male", "white", "black"))

head(census_20)

# ##.........PART 3...........## #

# merging two data frames
output <- full_join(election_20, census_20, by = "GEOID")
head(output)

# plotting percentage of votes for Biden
ggplot(output) +
  geom_sf(aes(geometry = geometry, fill = pBiden)) +
  ggtitle("Votes for Biden, %")

# plotting percentage of votes for Trump
ggplot(output) +
  geom_sf(aes(geometry = geometry, fill = pTrump)) +
  ggtitle("Votes for Trump, %")

# plotting median age for men
ggplot(output) +
  geom_sf(aes(geometry = geometry, fill = med_male)) +
  ggtitle("median age for men") # graph is not as expected because of missing values
