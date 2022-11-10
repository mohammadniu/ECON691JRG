# Econ 691 Classroom Example 3
# November 9, 2022

library(tidyverse)
library(tidycensus)

# Clean environment
rm(list = ls())

votes <- read.csv("./Data/Vote Data.csv")

levels(factor(votes$candidate))

v.cast <- votes %>% 
  filter(year == 2020) %>% 
  group_by(county_fips) %>% 
  summarise(cast = sum(candidatevotes))

head(v.cast)

# Select three states 
states <- c("IN", "IL", "MI")

main <- votes %>% 
  filter(year == 2020) %>% 
  filter(state_po %in% states) %>% 
  filter(party == "DEMOCRAT" | party == "REPUBLICAN") %>% 
  mutate(cand = case_when(party == "DEMOCRAT" ~ "Biden",
                          party == "REPUBLICAN" ~ "Trump"),
         percent = candidatevotes / totalvotes) %>% 
  pivot_wider(id_cols = county_fips, names_from = cand, values_from = c(percent, candidatevotes)) %>% 
  rename(Biden = candidatevotes_Biden,
       Trump = candidatevotes_Trump,
       pBiden = percent_Biden,
       pTrump = percent_Trump,
       GEOID = county_fips)
head(main)
