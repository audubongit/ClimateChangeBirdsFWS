

## extract the top mode for each species trend at each refuge

library(plyr)
library(dplyr)
library(data.table)
setwd('Z:/Climate_2_0/Climate_USFWS_Briefs/')

# read data of top two modes
trends <- fread('outputs/all_refuges_mode_tables_filter.csv')

# reduce down to the one top mode
mode <- trends %>%
  group_by(refuge_id, species, season, year) %>%
  arrange(desc(n)) %>%
  slice(which.max(n))

# check: how many unique refuges are there?
unique.t <- trends %>%
  select(refuge_id) %>%
  distinct() # 525 refuges

# how many species does each refuge have?
n_spp <- trends %>% 
  filter(year==2055) %>%
  group_by(refuge_id, season) %>% 
  mutate(n = length(species)) %>%
  select(refuge_id, season, year, n) %>% distinct() # lowest is 38 species

fwrite(mode, paste0('outputs/species_filtered_top_mode_trend_fws.csv'))