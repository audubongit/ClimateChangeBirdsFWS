
## filter present species by those documented in occurrence database.
## tally = counts of number of cells in each refuge under each trend class (e.g. improving or potential colonization)
## mode = the top two trends in each refuge (e.g. stable and worsening as the most common ones)

library(data.table)
library(plyr)
library(dplyr)
library(tidyverse)
library(tools)

## ----------------------- Consolidate tables of all groups ----------------------- ##
out_dir <- "Z:/Climate_2_0/Climate_USFWS_Briefs/outputs/"
setwd(out_dir)
sp_key <- fread('../Species_Lists/bst_4lettercode_2.0_species_key.csv') %>%
  select(species=Code, common_name=Common.Name)
# add in geography names
ref_key <- fread("Z:/Climate_2_0/Climate_USFWS_Briefs/GIS/Refuges/USFWS_refuges_list.csv") %>% 
  select(refuge_id, refuge_name) 

## ------ 1. tally tables, filtered and not filtered ------ ##
merged.tallies <- fread('all_refuges_tally_tables.csv')

joined <- merged.tallies %>%
  select(refuge_id, refuge_name, species, season, year, value, n) %>%
  arrange(refuge_id, refuge_name, species, season, year) # drop_na() # no more NAs!

# fwrite(joined, 'all_refuges_tally_tables.csv')

# filter tally tables by present species, and remove exotic species
spp <- fread("Z:/Climate_2_0/Climate_USFWS_Briefs/Species_Lists/USFWS_refuges_county_spp_list.csv") 
# some refuges had mismatched names, edit:
# spp$refuge_name[spp$refuge_name=='John Heinz National Wildlife Refuge At Tinicum'] <- 'John Heinz National Wildlife Refuge at Tinicum'
# spp$refuge_name[spp$refuge_name=='Kankakee National Wildlife Refuge And Conservation Area'] <- 'Kankakee National Wildlife Refuge and Conservation Area'
# spp$refuge_name[spp$refuge_name=='Big Muddy National Fish And Wildlife Refuge'] <- 'Big Muddy National Fish and Wildlife Refuge'
# spp$refuge_name[spp$refuge_name=='Upper Mississippi River National Wildlife And Fish Refuge'] <- 'Upper Mississippi River National Wildlife and Fish Refuge'
# spp$refuge_name[spp$refuge_name=='Everglades Headwaters National Wildlife Refuge And Conservation Area'] <- 'Everglades Headwaters National Wildlife Refuge and Conservation Area'
# spp$refuge_name[spp$refuge_name=='Eastern Shore Of Virginia National Wildlife Refuge'] <- 'Eastern Shore of Virginia National Wildlife Refuge'
# spp$refuge_name[spp$refuge_name=='Silvio O. Conte National Fish And Wildlife Refuge'] <- 'Silvio O. Conte National Fish and Wildlife Refuge'
# spp$refuge_name[spp$refuge_name=="D 'Arbonne National Wildlife Refuge"] <- "D'Arbonne National Wildlife Refuge"
# spp$refuge_name[spp$refuge_name=='Black River Unit Of Billy Frank Jr. Nisqually National Wildlife Refuge'] <- 'Black River Unit of Billy Frank Jr. Nisqually National Wildlife Refuge'
# spp$refuge_name[spp$refuge_name=='Julia Butler Hansen Refuge For The Columbian White-Tailed Deer'] <- 'Julia Butler Hansen Refuge for the Columbian White-Tailed Deer'
# spp$refuge_name[spp$refuge_name=='Lewis And Clark National Wildlife Refuge'] <- 'Lewis and Clark National Wildlife Refuge'
# spp$refuge_name[spp$refuge_name=='John W. And Louise Seier National Wildlife Refuge'] <- 'John W. and Louise Seier National Wildlife Refuge'
# spp$refuge_name[spp$refuge_name==''] <- ''

# fwrite(spp, "Z:/Climate_2_0/Climate_USFWS_Briefs/Species_Lists/USFWS_refuges_county_spp_list.csv")
spp <- spp %>% 
  select(refuge_name, species=species_code, season)
# read in exotic species to remove
exotic <- read.csv('../inputs/exotic_species.csv', header=T) %>% select(species)

# check: are all the refuge names the same in sp list and tallies?
ref.tallies <- unique(merged.tallies$refuge_id)
ref.tallies <- unique(merged.tallies$refuge_name)
ref.spp <- unique(spp$refuge_name)
setdiff(ref.tallies, ref.spp)
setdiff(ref.spp, ref.tallies)

filtered <- filter(joined, value %in% 1:6) %>%
  inner_join(spp[, 1:3]) %>%
  rbind(joined[joined$value==7,]) %>% # filter(n > 5) %>% # don't use this because there are small refuges
  arrange(refuge_name, species, season, year) %>%
  anti_join(exotic) 
fwrite(filtered, 'all_refuges_tally_tables_filter.csv')

# check all refuges made it
length(unique(filtered$refuge_name))

## ------ 2. mode tables, filtered by lists of eBird species ------ ##
merged.modes <- fread('all_refuges_mode_tables.csv') # %>% drop_na()
  
# mode key table
mode_key <- data.frame(c(1,2,4,6,7), c('Potential extirpation','Worsening','Stable','Improving','Potential colonization'))
colnames(mode_key) <- c('top_modes', 'trend')

# add in species key
joined <- merged.modes %>%
  left_join(sp_key) %>%
  left_join(mode_key) %>%
  select(refuge_id, refuge_name, species, common_name, season, year, prop_cells, top_modes, trend, n) 

# read in present species and remove exotics
spp <- fread("Z:/Climate_2_0/Climate_USFWS_Briefs/Species_Lists/USFWS_refuges_county_spp_list.csv") %>% 
  select(refuge_name, species=species_code, season)
exotic <- read.csv('../inputs/exotic_species.csv', header=T) %>% select(species)

# check: are all the refuge names the same in sp list and tallies?
ref.modes <- unique(joined$refuge_name)
ref.spp <- unique(spp$refuge_name)
setdiff(ref.modes, ref.spp)
setdiff(ref.spp, ref.modes)

# filter mode tables by present species, remove exotic species
filtered <- filter(joined, top_modes %in% 1:6) %>%
  inner_join(spp) %>% # filter present species by those in the occurrence database
  rbind(joined[joined$top_modes==7,]) %>% # filter(n > 5) %>% # don't use this because there are small refuges
  arrange(refuge_name, species, season, year) %>%
  anti_join(exotic) %>%
  filter(!(trend=='Potential colonization' & prop_cells < 0.01)) %>% # remove colonization trends comprising less than 1% of the refuge
  select(refuge_id, refuge_name, species, common_name, season, year, prop_cells, top_modes, trend, n)
head(filtered)

# check: number of refuges
length(unique(filtered$refuge_name))

fwrite(filtered, 'all_refuges_mode_tables_filter.csv')

