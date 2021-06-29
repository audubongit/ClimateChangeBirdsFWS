
# habitat groups by region
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(Hmisc)
setwd("Z:/Climate_2_0/Climate_USFWS_Briefs/")

# 3 ways of counting 2010 vs. 2050 common groups:
# 1 (simplest). take unique modes of species within an entire region. loses complexity about what's more common, but captures if a species is not there
# 2. count the number of refuges that have birds in each group in 2010 and 2050
# 3. (most complex) go back to tally tables, count all cells each species comprises in 2010 and 2050 and tally groups

# read in new regional breakdowns
new <- fread('Z:/Climate_2_0/Climate_USFWS_Briefs/GIS/Refuges/USFWS_refuges_new_regions.csv') %>%
  select(region, zone=RefugeID)

groups <- fread('Z:/Climate_2_0/all_results/all_species_vulnerability_capped.csv') %>%
  filter(Scenario=='2.0 deg C') %>%
  select(species=Code, scenario = Scenario, season=Season, group=Group, vulnerability=Vulnerability) 
groups$group <- gsub("_", " ", groups$group) # switch underscrore to space
groups$group <- capitalize(groups$group) # Hmisc capitalize first letter

## --------- count all cells refuges occupy, summarize by region (method 3) ----------- ##
# consolidate tally tables
out_dir <- 'Z:/Climate_2_0/Climate_USFWS_Briefs/outputs/mode_outputs/'
setwd(out_dir)

tally.files <- list.files(pattern="*_pchg_tally_table.")
all.tallies <- lapply(tally.files, FUN=fread,stringsAsFactors=FALSE)
merged.tallies <- do.call(rbind, all.tallies)
merged <- merged.tallies %>%
  group_by(zone, species, season, year) %>%
  mutate(ref_cells_km2=sum(n), # add together all cells of that season and refuge, including 0 values
         prop_cells=n/ref_cells_km2) %>%
  inner_join(new) %>% # to get new region numbers
  rename(refuge_id=zone)

# total area of refuges within regions
area.refs <- fread('Z:/Climate_2_0/Climate_USFWS_Briefs/GIS/Refuges/tables/refuges_raster_area.csv') %>%
  select(zone, n) %>%
  inner_join(new) %>%
  group_by(region) %>%
  mutate(total_area = sum(n)) %>%
  dplyr::select(region, total_area) %>%
  distinct()

region.sum <- merged %>%
  group_by(region, species, season, year, value) %>%
  mutate(num_cells=sum(n)) %>%
  dplyr::select(region, species, season, year, value, num_cells) %>%
  distinct()
region.sum$region <- as.numeric(region.sum$region)

region.vuln <- inner_join(region.sum, area.refs) %>%
  inner_join(groups) %>%
  filter(value!=0) %>%
  mutate(prop_cells = round(num_cells/total_area, 3)) %>%
  arrange(region, species) %>%
  select(region, species, season, year, value, num_cells, prop_cells, total_area, group, scenario, vulnerability) %>%
  filter(year==2055) %>%
  distinct()
head(region.vuln)  
fwrite(region.vuln, 'Z:/Climate_2_0/Climate_USFWS_Briefs/outputs/species_habitat_groups_by_region_unfiltered.csv')  

## filter observations by species list
ref.key <- fread('Z:/Climate_2_0/Climate_USFWS_Briefs/GIS/Refuges/USFWS_refuges_list.csv') %>%
  select(refuge_id, refuge_name)
spp <- fread("Z:/Climate_2_0/Climate_USFWS_Briefs/Species_Lists/USFWS_refuges_county_spp_list.csv") %>% 
  select(refuge_name, species=species_code, season) %>%
  inner_join(ref.key) %>%
  select(refuge_id, season, species)
exotic <- read.csv('Z:/Climate_2_0/Climate_USFWS_Briefs/inputs/exotic_species.csv', header=T) %>% select(species) # read in exotic species to remove

filtered <- merged %>%
  filter(value %in% 1:6) %>% # filter species that are here now
  inner_join(spp) %>%
  rbind(merged[merged$value==7,]) %>% # add in colonizers
  filter(!(value==7 & prop_cells < 0.01)) %>% # remove colonizers in <1% of the refuge
  arrange(refuge_id, species, season, year) %>%
  anti_join(exotic) %>%
  select(region, refuge_id, species, season, year, value, n) %>%
  distinct()
filtered$region <- as.numeric(filtered$region)

# consolidate into a region and save
region.vuln <- filtered %>%
  group_by(region, species, season, year, value) %>%
  mutate(num_cells=sum(n)) %>%
  select(-refuge_id) %>%
  distinct() %>%
  inner_join(area.refs) %>%
  inner_join(groups) %>%
  mutate(prop_cells = round(num_cells/total_area, 3)) %>%
  arrange(region, species) %>%
  select(region, species, season, year, value, num_cells, prop_cells, total_area, group, scenario, vulnerability) %>%
  filter(year==2055) %>% # inaccurate to join to to other vuln scenarios
  distinct()
head(region.vuln)
fwrite(region.vuln, 'Z:/Climate_2_0/Climate_USFWS_Briefs/outputs/species_habitat_groups_by_region_filtered.csv')
