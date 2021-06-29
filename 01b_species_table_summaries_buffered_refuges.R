

library(data.table)
library(raster)
library(rgdal)
library(plyr)
library(tidyverse)
library(ff)

buffer <- 'Y'

# output directory
out_dir <- 'Z:/Climate_2_0/Climate_USFWS_Briefs/outputs/mode_outputs/'

## zone layers: using buffered zones
zones <- list.files('Z:/Climate_2_0/Climate_USFWS_Briefs/GIS/Refuges/2500m_buffer/', full.names=T, pattern='.tif$')
  
# helps to generate a data table
datalist <- list()

for (i in 1:107){

  one.zone <- as.data.frame(stack(zones[i])) %>%
    na.omit()
  
  zone <- one.zone %>%
    mutate(grid_id = rownames(one.zone), # rownames gets the original indices
           refuge = substr(names(one.zone[1]), 5,8)) %>%
    select(grid_id, refuge)
  
  datalist[[i]] <- zone
}

# Bind together all zonal statistics results from all zones (e.g., parks or states).
grid_id_zones <- dplyr::bind_rows(datalist)


# need to loop through each zone because of some spatial overlap

# species layers

# settings
spp <- NA #spp <- 'ATSP'
spp_group <- NA #'urban_suburban' # subset by group or use NA

github_path <- '~/github/climate2/climate20_fxns_domino/'
data_path <- 'Z:/Climate_2_0/ss_model_outputs_groups/'

# species lists from phenology tables
spp_summer <- dir(github_path, pattern='summer.csv', full.names=TRUE)
spp_winter <- dir(github_path, pattern='winter.csv', full.names=TRUE)

# get file paths
spp_paths_2025 <- data.frame(file=c(spp_summer, spp_winter), stringsAsFactors=FALSE) %>%
  mutate(data=lapply(file, fread, data.table=FALSE, drop=c('Drop.BCR', 'Drop.from.M', 'Add.to.M', 'V17', 'V18'))) %>%
  unnest(data) %>% rename_all(tolower) %>%
  mutate(season=ifelse(grepl('summer', file), 'summer', 'winter'),
         group=ifelse(group=='Climate.Watch', 'climatewatch', gsub('\\.', '_', tolower(group))),
         thresh=ifelse(is.na(threshold), 'tss', threshold),
         disp=ifelse(season=='summer' | type=='resident', '_dispersal', ''), 
         disp=ifelse(code %in% c('LAGO', 'PISI', 'RECR', 'WWCR'), '', disp), 
         clip=ifelse(!is.na(bcr.mask), '_clip', ''),
         file=paste0(data_path, 'ss_', group, '_', season, '/', code, '_ss_', group, '_', season,
                     '/future_predictions/', code, '_ENSEMBLE_85_2025_', thresh, '_pchg.tif'),
         col_name=paste(code,season,2025, sep='_'),
         year=2025) 
spp_paths_2055 <- spp_paths_2025 %>%
  mutate(file=str_replace(file, '2025','2055'),
         col_name=str_replace(col_name, '2025', '2055'),
         year=2055)
spp_paths_2085 <- spp_paths_2025 %>%
  mutate(file=str_replace(file, '2025','2085'), 
         col_name=str_replace(col_name, '2025', '2085'),
         year=2085)

spp_paths <- rbind(spp_paths_2025,spp_paths_2055,spp_paths_2085) %>% 
  filter(file.exists(file)) %>% 
  select(-notes, -round) %>%
  distinct() 

# check for missing paths
spp_paths_all <- rbind(spp_paths_2025,spp_paths_2055,spp_paths_2085) %>%
  select(-notes, -round) %>%
  distinct()
diff <- setdiff(spp_paths_all, spp_paths) %>%
  select(file, group, col_name, thresh)
#fwrite(diff, 'F:/Climate_2_0/data_tables/missing_F_drive_rasters.csv')
## end check

if(!is.na(spp)){spp_paths <- spp_paths %>% subset(code %in% spp)}
if(!is.na(spp_group)){spp_paths <- spp_paths %>% filter(group==spp_group)}

## ------ use ff package to create matrix to disk space rather than occupying RAM
# Code from: https://www.r-bloggers.com/extract-values-from-numerous-rasters-in-less-time/
# Still need to loop through each species so inidividual stacks do not max out integer value
out_dir <- 'Z:/Climate_2_0/Climate_USFWS_Briefs/outputs/buffered_outputs/'
  
fun.mode <- function(sp, refuges, species_paths) {
    
    sp_file <- filter(species_paths, code==sp) %>% pull(file) 
    sp_path <- filter(species_paths, code==sp)
    spp_stack <- stack(sp_file) # create raster stack on one species
    
    mat <- ff(vmode="double", dim=c(ncell(spp_stack), nlayers(spp_stack))) # create matrix with number of cells and layers of the stack
    
    for(i in 1:nlayers(spp_stack)){ # fill values from species stack to the matrix
      mat[,i] <- spp_stack[[i]][]
    }
    
    spp_df <- as.data.frame(mat[,])  # now convert to data frame
    spp_grid <- spp_df %>% 
      mutate(grid_id = rownames(spp_df)) %>% # add a column of grid ids 
      drop_na(V1)
    
    ## ---- Write all tallies and their counts ---- ##  
    # count number of cells of each value in each season and zone
    m=ncol(spp_grid)+1
    spp_summary <- inner_join(grid_id_zones, spp_grid) %>%
      rename_at(1:m, ~c('grid_id', 'zone', sp_path$col_name)) %>%
      filter(zone!=0) %>% # rename to summer/winter and remove NA zones
      gather(spp_season, value, 3:m) %>%
      drop_na(value) %>%
      dplyr::group_by(zone, spp_season, value) %>% 
      tally() %>% # remove ungroup() as it caused all 2/3 and 5/6 values to add up across the spreadsheet!
      ## first edit to combine the slightly classes
      rename(n_cells=n, pchg=value) %>% 
      separate(spp_season, c('species','season','year')) %>%
      mutate(
        value=ifelse(pchg==3, 2, ifelse(pchg==5, 6, pchg))) %>% # lump slightly into worsening or improving categories
      group_by(zone, species, season, year) %>%
      mutate(n=ifelse(value==2, sum(n_cells[pchg==2|pchg==3]), 
                      ifelse(value==6, sum(n_cells[pchg==6|pchg==5]), n_cells))) %>%
      select(zone, species, season, year, value, n) %>% distinct() %>%
      arrange(zone, year, species, season)
    fwrite(spp_summary, paste0(out_dir, 'all_zones_', sp, '_pchg_tally_table.csv'))
    
    ## ---- Keep just the mode numbers ---- ##  
    # count number of cells of each value in each season and zone
    mode <- spp_summary %>% 
      dplyr::group_by(zone, species, season, year) %>%
      mutate(sum_cells=sum(n), # add together all cells of that season and refuge, including 0 values
             prop_cells=n/sum_cells) %>% 
      arrange(desc(n)) %>%
      filter(value!=0) %>% 
      slice(1:2) %>% # this finds the first maximum occurrence
      select(zone, species, season, year, prop_cells, top_modes = value, n) %>%
      arrange(zone, species, season, year)
    fwrite(mode, paste0(out_dir, 'all_zones_', sp, '_pchg_mode_table.csv'))
}


# apply the function
# fun.mode(sp='LBBG', species_paths=spp_paths) # test one iteration

# apply to all paths in this group
species <- unique(spp_paths$code)

start.time <- Sys.time()
lapply(species,
       species_paths=spp_paths,
       FUN=fun.mode)
Sys.time() - start.time

## ----------------------- Consolidate tables of buffered refuges ----------------------- ##

# consolidate tables
out_dir <- 'Z:/Climate_2_0/Climate_USFWS_Briefs/outputs/buffered_outputs/'
setwd(out_dir)

# 1. tally tables
tally.files <- list.files(pattern="*_pchg_tally_table.")
all.tallies <- lapply(tally.files, FUN=fread,stringsAsFactors=FALSE) # read files into a list
merged.tallies <- do.call(rbind, all.tallies) # bind all lists into a table

# count number of cells of each refuge
# sum <- ddply(merged.tallies, c('zone','species','season','year'), summarise,
#             n = length(species)) # takes a long time
merged.tallies <- merged.tallies %>% subset(value!=0) # remove non-intersecting refuges
# add in geography names
ref_key <- fread("Z:/Climate_2_0/Climate_USFWS_Briefs/GIS/Refuges/USFWS_refuges_list.csv") %>%
  select(refuge_name, refuge_id)
  #rename(refuge_id=RefugeID, refuge_name=Name)
# ref_key$refuge_name[ref_key$refuge_name=='Bosque del Apache National Wildlife Refuge'] <- 'Bosque Del Apache National Wildlife Refuge'
# ref_key$refuge_name[ref_key$refuge_name=='Chase Lake Prairie Project Wetland Management District'] <- 'Chase Lake Wetland Management District'
# fwrite(ref_key, "Z:/Climate_2_0/Climate_USFWS_Briefs/GIS/Refuges/USFWS_refuges_list.csv")  

merged.tallies <- merged.tallies %>%
  rename(refuge_id=zone) %>%
  inner_join(ref_key)
fwrite(merged.tallies, '../all_buffered_refuges_tally_tables.csv')

# 2. mode tables
mode.files <- list.files(pattern="*_pchg_mode_table.csv")
all.modes <- lapply(mode.files, FUN=fread,stringsAsFactors=FALSE)
merged.modes <- do.call(rbind, all.modes)

# add in mode key
merged.modes <- merged.modes %>%
  rename(refuge_id=zone) %>%
  inner_join(ref_key)

## check table
# sum <- ddply(merged.modes, c('zone','species','season','year'), summarise,
#              n = length(species)) # takes a long time

fwrite(merged.modes, '../all_buffered_refuges_mode_tables.csv')

## ------------------ Overwrite small refuges with buffered refuges ------------------ ##
# if refuges are in the set of buffered refuges, overwrite those 
# 1. remove those refuges from consolidated mode tables
# 2. append the new tally and mode tables

## tally tables
# list zones to remove
zones.remove <- list.files('Z:/Climate_2_0/Climate_USFWS_Briefs/GIS/Refuges/2500m_buffer') %>%
  substr(5,8)
# remove buffered zones from old tally table
old.tally <- fread('Z:/Climate_2_0/Climate_USFWS_Briefs/outputs/all_refuges_tally_tables.csv')
new.tally <- old.tally %>% 
  filter(!grepl(paste(zones.remove, collapse='|'), refuge_id))
# merge removed tallies with buffered tallies
all.tallies <- bind_rows(new.tally, merged.tallies[,1:6])
all.tallies <- all.tallies %>%
  select(-refuge_name) %>%
  inner_join(ref_key)
fwrite(all.tallies, 'Z:/Climate_2_0/Climate_USFWS_Briefs/outputs/all_refuges_tally_tables.csv') # overwrite old tallies

## mode tables
old.mode <- fread('Z:/Climate_2_0/Climate_USFWS_Briefs/outputs/all_refuges_mode_tables.csv') 
new.mode <- old.mode %>%
  filter(!grepl(paste(zones.remove, collapse='|'), refuge_id))
all.modes <- bind_rows(new.mode, merged.modes) %>%
  select(-refuge_name) %>%
  inner_join(ref_key)
fwrite(all.modes, 'Z:/Climate_2_0/Climate_USFWS_Briefs/outputs/all_refuges_mode_tables.csv') # overwrite old modes

