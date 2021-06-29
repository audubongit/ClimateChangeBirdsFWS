
## extract climate suitability data to geographies of NWRS refuges
## National Audubon Society, December 2019, Joanna Wu

library(data.table)
library(raster)
library(rgdal)
library(plyr)
library(tidyverse)
library(ff)

# output directory
out_dir <- 'Z:/Climate_2_0/Climate_USFWS_Briefs/outputs/mode_outputs/'

## zone layers: using all zones
zones <- 'Z:/Climate_2_0/Climate_USFWS_Briefs/GIS/Refuges/rasters/USFWS_refuges_mask.tif'
zones_df <- as.data.frame(stack(zones))

# settings
spp <- NA # spp <- 'ATSP' # test
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

## ------ extract cell-by-cell classified suitability information ----- ##
# use ff package to create matrix to disk space rather than occupying RAM
# Code from: https://www.r-bloggers.com/extract-values-from-numerous-rasters-in-less-time/

fun.mode <- function(sp, species_paths) {
    
    sp_file <- filter(species_paths, code==sp) %>% pull(file) 
    sp_path <- filter(species_paths, code==sp)
    spp_stack <- stack(sp_file) # create raster stack on one species
    
    mat <- ff(vmode="double", dim=c(ncell(spp_stack), nlayers(spp_stack))) # create matrix with number of cells and layers of the stack
    
    for(i in 1:nlayers(spp_stack)){ # fill values from species stack to the matrix
      mat[,i] <- spp_stack[[i]][]
    }
    
    spp_df <- as.data.frame(mat[,]) # now convert to data frame
    
    # values of climate trends at each 1-km cell
    # 1 = potential extirpation
    # 2 = worsening
    # 3 = slightly worsening
    # 4 = stable
    # 5 = slightly improving
    # 6 = improving
    # 7 = potential colonization
    
    ## ---- Write all tallies and their counts ---- ##  
    # count number of cells of each value in each season and zone
    m=ncol(spp_df)+1
    spp_summary <- cbind(zones_df, spp_df) %>%
      rename_at(1:m, ~c('zone', sp_path$col_name)) %>%
      filter(zone!=0) %>% # rename to summer/winter and remove NA zones
      gather(spp_season, value, 2:m) %>%
      dplyr::group_by(zone, spp_season, value) %>% 
      drop_na(value) %>%
      tally() %>% 
      ## first edit to combine the slightly classes
      rename(n_cells=n, pchg=value) %>% 
      separate(spp_season, c('species','season','year')) %>%
      mutate(value=ifelse(pchg==3, 2, ifelse(pchg==5, 6, pchg))) %>% # lump slightly into worsening or improving categories
      group_by(zone, species, season, year) %>%
      mutate(n=ifelse(value==2, sum(n_cells[pchg==2|pchg==3]), 
                      ifelse(value==6, sum(n_cells[pchg==6|pchg==5]), n_cells))) %>%
      select(zone, species, season, year, value, n) %>% distinct() %>%
      arrange(zone, year, species, season)
    fwrite(spp_summary, paste0(out_dir,'all_zones_',sp,'_pchg_tally_table.csv'))
    
    ## ---- Keep just the mode numbers ---- ##  
    # count number of cells of each value in each season and zone
    mode <- spp_summary %>% 
      dplyr::group_by(zone, species, season, year) %>%
      mutate(sum_cells=sum(n), # add together all cells of that season and refuge, including 0 values to get area
             prop_cells=n/sum_cells) %>% 
      arrange(desc(n)) %>%
      filter(value!=0) %>% # remove 0 values
      slice(1:2) %>% # this finds the top two maximu cell counts (i.e., modes) within each refuge
      select(zone, species, season, year, prop_cells, top_modes = value, n) %>%
      arrange(zone, species, season, year)
    fwrite(mode, paste0(out_dir, 'all_zones_',sp,'_pchg_mode_table.csv'))
}

# test one iteration
# fun.mode('CANG', species_paths=spp_paths)

# apply to all paths in this group
species <- unique(spp_paths$code)

start.time <- Sys.time()
lapply(species, 
       FUN=fun.mode, 
       species_paths=spp_paths)
Sys.time() - start.time

## ----------------------- Consolidate tables of all groups ----------------------- ##

# consolidate tables
# add in refuge names
ref_key <- fread("Z:/Climate_2_0/Climate_USFWS_Briefs/GIS/Refuges/USFWS_refuges_list.csv") %>% 
  select(refuge_id, refuge_name)
# 1. tally tables
out_dir <- 'Z:/Climate_2_0/Climate_USFWS_Briefs/outputs/mode_outputs/'
setwd(out_dir)

tally.files <- list.files(pattern="*_pchg_tally_table.")
all.tallies <- lapply(tally.files, FUN=fread,stringsAsFactors=FALSE)
merged.tallies <- do.call(rbind, all.tallies)

# count number of trends of each refuge
# sum <- ddply(merged.tallies, c('zone','species','season','year'), summarise,
#             n = length(species)) # takes a long time
# foo <- merged.tallies %>% filter(is.na(n))

merged.tallies <- merged.tallies %>% subset(value!=0) %>% # remove non-intersecting refuges
  rename(refuge_id=zone) %>%
  inner_join(ref_key)
fwrite(merged.tallies, '../all_refuges_tally_tables.csv')

# 2. mode tables
mode.files <- list.files(pattern="*_pchg_mode_table.csv")
all.modes <- lapply(mode.files, FUN=fread,stringsAsFactors=FALSE)
merged.modes <- do.call(rbind, all.modes)

# count # of modes per refuge
# sum <- ddply(merged.modes, c('zone','species','season','year'), summarise,
#            n = length(species)) # takes a long time

## check table
merged.modes <- merged.modes %>%
  rename(refuge_id=zone) %>%
  inner_join(ref_key)
fwrite(merged.modes, '../all_refuges_mode_tables.csv')

