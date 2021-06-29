


## Sorensen species turnover index

library(vegan)
library(reshape2)
library(data.table)
library(tidyr)
library(dplyr)
setwd('Z:/Climate_2_0/Climate_USFWS_Briefs/')

# change parameters below then run through lines 106, repeat, then run last lines to consolidate.
breed <- 'summer' # 'summer', 'winter'
time <- 2055 # 2025, 2055, 2085

## ------------ Select data, manipulate to right format ------------------------ ##

dat<-fread('outputs/species_filtered_top_mode_trend_fws.csv') %>% filter(year==time) %>% filter(season==breed) %>%
  rename(site=refuge_id, mode=top_modes) %>%
  select(site, species, season, year, mode, trend)

# time 1: the species that are present currently: stable, increasing, worsening, and lost
t1<-dat[c(dat$mode %in% 1:6), ]

# time 2: the species that will be present in the future: stable, increasing, worsening, and gained
t2<-dat[c(dat$mode %in% 2:7), ]

# Shape input table
temp<-merge(t1[, c('species', 'site', 'mode', 'trend')], 
              t2[, c('species', 'site', 'mode', 'trend')], 
              by.x=c('species', 'site', 'mode'),
              by.y=c('species', 'site', 'mode'),
              all.x=T, all.y=T)

# Make binary when species are present or absent in present/future
temp$present[is.na(temp$trend.x)] <- 0
temp$present[!is.na(temp$trend.x)] <- 1
temp$future[is.na(temp$trend.y)] <- 0
temp$future[!is.na(temp$trend.y)] <- 1

# Remove unncessary columns
tempb<-temp[, c('species','site', 'present', 'future')]

# First shape to long to create the variables time and occurrence.
long<-reshape2::melt(tempb, id.vars=c('species', 'site'),
             variable.name='time',
             value.name='occurrence')

# Now cast to wide. Reshape doesn't allow for fun=sum, yielding NAs.
# This worked for casting all sites. 
wide<-reshape2::dcast(long, time + site ~ species, value.var='occurrence', fun=sum)
# Number each site for 1:n later
wide<-wide %>% 
  group_by(time) %>%
  mutate(id=row_number(1))

# Function to calculate Bray-Curtis (same as Sorensen except for abundance data) for each site and collate results 
sites<-as.character(unique(long$site))
sites<-sort(sites)
bray.list<-list()

fun.bray<-function(i, zones, table) {
for (i in zones[1:length(zones)]) {

  temp<-table[table$site==i, -c(1:2)]
 
  bc<-vegdist(temp, method='bray', binary=T) # binary, not abundance = Sorensen 
  value<-rbind(as.numeric(bc))
  value<-as.data.frame(value)
  
  bray.list[[i]]<-value
}

data<-do.call(rbind, Map(data.frame, site=names(bray.list), bray.list)) #extracts the names of the list, and writes corresponding values
colnames(data)[colnames(data)=='V1']<-'bray'

return(data)

}

if (breed=='summer' & time==2055) {summer_2055<-fun.bray(zones=sites, table=wide)}
if (breed=='winter' & time==2055) {winter_2055<-fun.bray(zones=sites, table=wide)}

# if (breed=='summer' & time==2025) {summer_2025<-fun.bray(zones=sites, table=wide)} # parameters for other time/season scenarios
# if (breed=='winter' & time==2025) {winter_2025<-fun.bray(zones=sites, table=wide)}
# 
# if (breed=='summer' & time==2085) {summer_2085<-fun.bray(zones=sites, table=wide)}
# if (breed=='winter' & time==2085) {winter_2085<-fun.bray(zones=sites, table=wide)}


## Check ###
for (i in sites[1:length(sites)]) {
  
  temp<-wide[wide$site==i, -c(1:2)]
  
  bc<-vegdist(temp, method='bray', binary=T) 
  value<-rbind(as.numeric(bc))
  value<-as.data.frame(value)
  
  bray.list[[i]]<-value
}

bray.list



## ------------ Merge all sites into one table ------------------------ ##
# Make distinguishable column headings
colnames(summer_2055)[colnames(summer_2055)=='bray']<-'summer_2055'
colnames(winter_2055)[colnames(winter_2055)=='bray']<-'winter_2055'

# Merge 4 tables using join_all
bray<-plyr::join_all(list(summer_2055, winter_2055), by='site', type='left')
# below: with both sets of years
# bray<-plyr::join_all(list(bbs45_2085,bbs45_2085,bbs85_2085,winter45_2085,winter45_2085,winter85_2085,
#                           bbs45_2055,bbs45_2055,bbs85_2055,winter45_2055,winter45_2055,winter85_2055), by='site', type='left')
head(bray)

## ------------ Add names & refuge xy ------------- ##

# read files
bray <- mutate(bray, ref = as.character(site),
               refuge_id = as.integer(ref)) %>%
  select(refuge_id, summer_2055, winter_2055)
ref.key <- fread("Z:/Climate_2_0/Climate_USFWS_Briefs/GIS/Refuges/USFWS_refuges_list.csv") %>% 
  select(refuge_id, refuge_name)

coords <- fread('GIS/Refuges/refuges_xy.csv') %>%
  select(refuge_id = RefugeID, latitude, longitude)

# join, rearrange, write
turnover <- inner_join(bray, ref.key) %>%
  inner_join(coords) %>%
  select(refuge_id, refuge_name, summer_2055, winter_2055, latitude, longitude)

head(turnover)
fwrite(turnover, 'Z:/Climate_2_0/Climate_USFWS_Briefs/outputs/bray_curtis_all.csv')

## summary metrics
summary(turnover)
sd(turnover$summer_2055/sqrt(525)) # SEM in summer
sd(turnover$winter_2055/sqrt(525)) # SEM in winter
