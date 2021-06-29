

## Summarize trends of refuges by count
# Updated Jan 2020 #

library(data.table)
library(plyr)
library(dplyr)
setwd("Z:/Climate_2_0/Climate_USFWS_Briefs/")

## ------------------------ # ------------------------ # ------------------------ # ------------------------ ##

## Summarize suitability trends by refuge
# refuge, season, rcp, year

# mode outputs
dat<-fread(paste0('outputs/species_filtered_top_mode_trend_fws.csv'))

# Count the number of species within each park for which climate suitability is improving (6,7), stable (4), or worsening (1,2)
sum<-ddply(dat, c('refuge_id','season','year'), summarise, 
           loss=length(species[top_modes==1]),
           worsen=length(species[top_modes==2]),
           stable=length(species[top_modes==4]),
           improve=length(species[top_modes==6]),
           gain=length(species[top_modes==7]),
           N=sum(loss,worsen,stable,improve) # Total excludes species to be gained! Total of the current pool under rcps are not identical.
)

# Take proportions of species trends based on number of species
per<-ddply(sum, c('refuge_id','season','year'), summarise,
           loss=loss,
           loss.p=round(loss/N, 4),
           worsen=worsen,
           worsen.p=round(worsen/N, 4),
           stable=stable,
           stable.p=round(stable/N, 4),
           improve=improve,
           improve.p=round(improve/N, 4),
           gain=gain,
           gain.p=round(gain/N,4),
           N=N
)

ref <- fread('GIS/Refuges/USFWS_refuges_list.csv') %>% 
  select(refuge_id, refuge_name)
per <- inner_join(per, ref) 
per <- per[, c(1,15,2:14)]

# check
per.check <- ddply(per, c('refuge_id'), summarise,
                   num.row = length(refuge_name))
diff <- setdiff(ref$refuge_id, per.check$refuge_id) %>%
  data.frame(refuge_id=.) # none right now!

fwrite(per, 'outputs/species_trends_counts.csv')

## --------------- percent of trends that are significant? ------------------- ##

# 1. how many trends are not stable?
# in each season
sig <- ddply(dat, c('season','year'), summarise, 
           neg=length(species[top_modes==1|top_modes==2]),
           stable=length(species[top_modes==4]),
           pos=length(species[top_modes==6|top_modes==7]),
           N=sum(neg,stable,pos),
           neg.per = neg/N,
           stable.per = stable/N,
           pos.per = pos/N,
           sig.per = sum(neg.per, pos.per)
)
fwrite(sig, 'outputs/species_significant_trend_percents.csv')

# of both seasons, total species analyzed
sig2 <- ddply(dat, 'year', summarise, 
             neg=length(species[top_modes==1|top_modes==2]),
             stable=length(species[top_modes==4]),
             pos=length(species[top_modes==6|top_modes==7]),
             N=sum(neg,stable,pos),
             neg.per = neg/N,
             stable.per = stable/N,
             pos.per = pos/N,
             sig.per = sum(neg.per, pos.per)
)
fwrite(sig2, 'outputs/species_significant_trend_percents_across_seasons.csv')

## ------------------------ # ------------------------ # ------------------------ # ------------------------ ##

# break down vulnerability by groups

## 1. number highly vulnerable total, and remain vs. extirpate
trend.group <- fread('outputs/species_filtered_top_mode_trend_fws_vulnerability.csv') %>%
  filter(year==2055 & scenario=='2.0 deg C') %>% distinct() # scenario indicates RANGEWIDE vulnerability at the 2 deg scenario
sum <- trend.group %>%
  group_by(refuge_id, season) %>%
  mutate(vuln=ifelse((vulnerability=='H' | vulnerability=='M'), 1, 0), # assign 1 or 0 to vulnerability
         num_vuln=length(species[vuln==1]), # number spp vuln
         num_not_vuln=length(species[vuln==0]), # number spp not vuln
         vuln_remain=length(species[vuln==1 & top_modes %in% 2:7]), # num vuln spp that remain or colonize
         vuln_ext=length(species[vuln==1 & top_modes==1])) %>% # num vuln spp that extirpate
  select(refuge_id, refuge_name, season, num_not_vuln, num_vuln, vuln_remain, vuln_ext) %>%
  distinct()

fwrite(sum, 'outputs/trend_vulnerability_counts.csv')

## 2. number of vulnerable species in each group
grp <- trend.group %>%
  group_by(refuge_id, season, group) %>%
  mutate(vuln=ifelse((vulnerability=='H' | vulnerability=='M'), 1, 0), # assign 1 or 0 to vulnerability
         num_vuln=length(species[vuln==1]), # number spp vuln
         num_not_vuln=length(species[vuln==0]), # number spp not vuln
         vuln_remain=length(species[vuln==1 & top_modes %in% 2:7]), # num vuln spp that remain or colonize
         vuln_ext=length(species[vuln==1 & top_modes==1])) %>% # num vuln spp that extirpate
  select(refuge_id, refuge_name, season, group, num_not_vuln, num_vuln, vuln_remain, vuln_ext) %>%
  distinct()

fwrite(grp, 'outputs/trend_vulnerability_group_counts.csv')


## species with H and M vuln loss

sum <- trend.group %>%
  group_by(refuge_id, season) %>%
  mutate(vuln_h=ifelse((vulnerability=='H'), 1, 0), 
         vuln_m=ifelse((vulnerability=='M'), 1, 0), # assign 1 or 0 to vulnerability
         vuln_h_remain=length(species[vuln_h==1 & top_modes %in% 2:6]),
         vuln_m_remain=length(species[vuln_m==1 & top_modes %in% 2:6]),# num vuln spp that remain NOT colonize
         vuln_h_ext=length(species[vuln_h==1 & top_modes==1]),
         vuln_m_ext=length(species[vuln_m==1 & top_modes==1])) %>% # num vuln spp that extirpate
  select(refuge_id, refuge_name, season, vuln_h_remain, vuln_m_remain, vuln_h_ext, vuln_m_ext) %>%
  distinct()

## 3. number of current species across seasons, and number vulnerable overall
trends <- trend.group %>%
  filter(!top_modes==7) %>% # exclude colonizers
  select(refuge_id, refuge_name, species, vulnerability, season) %>%
  mutate(vuln.num = ifelse(vulnerability=='H',3,
                            ifelse(vulnerability=='M',2,
                                   ifelse(vulnerability=='L',1,
                                          ifelse(vulnerability=='N',0,NA)))))
# find most vulnerable season within refuges and species
trends$season <- factor(trends$season,levels = c("summer", "winter"), ordered = TRUE)
scores <- ddply(trends, c("refuge_id","species"), summarise,
                max.vuln = max(vuln.num),
                season = first(season[vuln.num==max.vuln]))
# join the max score within each refuge & species (across seasons) with all trends, removing non-matching trends
current <- trends %>%
  select(refuge_id, refuge_name, species, season) %>%
  inner_join(scores) %>%
  group_by(refuge_id) %>%
  mutate(vuln=ifelse((max.vuln==3 | max.vuln==2), 1, 0), # assign 1 or 0 to vulnerability
         current_num_vuln=length(species[vuln==1]), # number spp vuln
         num_not_vuln=length(species[vuln==0]), # number spp not vuln
         current_n=length(species),
         percent_vuln=current_num_vuln/current_n) %>% # # total species excludes colonizers
  select(refuge_id, refuge_name, current_num_vuln, current_n, percent_vuln) %>%
  distinct()
fwrite(current, 'outputs/trend_vulnerability_counts_current_species.csv')
