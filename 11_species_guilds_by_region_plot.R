
library(dplyr)
library(data.table)
library(ggplot2)
library(tidyr)
setwd('Z:/Climate_2_0/Climate_USFWS_Briefs/')

time=2055

# read filtered data
dat <- fread('outputs/species_habitat_groups_by_region_filtered.csv') %>%
  filter(year==time)

## ------- Add up all cells of species groups within regions --------- ##
# sum up current and future values
counts.spp <- dat %>%
  group_by(species, region, group, season) %>% 
  mutate(num_2010=sum(num_cells[value %in% 1:6]), # sum current trend cells per species
         num_2050=sum(num_cells[value %in% 2:7])) %>%
  select(region, species, season, year, total_area, group, vulnerability, num_2010, num_2050) %>%
  distinct() %>%
  group_by(region, group, season) %>%
  mutate(area_2010=sum(num_2010),
         area_2050=sum(num_2050)) %>%
  select(region, season, group, area_2010, area_2050) %>% # species overlap in areas
  distinct()

# tally largest group by area
counts <- gather(counts.spp, year, count, area_2010:area_2050) %>%
  dplyr::group_by(region, season, group, year) %>%
  mutate(time = substr(year, 8,9),
         reg = paste(region, time, sep='.')) %>%
  dplyr::group_by(region, season, year) %>%
  mutate(proportion = count/sum(count)) %>%
  select(region, reg, season, time, group, count, proportion) %>%
  arrange(desc(proportion)) %>%
  group_by(region, season, time) %>%
  slice(1) # this finds the first maximum occurrence
# counts$grp <- factor(counts$grp, levels=c('Aridlands 2010','Boreal forests 2010','Eastern forests 2010','Generalists 2010', 'Waterbirds 2010',
#                                                     'Aridlands 2050','Boreal forests 2050','Eastern forests 2050','Generalists 2050'), ordered=T)

# assign colors to groups
unique(counts$group)
# for 8 groups:
# cbbPalette <- c("#E69F00", "#E69F00", "#CC79A7", "#CC79A7", "#56B4E9", "#56B4E9",
#                 "#009E73", "#E69F00", "#E69F00") 
# colors <- data.frame(unique(counts$grp), color.scheme=cbbPalette)
# for 5 groups:
cbbPalette <- c("#E69F00", "#CC79A7", "#56B4E9", "#009E73", "#E69F00", "#663366")
colors <- data.frame(unique(counts$group), color.scheme=cbbPalette)
#counts <- inner_join(counts, colors)

# set season
seas='winter'
counts.seas <- subset(counts, season==seas) 
#counts.seas$time <- factor(counts.seas$time, levels=c('10','50'), ordered=T)

# turn into a graph
#p <- ggplot(counts, aes(fill=group, y=count, x=region))
p <- ggplot(counts.seas, aes(x=reg, y=proportion, fill=group))
p + geom_bar(aes(fill=group), position='dodge', stat='identity') +
  labs(y='Proportion of region', x='Region', fill='Habitat group') +
  scale_x_discrete(labels=c('1.10'='1','1.50'='','2.10'='2','2.50'='','3.10'='3','3.50'='','4.10'='4','4.50'='',
                            '5.10'='5','5.50'='','6.10'='6','6.50'='','7.10'='7','7.50'='','8.10'='8','8.50'='',
                            '9.10'='9','9.50'='','10.10'='10','10.50'='','11.10'='11','11.50'='')) +
  theme(axis.text.x=element_text(hjust=-1.5)) # adjusts labels to the middle
#  scale_fill_manual(values=colors$color.scheme)

# continue on this plot
if(seas=='summer'){ggsave('Figures/regions_groups_proportions_summer.png', height=3.5, width=7.5, units="in")}
if(seas=='winter'){ggsave('Figures/regions_groups_proportions_winter.png', height=3.5, width=7.5, units="in")}



