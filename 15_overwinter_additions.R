

# Are gained species over the winter species already present in the summer?

library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
setwd("E:/Climate_2_0/Climate_USFWS_Briefs/")

# Using filtered, to 2055 data
all<-fread('outputs/species_filtered_top_mode_trend_fws.csv') %>%
  filter(year==2055)

# How many gained species in the winter are in the park in the summer?
# 1. Select out gained winter species by park
winter.gained<-all %>%
  subset(season=='winter' & top_modes==7) %>%
  select(refuge_id, refuge_name, species, common_name)

# 2. Which park-species in the summer match winter gains?
# a. Remove gains from the pool of bbs species
summer.nongains<-all %>%
  subset(season=='summer' & top_modes!=7) %>%
  select(refuge_id, refuge_name, species, common_name)

# b. Select for intersecting combos
overwinter<-match_df(winter.gained, summer.nongains) %>%
  mutate(region=substr(refuge_id, 1,1))
fwrite(overwinter, 'outputs/overwinter_species_gained_from_summer_2055.csv')

# 3. Which species are gained the most times? 
spp1<-overwinter %>%
  group_by(common_name) %>%
  summarise(n=length(refuge_id)) %>%
  arrange(desc(n))
head(spp1)
# wood duck (149), kestrel (124), mourning dove (113), cedar waxwing (106), bc night-heron (104), brewer's blackbird (104)

# 4. Count overwintering gains at each site
sum<-overwinter %>%
  group_by(refuge_id) %>%
  mutate(n = length(species)) %>%
  select(refuge_id, refuge_name, region, n) %>%
  distinct()

# 5. Which sites gain the most species?
overwinter<-fread('outputs/overwinter_species_gained_from_summer_2055.csv')
sites<-fread('GIS/Refuges/USFWS_refuges_list.csv') %>%
  select(refuge_id)

# Add in sites with 0 species gained
none <- anti_join(sites, overwinter) # 6 sites

# Create data frame to fill in missing values
new.reg <- fread('GIS/Refuges/USFWS_refuges_new_regions.csv') %>%
  select(refuge_id=RefugeID, refuge_name=name, region)
sum.missing<-data.frame(site=none,
                        n=0) %>%
  inner_join(new.reg)

# Bind together
sum.bind<-rbind(sum, sum.missing) %>%
  arrange(desc(n))
fwrite(sum.bind, 'outputs/overwinter_species_counts_2055.csv')

sum.bind<-fread('outputs/overwinter_species_counts_2055.csv')
mean<-sum.bind %>%
  summarise(mean=mean(n), 
            sd=sd(n),
            se=sd(n)/sqrt(525), 
            min=min(n),
            max=max(n))
mean


# 6. Percent of each park that will become overwintering additions.
all<-fread('outputs/species_filtered_top_mode_trend_fws.csv') %>%
  filter(year==2055)
current_n <- all%>%
  group_by(refuge_id, season) %>%
  filter(top_modes!=7) %>%
  filter(season=="summer") %>%
  summarise(current_n=length(species))

overw <- full_join(current_n, sum.bind) %>%
  mutate(percent = n/current_n)

overw %>% 
  summary() # 11.7 species, 10.7% of current summer species

overw %>%
  group_by(region) %>%
  mutate(percent.current = mean(percent)) %>%
  select(region, percent.current) %>%
  distinct()

# 7. Is there a difference by region?
reg.avg <- sum.bind %>%
  group_by(region) %>%
  mutate(mean.reg = mean(n),
         sem.reg = sd(n)/sqrt(length(n))) %>%
  select(region, mean.reg, sem.reg) %>%
  distinct() %>%
  arrange(region)
fwrite(reg.avg, 'outputs/overwinter_species_regional_means.csv')

over.reg<-fread('outputs/overwinter_species_counts_2055.csv')

lm<-lm(n ~ region, data=over.reg)
aov<-aov(n ~ region, data=over.reg)

summary(lm)
summary(aov)

group_by(over.reg, region) %>% 
  summarise(mean=mean(n), std=sd(n), se=std/sqrt(length(region)))

## Function to perform wilcox tests on all regions
shapiro.test(over.reg$n)
r<-unique(over.reg$region)

fun.wilcox<-function(i, regions, all) {
  for (i in regions[1:length(regions)]) {
    
    temp<-all[all$region==i, n]
    wfun<-wilcox.test(temp, all$n)
    
    print(paste('region',i, sep=' '))
    print(wfun)
    
  }
}

fun.wilcox(regions=r, all=over.reg)

wilcox.test(over.reg[over.reg$region==2, n], over.reg[, n]) #qc
t.test(over.reg[over.reg$region==3, n], over.reg[, n]) #qc

# Visualize plot
ggplot(over.reg, aes(x=region, y=n)) +
  geom_boxplot() +
  scale_x_discrete() +
  theme_bw(13) +
  xlab('') +
  theme(axis.text.x=element_text(angle=45, size=12, hjust=1)) +
  ylab('Overwintering gains') +
  geom_hline(yintercept=5.6)

# Linear regression with latitude
sites<-fread('GIS/Refuges/refuges_xy.csv') %>%
  select(refuge_id=RefugeID, latitude)
over.reg <- inner_join(over.reg, sites)

over.reg$lat<-scale(over.reg$latitude, center=T, scale=F) # Center dependent variable

lm.over<-lm(n~lat+I(lat^2), data=over.reg)
summary(lm.over) # sig quadratic

lm.over2<-lm(n~lat, data=over.reg)
summary(lm.over2) # non-sig linear

ggplot(over.reg, aes(x=lat, y=n)) +
  geom_point() # quadratic relationship; higher in the lower 48 then lower in AK


## ----------- Region queries --------------- ##
num.sp <- overwinter %>%
  filter(region==3) %>%
  group_by(common_name) %>%
  mutate(num=length(refuge_name)) %>%
  select(common_name, num) %>%
  distinct()
