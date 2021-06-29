
## 6/23/21 Edit: use beta regression rather than simple linear regression per reviewer 1 suggestion that
# turnover (and extirpation) rates are percentages and therefore have a beta error distribution


# regression of trends by latitude
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape)
library(data.table)
library(tidyr)
library(betareg)
setwd("E:/Climate_2_0/Climate_USFWS_Briefs/")
ak=0 # 0 if remove, 1 if keep Alaska, 2 if Alaska only

## --------------- Put gains, losses, and turnover in one file ------------------- ##
dat<-fread("outputs/species_trends_counts.csv") %>%
  filter(year==2055) %>%
  select(refuge_id, season, year, loss.p, worsen.p, stable.p, improve.p, gain.p)
bray<-fread('outputs/bray_curtis_all.csv') %>%
  dplyr::rename(summer=summer_2055, winter=winter_2055) %>%
  gather(season, turnover, summer:winter)
ref <- fread('Z:/Climate_2_0/Climate_USFWS_Briefs/GIS/Refuges/USFWS_refuges_new_regions.csv') %>%
  select(refuge_id=RefugeID, region)

# Merge together, write file
m <- inner_join(bray, dat) %>%
  inner_join(ref) %>%
  select(refuge_id, region, latitude, longitude, season, turnover, loss.p, worsen.p, stable.p, improve.p, gain.p)

m2 <- m %>% pivot_wider(names_from=season, values_from=c(turnover, loss.p, worsen.p, stable.p, improve.p, gain.p))

# take mean of seasons
per <- m2 %>%
  mutate(avg_turnover=(turnover_summer+turnover_winter)/2,
         avg_loss=(loss.p_summer+loss.p_winter)/2,
         avg_worsen=(worsen.p_summer+worsen.p_winter)/2,
         avg_stable=(stable.p_summer+stable.p_winter)/2,
         avg_improve=(improve.p_summer+improve.p_winter)/2,
         avg_persist=(worsen.p_summer+worsen.p_winter+stable.p_summer+stable.p_winter+improve.p_summer+improve.p_winter)/6,
         avg_gain=(gain.p_summer+gain.p_winter)/2)

fwrite(per, 'outputs/trend_proportions_bray_with_coordinates.csv')

## --------------- Linear regression: all trends and turnover rates ------------------- ##

# Data manipulation & set up
m2<-read.csv('outputs/trend_proportions_bray_with_coordinates.csv')
lat <- gather(m2, variable, value, turnover_summer:gain.p_winter) 
fwrite(lat, 'outputs/species_trends_long.csv')

# --- Apply linear regression by each variable type --- #
# If starting here 
lat<-fread('outputs/species_trends_long.csv')

# Remove Alaska?
if(ak==0) {lat <- subset(lat, region!=11)}
if(ak==2) {lat <- subset(lat, region==11)}

# regression of 3 rates
lat$latitude<-scale(lat$latitude, center=T, scale=F) # Center dependent variable

# lm: Apply lm to df
# b.lm <- dlply(lat, .(variable), function(f)
#   lm(value~latitude, data=f))
# beta regression
# rescale values to (0, 1) within each variable parameter
fun.rs <- function(x){(x-min(x-0.001))/(max(x+0.001)-min(x-0.001))} # formula to rescale betwene 0 and 1, toggle 
lat <- lat %>%
  group_by(variable) %>% # group by variable and rescale within each of them
  mutate(rs = fun.rs(value))
# perform regression
b.lm <- dlply(lat, .(variable), function(f)
  betareg(rs ~ latitude, data=f))

# Retrieve coefficients
b.coef <- ldply(b.lm, function(m)
  summary(m)$coefficients$mean)
b.aic <- ldply(b.lm, function(m)
  AIC(m))
# Data manip
names(b.coef)[5]<-"p" #change column name to p (complex characters)
#Add * if signficant
b.coef$sig<-ifelse(b.coef$p<0.001, '***',
                   ifelse(b.coef$p<0.01, '**',
                          ifelse(b.coef$p<0.05, '*',
                                 '')))
b.output<-b.coef[c(F,T),] # alternate lines, with the first referring to coefficients of the intercept
b.output$reg.type<-'beta'

# Add r2 and p-value to file
r2.beta<-ldply(b.lm,function(m) 
  summary(m)$pseudo.r.squared)
b.output$pseudo.r2<-r2.beta$V1

# Check normality of residuals
b.res<-llply(b.lm, function(m)
  residuals(m))
b.res.norm<-llply(b.res, function(m)
  shapiro.test(m)) 
b.res.norm 

# write output
#if(ak==0){fwrite(b.output, 'outputs/species_trends_lm_results.csv')} # linear regression
if(ak==0){fwrite(b.output, 'outputs/species_trends_betareg_results.csv')} # beta regression
if(ak==1){fwrite(b.output, 'outputs/species_trends_lm_results_with_ak.csv')}
if(ak==2){fwrite(b.output, 'outputs/species_trends_lm_results_only_ak.csv')}


