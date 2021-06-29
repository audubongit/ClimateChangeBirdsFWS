

## turnover rates by region

library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)

setwd('E:/Climate_2_0/Climate_USFWS_Briefs/')

turnover <- fread('outputs/bray_curtis_all.csv')
ref <- fread('GIS/Refuges/USFWS_refuges_new_regions.csv') %>%
  select(region, refuge_id=RefugeID)

turnover <- inner_join(ref, turnover)

# mean, sd, and se
mean(turnover$summer_2055); mean(turnover$winter_2055)
sd(turnover$summer_2055); sd(turnover$winter_2055)
sd(turnover$summer_2055)/sqrt(length(turnover$refuge_id));sd(turnover$winter_2055)/sqrt(length(turnover$refuge_id))

# summarise turnover rate within each region
region.t <- turnover %>%
  gather(season, turnover, summer_2055:winter_2055) %>%
  mutate(season = ifelse(season=='summer_2055', 'Summer', 'Winter')) %>%
  group_by(region, season) %>%
  summarize(mean = mean(turnover),
            sd.turnover = sd(turnover),
            se.turnover = sd.turnover/sqrt(length(refuge_id)))
fwrite(region.t, 'outputs/bray_curtis_regions.csv')

## ---------------- Plot turnover rates of regions as a bar plot with SEM ----------------- ##
# Facet both seasons
region.t <- fread('outputs/bray_curtis_regions.csv') %>%
  mutate(seas.mean = ifelse(season=='Summer', 0.23, 0.26)) # set overall mean to plot horizontal line
region.t$region <- as.character(region.t$region)
region.t$region <- ordered(region.t$region, levels=c('1','2','3','4','5','6','7','8','9','10','11'))

hex <- c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e','#003c30')
  
ggplot(region.t, aes(x=region, y=mean, fill=region, color=I('black'))) +
  geom_bar(stat='identity') +
  scale_x_discrete() +
  xlab('Region') +
  theme_bw(13) +
  ylab('Species turnover rate') +
  theme(legend.position='none') +
  scale_fill_manual(values=hex) +
  # add error bars
  geom_errorbar(aes(ymin=mean-se.turnover, ymax=mean+se.turnover), width=0.1) +
  facet_wrap(~season, ncol=1) +
  # add mean dotted line
  geom_hline(aes(yintercept=seas.mean), linetype='dashed') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(strip.background = element_blank())


ggsave('Figures/bray.curtis.regions.both.png', width=3, height=3.5)

## -------------- Differences in bray among regions? ---------- ##
b.mod<-lm(summer_2055 ~ region, data=turnover)
b.aov<-aov(summer_2055 ~ region, data=turnover) # same 
summary(b.aov)
summary(b.mod)

c.mod<-lm(winter_2055 ~ region, data=turnover)
summary(c.mod)
