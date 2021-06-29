

## Plot overall proportions of 5 trends at each time period within each refuge

library(ggplot2)
library(dplyr)
library(reshape)
library(data.table)
library(viridis)
setwd("Z:/Climate_2_0/Climate_USFWS_Briefs/")

# --------------------------------   Facet summer and winter in panels ---------------------------------- #

# Read in if starting here:
all<-fread('outputs/species_trends_counts.csv')

# Create lists to apply to
pk<-unique(paste0(all$refuge_id))

all<-melt(all, id.vars=c('refuge_id', 'season', 'N','year'),
          measure.vars=c('improve', 'gain', 'stable', 'worsen', 'loss'),
          variable.name='trend', value.name='value')

# Factor, order
all$season<-factor(all$season,
                levels=c('summer', 'winter'),
                labels=c('Summer', 'Winter'),
                ordered=TRUE)
all$trend<-factor(all$trend,
                  levels=c('gain','improve','stable','worsen','loss'),
                  labels=c('Potential colonization','Improving','Stable','Worsening','Potential extirpation'),
                  ordered=TRUE)

# Viridis colors with black outline
fun.plot<-function(i, table) { 
  
  temp<-all[all$refuge_id==i,]
  
  p1<-ggplot(temp, aes(x=season, y=value))+          
    geom_bar(aes(fill=trend, colour=trend), stat="identity")
  p1+theme_bw(10)+  
    ylab("Number of species")+
    xlab(" ")+
    theme(strip.text.x = element_text(size = 12))+
    theme(axis.text = element_text(size=10, colour='black')) +
    theme(axis.title = element_text(size=12)) +
    scale_fill_manual(name="Climate suitability",
                      breaks=c('Potential colonization','Improving','Stable','Worsening','Potential extirpation'),
                      values=c('#74add1','#abd9e9','#ffffbf','#fdae61', '#f46d43'))+
    scale_colour_manual(name="Climate suitability",
                        breaks=c('Potential colonization','Improving','Stable','Worsening','Potential extirpation'),
                        values=c('black','black','black','black','black'))+
    theme(legend.text=element_text(size=10), legend.title=element_text(size=10),
          axis.text.x = element_text(angle = 45, hjust = 1), # rotate x-axis for season
          plot.margin = margin(0,0,0,0, "cm")) + 
    #facet_wrap(~Season)
    facet_wrap(~year) 
  
  ggsave(filename=paste0('Figures/zone_trends/plot_refuge_trends_filtered_',i,'.png'),
                                 height=4.6, width=7.5, units='in')
}

# fun.plot(3036, table=all) #test one

# all regions
lapply(pk, FUN=fun.plot, table=all)

