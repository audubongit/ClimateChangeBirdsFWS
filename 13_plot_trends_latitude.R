
  
  
# Plots of species change by latitude

library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
setwd("E:/Climate_2_0/Climate_USFWS_Briefs/")
# include AK (1) or not (0)? 2=AK birds only
ak=0

## --------------- For ms: turnover, col, ext. rates; high es; season-faceted ------------------- ##
# Manipulate short hand 
all<-fread('outputs/species_trends_long.csv') %>%
  # make new column to name season and variable type
  mutate(Season=ifelse(str_detect(variable, 'summer'), 'Summer', 'Winter'),
         Metric=ifelse(str_detect(variable, 'turnover'), 'Turnover', variable), 
         Metric=ifelse(str_detect(variable, 'loss'), 'Extirpation', Metric), # subsequently replace with "metric" to print metric variable
         Metric=ifelse(str_detect(variable, 'gain'), 'Colonization', Metric),
         Metric=ifelse(str_detect(variable, 'worsen'), 'Worsening', Metric),
         Metric=ifelse(str_detect(variable, 'stable'), 'Stable', Metric),
         Metric=ifelse(str_detect(variable, 'improve'), 'Improving', Metric))

# Manipulation to add p-values sig stars and r2
# excludes Alaska
if(ak==0){
  #lm<-fread('outputs/species_trends_lm_results.csv') # for lm
  lm<-fread('outputs/species_trends_betareg_results.csv') # for beta regression
  red=6025
  } 
# includes AK
if(ak==1){
  lm<-fread('outputs/species_trends_lm_results_with_ak.csv')
  red=7003
} 
if(ak==2){
  lm<-fread('outputs/species_trends_lm_results_only_ak.csv')
  red=7003
} 

if(ak!=2){ # adjusted for beta regression
  # Sig and r2 at highest latitude refuge_id for bray
  all$sig[all$refuge_id==red & all$Metric=='Turnover' & all$Season=='Summer'] <- lm$sig[lm$variable=='turnover_summer' & lm$reg.type=='beta']
  all$sig[all$refuge_id==red & all$Metric=='Turnover' & all$Season=='Winter'] <- lm$sig[lm$variable=='turnover_winter' & lm$reg.type=='beta']
  all$r2[all$refuge_id==red & all$Metric=='Turnover' & all$Season=='Summer'] <- round(lm$pseudo.r2[lm$variable=='turnover_summer' & lm$reg.type=='beta'], digits=2)
  all$r2[all$refuge_id==red & all$Metric=='Turnover' & all$Season=='Winter'] <- round(lm$pseudo.r2[lm$variable=='turnover_winter' & lm$reg.type=='beta'], digits=2)
  # Sig and r2 at highest latitude refuge_id for summer col and ext
  all$sig[all$refuge_id==red & all$Metric=='Colonization' & all$Season=='Summer'] <-  lm$sig[lm$variable=='gain.p_summer' & lm$reg.type=='beta']
  all$r2[all$refuge_id==red & all$Metric=='Colonization' & all$Season=='Summer'] <- round(lm$pseudo.r2[lm$variable=='gain.p_summer' & lm$reg.type=='beta'], digits=2)
  all$sig[all$refuge_id==red & all$Metric=='Extirpation' & all$Season=='Summer'] <- lm$sig[lm$variable=='loss.p_summer' & lm$reg.type=='beta']
  all$r2[all$refuge_id==red & all$Metric=='Extirpation' & all$Season=='Summer'] <-  round(lm$pseudo.r2[lm$variable=='loss.p_summer' & lm$reg.type=='beta'], digits=2)
  # Sig and r2 at highest latitude refuge_id for winter col and ext
  all$sig[all$refuge_id==red & all$Metric=='Colonization' & all$Season=='Winter'] <- lm$sig[lm$variable=='gain.p_winter' & lm$reg.type=='beta']
  all$r2[all$refuge_id==red & all$Metric=='Colonization' & all$Season=='Winter'] <- round(lm$pseudo.r2[lm$variable=='gain.p_winter' & lm$reg.type=='beta'], digits=2)
  all$sig[all$refuge_id==red & all$Metric=='Extirpation' & all$Season=='Winter'] <- lm$sig[lm$variable=='loss.p_winter' & lm$reg.type=='beta']
  all$r2[all$refuge_id==red & all$Metric=='Extirpation' & all$Season=='Winter'] <- round(lm$pseudo.r2[lm$variable=='loss.p_winter' & lm$reg.type=='beta'], digits=2)
}

if(ak==2) {
  # Only winter extirpation has significant trend
  all$sig[all$refuge_id==red & all$Metric=='Extirpation' & all$Season=='Winter'] <- lm$p.sig[lm$variable=='loss.p_winter' & lm$reg.type=='linear']
  all$r2[all$refuge_id==red & all$Metric=='Extirpation' & all$Season=='Winter'] <- round(lm$r2[lm$variable=='loss.p_winter' & lm$reg.type=='linear'], digits=2)
}

# Custom y values to place at the right positions
if(ak==0){
  all$y[all$refuge_id==red & all$Metric=='Colonization' & all$Season=='Summer']<-0.4
  all$y[all$refuge_id==red & all$Metric=='Turnover' & all$Season=='Summer']<-0.5
  all$y[all$refuge_id==red & all$Metric=='Extirpation' & all$Season=='Summer']<-0.59
  
  all$y[all$refuge_id==red & all$Metric=='Colonization' & all$Season=='Winter']<-0.84
  all$y[all$refuge_id==red & all$Metric=='Turnover' & all$Season=='Winter']<-0.66
  all$y[all$refuge_id==red & all$Metric=='Extirpation' & all$Season=='Winter']<-0.45
}
if(ak==1){
  all$y[all$refuge_id==red & all$Metric=='Colonization' & all$Season=='Summer']<-0.52
  all$y[all$refuge_id==red & all$Metric=='Turnover' & all$Season=='Summer']<-0.75
  all$y[all$refuge_id==red & all$Metric=='Extirpation' & all$Season=='Summer']<-0.62
  
  all$y[all$refuge_id==red & all$Metric=='Colonization' & all$Season=='Winter']<-1.1
  all$y[all$refuge_id==red & all$Metric=='Turnover' & all$Season=='Winter']<-0.82
  all$y[all$refuge_id==red & all$Metric=='Extirpation' & all$Season=='Winter']<-0.3
}
if(ak==2){
  all$y[all$refuge_id==red & all$Metric=='Extirpation' & all$Season=='Winter']<-0.15}

# Write writes to csv
if(ak==0){
  all<-subset(all, region!=11) %>%
  filter(Metric=='Colonization'|Metric=='Extirpation'|Metric=='Turnover')
  fwrite(all, 'outputs/species_3trends_2055_plots_excl_AK_beta.csv')
  }
if(ak==1){
  all<-all %>% 
    filter(Metric=='Colonization'|Metric=='Extirpation'|Metric=='Turnover')
  fwrite(all, 'outputs/species_3trends_2055_plots_incl_AK.csv')
  }
if(ak==2){
  all<-all %>% 
    filter(region==11) %>%
    filter(Metric=='Colonization'|Metric=='Extirpation'|Metric=='Turnover')
  fwrite(all, 'outputs/species_3trends_2055_plots_only_AK.csv')
  }


## ------------ plot starts here --------------- ##
if(ak==0){all<-fread('outputs/species_3trends_2055_plots_excl_AK_beta.csv')}
if(ak==1){all<-fread('outputs/species_3trends_2055_plots_incl_AK.csv')}
if(ak==2){all<-fread('outputs/species_3trends_2055_plots_only_AK.csv')}

p1<-ggplot(all, aes(x=latitude, y=sqrt(value), color=Metric, linetype=Metric))+
#p1<-ggplot(all, aes(x=latitude, y=value, color=Metric, linetype=Metric))+
  stat_smooth(method='lm', formula = y ~ x, alpha=0.3, level=0.95) # level indicates 95% confidence interval
p1+theme_bw(12)+
  ylab("Rate")+
  xlab("Latitude")+
  scale_colour_manual(values=c(Colonization=rgb(0,158,115,max=255), Extirpation=rgb(255,110,38,max=255), Turnover=rgb(0,114,178,max=255)))+
  scale_linetype_manual(values=c('longdash', 'dotted', 'solid'))+
  guides(colour=guide_legend(title=''), linetype=guide_legend(title=''))+
  geom_text(aes(label=sig, y=y+0.002), size=8, show.legend=F)+
  geom_text(aes(label=r2, vjust=1, y=y+0.001), size=4, show.legend=F)+
  #geom_point() +
  xlim(25, 50) + # without AK
  #xlim(25, 72)+ # with AK
  #xlim(54, 70) + # AK only
  facet_wrap(~Season, nrow=1) +
  theme(legend.key.width = unit(1.5, "cm")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(strip.background = element_blank())

if(ak==0){
  ggsave('Figures/refuges_latitude_trends_beta.png', height=3, width=7.5, units="in", dpi=600)
  ggsave('Figures/refuges_latitude_trends_points_beta.png', height=6, width=12, units="in", dpi=600)
  }
if(ak==1){
  ggsave('Figures/refuges_latitude_trends_with_ak.png', height=3, width=7.5, units="in", dpi=600)
  ggsave('Figures/refuges_latitude_trends_with_ak_points.png', height=6, width=12, units="in", dpi=600) # enlarge for plot with points
}
if(ak==2){
  ggsave('Figures/refuges_latitude_trends_only_ak.png', height=3, width=7.5, units="in", dpi=600)
  ggsave('Figures/refuges_latitude_trends_only_ak_points.png', height=6, width=12, units="in", dpi=600) # enlarge for plot with points
}

