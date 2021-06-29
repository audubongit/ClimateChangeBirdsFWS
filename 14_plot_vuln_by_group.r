
# Plot vulnerability by species guilds for all species across regions

library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(gridExtra)

setwd("Z:/Climate_2_0/Climate_USFWS_Briefs/")

# vulnerability by state (species present in 2010)
tb <- fread('outputs/species_habitat_groups_by_region_filtered.csv') %>%
  filter(year==2055 & value!=7) %>%
  select(species, season, year, group, vulnerability) %>% # remove regions and select for distinct species
  distinct() %>%
  mutate(season=factor(season, levels=c('summer', 'winter'), labels=c('Summer', 'Winter'), ordered=TRUE), 
         group=replace(group, group=='Boreal forests', 'Boreal Forests'), 
         group=replace(group, group=='Eastern forests', 'Eastern Forests'), 
         group=replace(group, group=='Subtropical forests', 'Subtropical Forests'), 
         group=replace(group, group=='Western forests', 'Western Forests'), 
         #group=replace(group, group=='Urban suburban', '(Sub)Urban'), 
         vuln.fct=factor(vulnerability, levels=c('N', 'L', 'M', 'H'), labels=1:4, ordered=TRUE), 
         vuln.num=as.numeric(vuln.fct))

# function to build plots
fun.state.plots <- function(table) { # function(i, table) { 
  
  # filter to state
  temp <- tb #filter(tb, region==i)
  
  # choose most vulnerable season. if there is a tie, choose the summer season
  scores <- ddply(temp, c('species', 'season'), summarise, 
                  max.vuln=max(vuln.num), 
                  group=first(group))
  
  # summarize as numbers
  sum <- scores %>%
    group_by(group, season, max.vuln) %>%
    summarize(count=n()) %>%
    mutate(vuln=factor(max.vuln, levels=c(1,2,4,3), labels=c('Neutral','Low','High','Moderate'), ordered=TRUE),
           number.vuln=sum(count[vuln=='High'], count[vuln=="Moderate"]),
           number.not.vuln=sum(count[vuln=='Low'], count[vuln=="Neutral"])) %>%
    group_by(group, season) %>%
    mutate(count_group=sum(count)) %>%
    arrange(number.vuln, count_group)
  
  vuln <- c('High', 'Moderate', 'Low', 'Neutral')
  color_key <- c('#ee8786','#f4b375','#eeca4e','#669fc6')
  names(color_key) <- vuln
  
  # split by seasons
  sum.s <- filter(sum, season=='Summer')
  group.s <- unique(sum.s$group)
  offset.s <- 0.05*max(sum.s$number.vuln, sum.s$number.not.vuln)
  sum.w <- filter(sum, season=='Winter')
  group.w <- unique(sum.w$group)
  offset.w <- 0.05*max(sum.w$number.vuln, sum.w$number.not.vuln)
  
  # summer 
  plot.s <- ggplot(sum.s, aes(x=group, fill=vuln)) + 
    geom_bar(data=subset(sum.s, max.vuln >= 3), aes(y=count), position='stack', stat='identity') + 
    geom_bar(data=subset(sum.s, max.vuln < 3), aes(y=-count), position='stack', stat='identity') +
    geom_text(aes(label=number.vuln, y=number.vuln+offset.s), size=2.5) + # % labels
    geom_text(aes(label=number.not.vuln, y=-(number.not.vuln+offset.s)), size=2.5) + # % labels
    ggtitle('Summer') + labs(x=NULL, y=NULL) + 
    theme_bw() +
    theme(text=element_text(size=11),
          plot.title=element_text(size=10, hjust=0.5),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          plot.margin=margin(unit(c(4,3,1,3), unit="cm"))) +
    scale_x_discrete(limits=group.s) +
    scale_y_continuous(expand=c(0.1, 0.1)) +
    scale_fill_manual(values=color_key,
                      breaks=rev(names(color_key)),
                      guide=guide_legend()) + theme(legend.title = element_blank()) + theme(legend.position="bottom") + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    coord_flip()
  
  # winter
  plot.w <- ggplot(sum.w, aes(x=group, fill=vuln)) + 
    geom_bar(data=subset(sum.w, max.vuln >= 3), aes(y=count), position='stack', stat='identity') + 
    geom_bar(data=subset(sum.w, max.vuln < 3), aes(y=-count), position='stack', stat='identity') +
    geom_text(aes(label=number.vuln, y=number.vuln+offset.w), size=2.5) + # % labels
    geom_text(aes(label=number.not.vuln, y=-(number.not.vuln+offset.w)), size=2.5) + # % labels
    ggtitle('Winter') + labs(x=NULL, y=NULL) + 
    theme_bw() +
    theme(text=element_text(size=11),
          plot.title=element_text(size=10, hjust=0.5),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          plot.margin=margin(unit(c(4,3,1,3), unit="cm"))) +
    scale_x_discrete(limits=group.w) +
    scale_y_continuous(expand=c(0.1, 0.1)) +
    scale_fill_manual(values=color_key, 
                      breaks=rev(names(color_key)), 
                      guide=FALSE) + coord_flip() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  # shared legend
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  legend<-g_legend(plot.s)
  
  # arrange together
  png(paste0('E:/Climate_2_0/Climate_USFWS_Briefs/Figures/vulnerability_habitat_all.png'), 
         width=7.5, height=3.25, units='in', res=600)
  grid.arrange(arrangeGrob(plot.s + theme(legend.position='none'), plot.w, ncol=2, nrow=1),
               legend, nrow=2, heights=c(10,1))
  dev.off()
  
}

# one iteration
fun.state.plots(table=tb)

