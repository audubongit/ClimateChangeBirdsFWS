
# site adaptation classes

library(reshape2)
library(dplyr)
library(ggplot2)
library(viridis)
library(sp)
library(rgeos)
library(data.table)
setwd('E:/Climate_2_0/Climate_USFWS_Briefs/')


## -------------- Selecting out moderate change class: summer -------------------- ##
b<-read.csv('outputs/species_trends_counts.csv') %>%
  subset(season=='summer' & year==2055) %>%
  select(refuge_id, gain.p, loss.p)

# Specify coordinates of diamond with quantiles as x, y
x<-as.numeric(c(quantile(b$gain.p, 0.25), quantile(b$gain.p, 0.5), quantile(b$gain.p, 0.75), quantile(b$gain.p, 0.5), quantile(b$gain.p, 0.25)))
y<-as.numeric(c(quantile(b$loss.p, 0.5), quantile(b$loss.p, 0.75), quantile(b$loss.p, 0.5), quantile(b$loss.p, 0.25), quantile(b$loss.p, 0.5)))

# Use sp to create a spatial poly 
ply <- matrix(c(x,y), ncol = 2)
ply <- SpatialPolygons(list(Polygons(list(Polygon(ply)), ID = 1)))
ply <- SpatialPolygonsDataFrame(Sr = ply, data = data.frame(polyvar = 1))

# Create a spatial point data set
#pts<-data.frame(x=b$G8.5p, y=b$L8.5p) # filtered
pts<-data.frame(x=b$gain.p, y=b$loss.p)  
coordinates(pts) <- ~x+y
plot(pts)
plot(ply, add=T)

# Use rgeos::gIntersection to overlay points
pts.intersect<-gIntersection(ply, pts, byid=T)
# Extract point IDs from intersected data
pts.intersect.strsplit <- strsplit(dimnames(pts.intersect@coords)[[1]], " ")
pts.intersect.id <- as.numeric(sapply(pts.intersect.strsplit, "[[", 2))

# Subset original data frame by extracted point IDs
pts<-data.frame(x=b$gain.p, y=b$loss.p) # non park-filtered
df.in <- pts[pts.intersect.id, ]

# subset parks that have these gain, loss "coordinates"
b$class<-'default'
b.mod<-b[b$gain.p %in% df.in$x & b$loss.p %in% df.in$y, ]  

# Define the 5 classes: not filtered
# 1. High turnover class: high losses, high gains
b$class[b$loss.p >= as.numeric(quantile(b$loss.p, 0.5)) & b$gain.p >= as.numeric(quantile(b$gain.p, 0.5))] <- "High turnover"
# 2. High losses: high losses, low colonizations
b$class[b$loss.p >= as.numeric(quantile(b$loss.p, 0.5)) & b$gain.p <= as.numeric(quantile(b$gain.p, 0.5))] <- "High potential extirpation"
# 3. High persistence: low colonization, low losses
b$class[b$loss.p <= as.numeric(quantile(b$loss.p, 0.5)) & b$gain.p <= as.numeric(quantile(b$gain.p, 0.5))] <- "Low change"
# 4. High gains: high colonization, low losses
b$class[b$loss.p <= as.numeric(quantile(b$loss.p, 0.5)) & b$gain.p >= as.numeric(quantile(b$gain.p, 0.5))] <- "High potential colonization"
# 5. Moderate change trends
b$class[b$refuge_id %in% b.mod$refuge_id] <- 'Intermediate change'
# 6. Write
coord <- fread('GIS/Refuges/refuges_xy.csv') %>% select(refuge_id=RefugeID, latitude, longitude) 
regions <- fread('GIS/Refuges/USFWS_refuges_new_regions.csv') %>% select(refuge_id=RefugeID, region, refuge_name=name)
b <- inner_join(b, coord) %>%
  inner_join(regions)
fwrite(b, 'outputs/adaptation_classes_refuges_2055.csv')

## ------ ## --------- ##

b <- fread('outputs/adaptation_classes_refuges_2055.csv') %>%
  select(refuge_id, class, region)
bray <- fread('outputs/bray_curtis_all.csv')
join <- inner_join(bray, b) %>%
  select(refuge_id, refuge_name, region, turnover_summer=summer_2055, turnover_winter=winter_2055, class, latitude, longitude)
fwrite(join, 'outputs/adaptation_classes_turnover_refuges_2055.csv')

## ----- Plot with 5 adaptation classes ----- ##
# If starting here
b <- fread('outputs/adaptation_classes_refuges_2055.csv') 

# function to round to 2 decimal places
scaleFUN <- function(x) sprintf("%.2f", x)

poly=data.frame(x=c(quantile(b$gain.p)[3],quantile(b$gain.p)[4],quantile(b$gain.p)[3],quantile(b$gain.p)[2]),
                y=c(quantile(b$loss.p)[2],quantile(b$loss.p)[3],quantile(b$loss.p)[4],quantile(b$loss.p)[3]))

ggplot(data=b, aes(x=gain.p, y=loss.p))+  # non park-filtered
  geom_point(aes(fill=class), size=2.25, pch=21)+
  theme_bw() +
  theme(panel.grid.major = element_line(colour='grey80',size=0.1), panel.grid.minor = element_line(colour='grey80',size=0.1))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scaleFUN)+
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scaleFUN)+
  xlab('Proportion of colonizations')+
  ylab('Proportion of extirpations')+
  scale_fill_manual(name="Park trends",
                    breaks=c('High turnover', 'High potential extirpation', 'High potential colonization', 'Intermediate change', 'Low change'),
                    values=c('#6dbeeb', '#ffffff', '#fc6c49', '#8fd33f', '#fee749'),
                    guide="none")+
  geom_segment(aes(x=median(b$gain.p), xend=median(b$gain.p), y=0, yend=quantile(b$loss.p)[2]), colour = "white", size = 1, linetype=1)+
  geom_segment(aes(x=median(b$gain.p), xend=median(b$gain.p), y=quantile(b$loss.p)[4], yend=Inf), colour = "white", size = 1, linetype=1)+
  geom_segment(aes(y=median(b$loss.p), yend=median(b$loss.p), x=0, xend=quantile(b$gain.p)[2]), colour = "white", size = 1, linetype=1)+
  geom_segment(aes(y=median(b$loss.p), yend=median(b$loss.p), x=quantile(b$gain.p)[4], xend=Inf), colour = "white", size = 1, linetype=1)+
  geom_polygon(data=poly, aes(x,y), colour="white", fill=NA, size=1, linetype=1)+
  geom_segment(aes(x=median(b$gain.p), xend=median(b$gain.p), y=quantile(b$loss.p)[4], yend=1), colour = NA, size = 0)+
  geom_segment(aes(y=median(b$loss.p), yend=median(b$loss.p), x=quantile(b$gain.p)[4], xend=1), colour = NA, size = 0)+
  geom_segment(aes(x=median(b$gain.p), xend=median(b$gain.p), y=0, yend=quantile(b$loss.p)[2]), colour = "black", size = 0.5, linetype=1)+
  geom_segment(aes(x=median(b$gain.p), xend=median(b$gain.p), y=quantile(b$loss.p)[4], yend=Inf), colour = "black", size = 0.5, linetype=1)+
  geom_segment(aes(y=median(b$loss.p), yend=median(b$loss.p), x=0, xend=quantile(b$gain.p)[2]), colour = "black", size = 0.5, linetype=1)+
  geom_segment(aes(y=median(b$loss.p), yend=median(b$loss.p), x=quantile(b$gain.p)[4], xend=Inf), colour = "black", size = 0.5, linetype=1)+
  geom_polygon(data=poly, aes(x,y), colour="black", fill=NA, size=0.5, linetype=1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


ggsave(filename="Figures/refuges_adaptation_classes.tif", device="tiff", height=3.5, width=3.5, units="in", dpi=600, compression="lzw")
ggsave(filename="Figures/refuges_adaptation_classes.png", height=3.5, width=3.5, units="in", dpi=600) 

## ----- Bar plot of regional summaries ----- ##
geo <- b %>%
  group_by(region, class) %>%
  summarize(n=length(refuge_id)) %>%
  mutate(tot.reg=sum(n),
         prop=n/tot.reg,
         t=tot.reg)
geo$t[geo$class=='Low change'|geo$class=='Intermediate change'] <- NA

geo$class<-factor(geo$class,
                  levels=c('High turnover', 'High potential extirpation', 'High potential colonization', 'Intermediate change', 'Low change', ordered=T))
geo$region <- as.character(geo$region)
geo$region <- ordered(geo$region, levels=c('1','2','3','4','5','6','7','8','9','10','11'))

ggplot(geo, aes(x=region, y=prop, fill=class))+
  geom_bar(aes(fill=class), colour="black", stat='identity')+
  theme_bw() +
  theme(panel.grid.major = element_line(colour='grey80',size=0.1), panel.grid.minor = element_line(colour='grey80',size=0.1))+
  theme(strip.text.x = element_text(size = 12))+
  theme(axis.text.x=element_text(angle=35, hjust=1))+
  ylab("Proportion of refuges")+
  xlab('Region')+
  scale_fill_manual(name="Trends",
                    breaks=c('High turnover', 'High potential extirpation', 'High potential colonization', 'Intermediate change', 'Low change'),
                    values=c('#fc6c49', '#ffffff', '#6dbeeb', '#8fd33f', '#fee749')) +
  geom_text(aes(label=t, y=1.04), size=3)

# ggsave(filename="../Figures/S1_Figure.tif", device="tiff", height=3.5, width=5, units="in", dpi=600, compression="lzw")
ggsave(filename="Figures/refuges_adaptation_region_plot.png", device="png", height=3.5, width=5, units="in", dpi=600)
# ggsave(filename="../Figures/S1_Figure.eps", device="eps", height=3.5, width=5, units="in", dpi=600)
                    
