# Thales way

rm(list=ls())
setwd("Q:/Forest Systems/Projects/Silviculture breeds trials/trials completed/FR7_Woodhill_Nov_finished/Densitometry/R analysis")
# Set libraries
library(ggplot2)
library(ggforce)
library(data.table)
library(plyr)
library(cowplot)

ROutput<-"Q:/Forest Systems/Projects/Silviculture breeds trials/trials completed/FR7_Woodhill_Nov_finished/Densitometry/R analysis/ROutput/"

# Read Glengarry densitometry file
rawdata <- read.csv('Woodhill FR7 densitometer data.csv')
head(rawdata)
str(rawdata)
nrow(rawdata)
tail(rawdata)
################################################################################
#GF13

# From rawdata select all GF13
GF13 <- rawdata[rawdata$Breed=="GF13_ LI28_870",]
head(GF13)
str(GF13)
nrow(GF13)
tail(GF13)

# From all GF13 data select all 100spha and convert from data frame to data table and exclude NA values for Ring_Den
GF13_Stocking_100 <- GF13[GF13$Stocking=="100",]
GF13_Stocking_100DT <- as.data.table(GF13_Stocking_100)
GF13_Stocking_100new <- GF13_Stocking_100DT[!is.na(Ring_Den)]


GF13_Stocking_100new[0,]
unique(GF13_Stocking_100new$Stocking)
head(GF13_Stocking_100new)

# GF13_Stocking_100 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF13_Stocking_100new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF13_Stocking_100spha")
###################################################################################
#Plot 1

# Plot 1 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF13_Stocking_100new, Plot==1), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 1")

# Plot 1 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg1 <- ddply(.data=subset(GF13_Stocking_100new, Plot==1), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg1)
avg1sd <- ddply(.data=subset(GF13_Stocking_100new, Plot==1), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg1upper <- avg1$RingDen + avg1sd 
avg1lower <- avg1$RingDen - avg1sd 
head(avg1sd)



# Plot 1 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg1, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 500)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg1lower, ymax=avg1upper), avg1, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 1 ring mean density")


#############################################################################
#Plot 5
# Plot 5 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF13_Stocking_100new, Plot==5), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 5")

# Plot 5 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg5 <- ddply(.data=subset(GF13_Stocking_100new, Plot==5), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg5)
avg5sd <- ddply(.data=subset(GF13_Stocking_100new, Plot==5), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg5upper <- avg5$RingDen + avg5sd 
avg5lower <- avg5$RingDen - avg5sd 
head(avg5sd)



# Plot 5 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg5, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 600)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg5lower, ymax=avg5upper), avg5, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 5 ring mean density")




#############################################################################

#############################################################################




# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg15 <- ddply(.data=subset(GF13_Stocking_100new, Plot==1|5), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg11154115$Plot<-as.character(avg15$Plot)
avg15$upper <- avg15$RingDen + avg15$SD 
avg15$lower <- avg15$RingDen - avg15$SD 
head(avg15)

#2 lines below needed to prevent legend changing to a colour bar when 4 plots see Woodhill GF13200spha file
avtemp<-avg15
avtemp$Plot<-as.character(avtemp$Plot)
#######
ggplot(avtemp, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="left")+
  theme_bw() + ylim(300,600) +
  labs(title="GF13_Stocking_100")



#### average by RingDen by year for all GF13_100 spha  
avgGF13_100 <- ddply(.data=avg15, .(Year), .fun=summarise, RingDen = mean(RingDen))




#### Plot average RingDen by year for all GF13_100 spha
p100Ave<-(ggplot(avgGF13_100, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,550) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF13_Stocking_100"))

print(p100Ave)

saveRDS(avgG13_100,paste0(ROutput,"aveGF13_100.RDS"))

avgG13_100<-readRDS(paste0(ROutput,"aveGF13_100.RDS"))

