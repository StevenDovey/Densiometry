# Thales way

rm(list=ls())
setwd("Q:/Forest Systems/Projects/Silviculture breeds trials/trials completed/FR10_Glengarry_finished/Densitometry/R analysis")
# Set libraries
library(ggplot2)
library(ggforce)
library(data.table)
library(plyr)

# Read Glengarry densitometry file
rawdata <- read.csv('Glengarry FR10 densitometer data2.csv')
head(rawdata)
str(rawdata)
nrow(rawdata)
tail(rawdata)
################################################################################
#GF7

# From rawdata select all GF7
GF7 <- rawdata[rawdata$Breed=="GF7_climbing_select",]
head(GF7)
str(GF7)
nrow(GF7)
tail(GF7)

# From all GF7 data select all 600spha and convert from data frame to data table and exclude NA values for Ring_Den
GF7_Stocking_600 <- GF7[GF7$Stocking=="600",]
GF7_Stocking_600DT <- as.data.table(GF7_Stocking_600)
GF7_Stocking_600new <- GF7_Stocking_600DT[!is.na(Ring_Den)]


GF7_Stocking_600new[0,]
unique(GF7_Stocking_600new$Stocking)
head(GF7_Stocking_600new)

# GF7_Stocking_600 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF7_Stocking_600new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF7_Stocking_600spha")
###################################################################################
#Plot 25

# Plot 25 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF7_Stocking_600new, Plot==25), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 25")

# Plot 25 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg25 <- ddply(.data=subset(GF7_Stocking_600new, Plot==25), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg25)
avg25sd <- ddply(.data=subset(GF7_Stocking_600new, Plot==25), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg25upper <- avg25$RingDen + avg25sd 
avg25lower <- avg25$RingDen - avg25sd 
head(avg25sd)



# Plot 25 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg25, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg25lower, ymax=avg25upper), avg25, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 25 ring mean density")


#############################################################################
#Plot 30
# Plot 30 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF7_Stocking_600new, Plot==30), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 30")

# Plot 30 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg30 <- ddply(.data=subset(GF7_Stocking_600new, Plot==30), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg30)
avg30sd <- ddply(.data=subset(GF7_Stocking_600new, Plot==30), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg30upper <- avg30$RingDen + avg30sd 
avg30lower <- avg30$RingDen - avg30sd 
head(avg30sd)



# Plot 30 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg30, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg30lower, ymax=avg30upper), avg30, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 30 ring mean density")




#############################################################################

# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg2530 <- ddply(.data=subset(GF7_Stocking_600new, Plot==25|30), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg2530$Plot<-as.character(avg2530$Plot)
avg2530$upper <- avg2530$RingDen + avg2530$SD 
avg2530$lower <- avg2530$RingDen - avg2530$SD 
head(avg2530)

ggplot(avg2530, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="right")+
  theme_bw() + ylim(300,500) +
  labs(title="GF7_Stocking_600")

#### average by RingDen by year for all GF7_600 spha  
avgGF7_600 <- ddply(.data=avg2530, .(Year), .fun=summarise, RingDen = mean(RingDen))




#### Plot average RingDen by year for all GF7_600 spha
print(ggplot(avgGF7_600, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,510) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF7_Stocking_600"))







