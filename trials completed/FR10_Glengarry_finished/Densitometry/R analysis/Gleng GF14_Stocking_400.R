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
#GF14

# From rawdata select all GF14
GF14 <- rawdata[rawdata$Breed=="GF14_850",]
head(GF14)
str(GF14)
nrow(GF14)
tail(GF14)

# From all GF14 data select all 400spha and convert from data frame to data table and exclude NA values for Ring_Den
GF14_Stocking_400 <- GF14[GF14$Stocking=="400",]
GF14_Stocking_400DT <- as.data.table(GF14_Stocking_400)
GF14_Stocking_400new <- GF14_Stocking_400DT[!is.na(Ring_Den)]


GF14_Stocking_400new[0,]
unique(GF14_Stocking_400new$Stocking)
head(GF14_Stocking_400new)

# GF14_Stocking_400 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF14_Stocking_400new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF14_Stocking_400spha")
###################################################################################
#Plot 17

# Plot 17 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF14_Stocking_400new, Plot==17), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 17")

# Plot 17 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg17 <- ddply(.data=subset(GF14_Stocking_400new, Plot==17), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg17)
avg17sd <- ddply(.data=subset(GF14_Stocking_400new, Plot==17), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg17upper <- avg17$RingDen + avg17sd 
avg17lower <- avg17$RingDen - avg17sd 
head(avg17sd)



# Plot 17 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg17, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg17lower, ymax=avg17upper), avg17, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 17 ring mean density")


#############################################################################
#Plot 23
# Plot 23 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF14_Stocking_400new, Plot==23), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 23")

# Plot 23 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg23 <- ddply(.data=subset(GF14_Stocking_400new, Plot==23), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg23)
avg23sd <- ddply(.data=subset(GF14_Stocking_400new, Plot==23), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg23upper <- avg23$RingDen + avg23sd 
avg23lower <- avg23$RingDen - avg23sd 
head(avg23sd)



# Plot 23 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg23, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg23lower, ymax=avg23upper), avg23, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 23 ring mean density")




#############################################################################

# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg1723 <- ddply(.data=subset(GF14_Stocking_400new, Plot==17|23), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg1723$Plot<-as.character(avg1723$Plot)
avg1723$upper <- avg1723$RingDen + avg1723$SD 
avg1723$lower <- avg1723$RingDen - avg1723$SD 
head(avg1723)

ggplot(avg1723, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="right")+
  theme_bw() + ylim(300,500) +
  labs(title="GF14_Stocking_400")

#### average by RingDen by year for all GF14_400 spha  
avgGF14_400 <- ddply(.data=avg1723, .(Year), .fun=summarise, RingDen = mean(RingDen))




#### Plot average RingDen by year for all GF14_400 spha
print(ggplot(avgGF14_400, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,510) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF14_Stocking_400"))







