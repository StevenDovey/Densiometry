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
#GF21

# From rawdata select all GF21
GF21 <- rawdata[rawdata$Breed=="GF21_268",]
head(GF21)
str(GF21)
nrow(GF21)
tail(GF21)

# From all GF21 data select all 600spha and convert from data frame to data table and exclude NA values for Ring_Den
GF21_Stocking_600 <- GF21[GF21$Stocking=="600",]
GF21_Stocking_600DT <- as.data.table(GF21_Stocking_600)
GF21_Stocking_600new <- GF21_Stocking_600DT[!is.na(Ring_Den)]


GF21_Stocking_600new[0,]
unique(GF21_Stocking_600new$Stocking)
head(GF21_Stocking_600new)

# GF21_Stocking_600 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF21_Stocking_600new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF21_Stocking_600spha")
###################################################################################
#Plot 28

# Plot 28 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF21_Stocking_600new, Plot==28), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 28")

# Plot 28 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg28 <- ddply(.data=subset(GF21_Stocking_600new, Plot==28), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg28)
avg28sd <- ddply(.data=subset(GF21_Stocking_600new, Plot==28), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg28upper <- avg28$RingDen + avg28sd 
avg28lower <- avg28$RingDen - avg28sd 
head(avg28sd)



# Plot 28 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg28, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg28lower, ymax=avg28upper), avg28, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 28 ring mean density")


#############################################################################
#Plot 31
# Plot 31 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF21_Stocking_600new, Plot==31), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 31")

# Plot 31 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg31 <- ddply(.data=subset(GF21_Stocking_600new, Plot==31), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg31)
avg31sd <- ddply(.data=subset(GF21_Stocking_600new, Plot==31), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg31upper <- avg31$RingDen + avg31sd 
avg31lower <- avg31$RingDen - avg31sd 
head(avg31sd)



# Plot 31 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg31, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg31lower, ymax=avg31upper), avg31, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 31 ring mean density")




#############################################################################

#############################################################################




# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg2831 <- ddply(.data=subset(GF21_Stocking_600new, Plot==28|31), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg2831$Plot<-as.character(avg2831$Plot)
avg2831$upper <- avg2831$RingDen + avg2831$SD 
avg2831$lower <- avg2831$RingDen - avg2831$SD 
head(avg2831)

ggplot(avg2831, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="right")+
  theme_bw() + ylim(300,510) +
  labs(title="GF21_Stocking_600")



#### average by RingDen by year for all GF21_600 spha  
avgGF21_600 <- ddply(.data=avg2831, .(Year), .fun=summarise, RingDen = mean(RingDen))




#### Plot average RingDen by year for all GF21_600 spha
print(ggplot(avgGF21_600, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,510) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF21_Stocking_600"))







