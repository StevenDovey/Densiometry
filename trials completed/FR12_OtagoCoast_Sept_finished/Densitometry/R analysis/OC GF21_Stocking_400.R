# Thales way

rm(list=ls())
setwd("Q:/Forest Systems/Projects/Silviculture breeds trials/trials completed/FR12_OtagoCoast_Sept_finished/Densitometry/R analysis")
# Set libraries
library(ggplot2)
library(ggforce)
library(data.table)
library(plyr)

# Read Otago Coast wood properties file
rawdata <- read.csv('Otago Coast FR12 densitometer data.csv')
head(rawdata)
str(rawdata)
nrow(rawdata)
tail(rawdata)
################################################################################
#GF21

# From rawdata select all GF21
GF21 <- rawdata[rawdata$Breed=="GF21_268",]
str(GF21)
nrow(GF21)
tail(GF21)

# From all GF21 data select all 400spha and convert from data frame to data table and exclude NA values for Ring_Den
GF21_Stocking_400 <- GF21[GF21$Stocking=="400",]
GF21_Stocking_400DT <- as.data.table(GF21_Stocking_400)
GF21_Stocking_400new <- GF21_Stocking_400DT[!is.na(Ring_Den)]


GF21_Stocking_400new[0,]
unique(GF21_Stocking_400new$Stocking)
head(GF21_Stocking_400new)

# GF21_Stocking_400 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF21_Stocking_400new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF21_Stocking_400spha")
###################################################################################
#Plot 31

# Plot 31 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF21_Stocking_400new, Plot==31), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 31")

# Plot 31 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg31 <- ddply(.data=subset(GF21_Stocking_400new, Plot==31), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg31)
avg31sd <- ddply(.data=subset(GF21_Stocking_400new, Plot==31), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
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
avg31 <- ddply(.data=subset(GF21_Stocking_400new, Plot==31), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg31$Plot<-as.character(avg31$Plot)
avg31$upper <- avg31$RingDen + avg31$SD 
avg31$lower <- avg31$RingDen - avg31$SD 
head(avg31)

ggplot(avg31, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="right")+
  theme_bw() + ylim(300,510) +
  labs(title="GF21_Stocking_400")

#### average by RingDen by year for all GF21_500 spha  
avgGF21_400 <- ddply(.data=avg31, .(Year), .fun=summarise, RingDen = mean(RingDen))



#### Plot average RingDen by year for all GF21_400 spha
print(ggplot(avgGF21_400, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,510) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF21_Stocking_400"))





