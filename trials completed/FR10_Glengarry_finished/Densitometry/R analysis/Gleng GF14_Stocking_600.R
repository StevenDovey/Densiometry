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

# From all GF14 data select all 600spha and convert from data frame to data table and exclude NA values for Ring_Den
GF14_Stocking_600 <- GF14[GF14$Stocking=="600",]
GF14_Stocking_600DT <- as.data.table(GF14_Stocking_600)
GF14_Stocking_600new <- GF14_Stocking_600DT[!is.na(Ring_Den)]


GF14_Stocking_600new[0,]
unique(GF14_Stocking_600new$Stocking)
head(GF14_Stocking_600new)

# GF14_Stocking_600 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF14_Stocking_600new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF14_Stocking_600spha")
###################################################################################
#Plot 26

# Plot 26 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF14_Stocking_600new, Plot==26), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 26")

# Plot 26 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg26 <- ddply(.data=subset(GF14_Stocking_600new, Plot==26), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg26)
avg26sd <- ddply(.data=subset(GF14_Stocking_600new, Plot==26), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg26upper <- avg26$RingDen + avg26sd 
avg26lower <- avg26$RingDen - avg26sd 
head(avg26sd)



# Plot 26 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg26, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg26lower, ymax=avg26upper), avg26, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 26 ring mean density")


#############################################################################
#Plot 32
# Plot 32 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF14_Stocking_600new, Plot==32), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 32")

# Plot 32 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg32 <- ddply(.data=subset(GF14_Stocking_600new, Plot==32), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg32)
avg32sd <- ddply(.data=subset(GF14_Stocking_600new, Plot==32), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg32upper <- avg32$RingDen + avg32sd 
avg32lower <- avg32$RingDen - avg32sd 
head(avg32sd)



# Plot 32 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg32, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg32lower, ymax=avg32upper), avg32, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 32 ring mean density")




#############################################################################

# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg1632 <- ddply(.data=subset(GF14_Stocking_600new, Plot==16|32), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg1632$Plot<-as.character(avg1632$Plot)
avg1632$upper <- avg1632$RingDen + avg1632$SD 
avg1632$lower <- avg1632$RingDen - avg1632$SD 
head(avg1632)

ggplot(avg1632, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="right")+
  theme_bw() + ylim(300,500) +
  labs(title="GF14_Stocking_600")

#### average by RingDen by year for all GF14_600 spha  
avgGF14_600 <- ddply(.data=avg1632, .(Year), .fun=summarise, RingDen = mean(RingDen))




#### Plot average RingDen by year for all GF14_600 spha
print(ggplot(avgGF14_600, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,510) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF14_Stocking_600"))







