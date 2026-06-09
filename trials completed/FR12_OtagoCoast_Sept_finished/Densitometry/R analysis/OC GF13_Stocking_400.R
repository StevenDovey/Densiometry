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
#GF13

# From rawdata select all GF13
GF13 <- rawdata[rawdata$Breed=="GF13_LI28_870_long_internode",]
str(GF13)
nrow(GF13)
tail(GF13)

# From all GF13 data select all 400spha and convert from data frame to data table and exclude NA values for Ring_Den
GF13_Stocking_400 <- GF13[GF13$Stocking=="400",]
GF13_Stocking_400DT <- as.data.table(GF13_Stocking_400)
GF13_Stocking_400new <- GF13_Stocking_400DT[!is.na(Ring_Den)]


GF13_Stocking_400new[0,]
unique(GF13_Stocking_400new$Stocking)
head(GF13_Stocking_400new)

# GF13_Stocking_400 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF13_Stocking_400new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF13_Stocking_400spha")
###################################################################################
#Plot 41

# Plot 41 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF13_Stocking_400new, Plot==41), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 41")

# Plot 41 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg41 <- ddply(.data=subset(GF13_Stocking_400new, Plot==41), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg41)
avg41sd <- ddply(.data=subset(GF13_Stocking_400new, Plot==41), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg41upper <- avg41$RingDen + avg41sd 
avg41lower <- avg41$RingDen - avg41sd 
head(avg41sd)



# Plot 41 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg41, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg41lower, ymax=avg41upper), avg41, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 41 ring mean density")


#############################################################################

#############################################################################


#### average by RingDen by year for all GF13_200 spha  
avgGF13_400 <- ddply(.data=avg41, .(Year), .fun=summarise, RingDen = mean(RingDen))



#### Plot average RingDen by year for all GF13_400 spha
print(ggplot(avgGF13_400, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,510) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF13_Stocking_400"))





