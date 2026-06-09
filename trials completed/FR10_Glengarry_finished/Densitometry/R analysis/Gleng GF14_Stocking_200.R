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

# From all GF14 data select all 200spha and convert from data frame to data table and exclude NA values for Ring_Den
GF14_Stocking_200 <- GF14[GF14$Stocking=="200",]
GF14_Stocking_200DT <- as.data.table(GF14_Stocking_200)
GF14_Stocking_200new <- GF14_Stocking_200DT[!is.na(Ring_Den)]


GF14_Stocking_200new[0,]
unique(GF14_Stocking_200new$Stocking)
head(GF14_Stocking_200new)

# GF14_Stocking_200 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF14_Stocking_200new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF14_Stocking_200spha")
###################################################################################
#Plot 12

# Plot 12 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF14_Stocking_200new, Plot==12), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 12")

# Plot 12 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg12 <- ddply(.data=subset(GF14_Stocking_200new, Plot==12), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg12)
avg12sd <- ddply(.data=subset(GF14_Stocking_200new, Plot==12), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg12upper <- avg12$RingDen + avg12sd 
avg12lower <- avg12$RingDen - avg12sd 
head(avg12sd)



# Plot 12 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg12, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg12lower, ymax=avg12upper), avg12, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 12 ring mean density")


#############################################################################
#Plot 14
# Plot 14 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF14_Stocking_200new, Plot==14), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 14")

# Plot 14 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg14 <- ddply(.data=subset(GF14_Stocking_200new, Plot==14), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg14)
avg14sd <- ddply(.data=subset(GF14_Stocking_200new, Plot==14), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg14upper <- avg14$RingDen + avg14sd 
avg14lower <- avg14$RingDen - avg14sd 
head(avg14sd)



# Plot 14 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg14, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg14lower, ymax=avg14upper), avg14, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 14 ring mean density")




#############################################################################
#Plot 43
# Plot 43 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF14_Stocking_200new, Plot==43), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 43")

# Plot 43 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg43 <- ddply(.data=subset(GF14_Stocking_200new, Plot==43), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg43)
avg43sd <- ddply(.data=subset(GF14_Stocking_200new, Plot==43), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg43upper <- avg43$RingDen + avg43sd 
avg43lower <- avg43$RingDen - avg43sd 
head(avg43sd)



# Plot 43 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg43, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg43lower, ymax=avg43upper), avg43, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 43 ring mean density")




#############################################################################
#Plot 46
# Plot 46 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF14_Stocking_200new, Plot==46), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 46")



# Plot 46 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg46 <- ddply(.data=subset(GF14_Stocking_200new, Plot==46), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg46)
avg46sd <- ddply(.data=subset(GF14_Stocking_200new, Plot==46), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg46upper <- avg46$RingDen + avg46sd 
avg46lower <- avg46$RingDen - avg46sd 
head(avg46sd)



# Plot 46 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg46, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg46lower, ymax=avg46upper), avg46, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 46 ring mean density")




#############################################################################




# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg12144346 <- ddply(.data=subset(GF14_Stocking_200new, Plot==12|14|43|46), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg11154115$Plot<-as.character(avg12144346$Plot)
avg12144346$upper <- avg12144346$RingDen + avg12144346$SD 
avg12144346$lower <- avg12144346$RingDen - avg12144346$SD 
head(avg12144346)

ggplot(avg12144346, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="right")+
  theme_bw() + ylim(300,500) +
  labs(title="GF14_Stocking_200")

#### average by RingDen by year for all GF14_200 spha  
avgGF14_200 <- ddply(.data=avg12144346, .(Year), .fun=summarise, RingDen = mean(RingDen))




#### Plot average RingDen by year for all GF14_200 spha
print(ggplot(avgGF14_200, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,510) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF14_Stocking_200"))







