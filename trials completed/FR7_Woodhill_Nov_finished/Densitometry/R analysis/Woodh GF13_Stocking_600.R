# Thales way

rm(list=ls())
setwd("Q:/Forest Systems/Projects/Silviculture breeds trials/trials completed/FR7_Woodhill_Nov_finished/Densitometry/R analysis")
# Set libraries
library(ggplot2)
library(ggforce)
library(data.table)
library(plyr)

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

# From all GF13 data select all 600spha and convert from data frame to data table and exclude NA values for Ring_Den
GF13_Stocking_600 <- GF13[GF13$Stocking=="600",]
GF13_Stocking_600DT <- as.data.table(GF13_Stocking_600)
GF13_Stocking_600new <- GF13_Stocking_600DT[!is.na(Ring_Den)]


GF13_Stocking_600new[0,]
unique(GF13_Stocking_600new$Stocking)
head(GF13_Stocking_600new)

# GF13_Stocking_600 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF13_Stocking_600new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF13_Stocking_600spha")
###################################################################################
#Plot 28

# Plot 28 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF13_Stocking_600new, Plot==28), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 28")

# Plot 28 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg28 <- ddply(.data=subset(GF13_Stocking_600new, Plot==28), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg28)
avg28sd <- ddply(.data=subset(GF13_Stocking_600new, Plot==28), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg28upper <- avg28$RingDen + avg28sd 
avg28lower <- avg28$RingDen - avg28sd 
head(avg28sd)



# Plot 28 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg28, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 650)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2828,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg28lower, ymax=avg28upper), avg28, alpha=0.28, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.28) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 28 ring mean density")


#############################################################################
#Plot 30
# Plot 30 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF13_Stocking_600new, Plot==30), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 30")

# Plot 30 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg30 <- ddply(.data=subset(GF13_Stocking_600new, Plot==30), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg30)
avg30sd <- ddply(.data=subset(GF13_Stocking_600new, Plot==30), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg30upper <- avg30$RingDen + avg30sd 
avg30lower <- avg30$RingDen - avg30sd 
head(avg30sd)



# Plot 30 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg30, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 650)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg30lower, ymax=avg30upper), avg30, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 30 ring mean density")




#############################################################################

#############################################################################




# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg2830 <- ddply(.data=subset(GF13_Stocking_600new, Plot==28|30), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg11154115$Plot<-as.character(avg2830$Plot)
avg2830$upper <- avg2830$RingDen + avg2830$SD 
avg2830$lower <- avg2830$RingDen - avg2830$SD 
head(avg2830)

#2 lines below needed to prevent legend changing to a colour bar when 4 plots see Woodhill GF13200spha file
avtemp<-avg2830
avtemp$Plot<-as.character(avtemp$Plot)
#######
ggplot(avtemp, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="left")+
  theme_bw() + ylim(300,600) +
  labs(title="GF13_Stocking_600")



#### average by RingDen by year for all GF13_600 spha  
avgGF13_600 <- ddply(.data=avg2830, .(Year), .fun=summarise, RingDen = mean(RingDen))




#### Plot average RingDen by year for all GF13_600 spha
print(ggplot(avgGF13_600, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,600) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF13_Stocking_600"))







