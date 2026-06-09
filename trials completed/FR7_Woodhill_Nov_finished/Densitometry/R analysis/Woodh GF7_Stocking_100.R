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
#GF7

# From rawdata select all GF7
GF7 <- rawdata[rawdata$Breed=="GF7_climbing_select",]
head(GF7)
str(GF7)
nrow(GF7)
tail(GF7)

# From all GF7 data select all 100spha and convert from data frame to data table and exclude NA values for Ring_Den
GF7_Stocking_100 <- GF7[GF7$Stocking=="100",]
GF7_Stocking_100DT <- as.data.table(GF7_Stocking_100)
GF7_Stocking_100new <- GF7_Stocking_100DT[!is.na(Ring_Den)]


GF7_Stocking_100new[0,]
unique(GF7_Stocking_100new$Stocking)
head(GF7_Stocking_100new)

# GF7_Stocking_100 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF7_Stocking_100new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF7_Stocking_100spha")
###################################################################################
#Plot 2

# Plot 2 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF7_Stocking_100new, Plot==2), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 2")

# Plot 2 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg2 <- ddply(.data=subset(GF7_Stocking_100new, Plot==2), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg2)
avg2sd <- ddply(.data=subset(GF7_Stocking_100new, Plot==2), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg2upper <- avg2$RingDen + avg2sd 
avg2lower <- avg2$RingDen - avg2sd 
head(avg2sd)



# Plot 2 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg2, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 600)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg2lower, ymax=avg2upper), avg2, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 2 ring mean density")


#############################################################################
#Plot 6
# Plot 6 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF7_Stocking_100new, Plot==6), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 6")

# Plot 6 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg6 <- ddply(.data=subset(GF7_Stocking_100new, Plot==6), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg6)
avg6sd <- ddply(.data=subset(GF7_Stocking_100new, Plot==6), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg6upper <- avg6$RingDen + avg6sd 
avg6lower <- avg6$RingDen - avg6sd 
head(avg6sd)



# Plot 6 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg6, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 600)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg6lower, ymax=avg6upper), avg6, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 6 ring mean density")




#############################################################################

#############################################################################




# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg26 <- ddply(.data=subset(GF7_Stocking_100new, Plot==2|6), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg11154115$Plot<-as.character(avg26$Plot)
avg26$upper <- avg26$RingDen + avg26$SD 
avg26$lower <- avg26$RingDen - avg26$SD 
head(avg26)

#2 lines below needed to prevent legend changing to a colour bar when 4 plots see Woodhill GF7200spha file
avtemp<-avg26
avtemp$Plot<-as.character(avtemp$Plot)
#######
ggplot(avtemp, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="left")+
  theme_bw() + ylim(300,600) +
  labs(title="GF7_Stocking_100")



#### average by RingDen by year for all GF7_100 spha  
avgGF7_100 <- ddply(.data=avg26, .(Year), .fun=summarise, RingDen = mean(RingDen))




#### Plot average RingDen by year for all GF7_100 spha
print(ggplot(avgGF7_100, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,550) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF7_Stocking_100"))







