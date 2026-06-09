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

# From all GF13 data select all 200spha and convert from data frame to data table and exclude NA values for Ring_Den
GF13_Stocking_200 <- GF13[GF13$Stocking=="200",]
GF13_Stocking_200DT <- as.data.table(GF13_Stocking_200)
GF13_Stocking_200new <- GF13_Stocking_200DT[!is.na(Ring_Den)]


GF13_Stocking_200new[0,]
unique(GF13_Stocking_200new$Stocking)
head(GF13_Stocking_200new)

# GF13_Stocking_200 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF13_Stocking_200new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF13_Stocking_200spha")
###################################################################################
#Plot 9

# Plot 9 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF13_Stocking_200new, Plot==9), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 9")

# Plot 9 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg9 <- ddply(.data=subset(GF13_Stocking_200new, Plot==9), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg9)
avg9sd <- ddply(.data=subset(GF13_Stocking_200new, Plot==9), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg9upper <- avg9$RingDen + avg9sd 
avg9lower <- avg9$RingDen - avg9sd 
head(avg9sd)



# Plot 9 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg9, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 600)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg9lower, ymax=avg9upper), avg9, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 9 ring mean density")


#############################################################################
#Plot 15
# Plot 15 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF13_Stocking_200new, Plot==15), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 15")

# Plot 15 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg15 <- ddply(.data=subset(GF13_Stocking_200new, Plot==15), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg15)
avg15sd <- ddply(.data=subset(GF13_Stocking_200new, Plot==15), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg15upper <- avg15$RingDen + avg15sd 
avg15lower <- avg15$RingDen - avg15sd 
head(avg15sd)



# Plot 15 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg15, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 600)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg15lower, ymax=avg15upper), avg15, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 15 ring mean density")




#############################################################################
#Plot 42
# Plot 42 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF13_Stocking_200new, Plot==42), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 42")

# Plot 42 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg42 <- ddply(.data=subset(GF13_Stocking_200new, Plot==42), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg42)
avg42sd <- ddply(.data=subset(GF13_Stocking_200new, Plot==42), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg42upper <- avg42$RingDen + avg42sd 
avg42lower <- avg42$RingDen - avg42sd 
head(avg42sd)



# Plot 42 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg42, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 600)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg42lower, ymax=avg42upper), avg42, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 42 ring mean density")




#############################################################################
#Plot 47
# Plot 47 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF13_Stocking_200new, Plot==47), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 47")



# Plot 47 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg47 <- ddply(.data=subset(GF13_Stocking_200new, Plot==47), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg47)
avg47sd <- ddply(.data=subset(GF13_Stocking_200new, Plot==47), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg47upper <- avg47$RingDen + avg47sd 
avg47lower <- avg47$RingDen - avg47sd 
head(avg47sd)



# Plot 47 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg47, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 600)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg47lower, ymax=avg47upper), avg47, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 47 ring mean density")




#############################################################################




# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg9154247 <- ddply(.data=subset(GF13_Stocking_200new, Plot==9|15|42|47), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg11154115$Plot<-as.character(avg9154247$Plot)
avg9154247$upper <- avg9154247$RingDen + avg9154247$SD 
avg9154247$lower <- avg9154247$RingDen - avg9154247$SD 
head(avg9154247)

#2 lines below needed to prevent legend changing to a colour bar 
avtemp<-avg9154247
avtemp$Plot<-as.character(avtemp$Plot)
#
ggplot(avtemp, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="left")+
  theme_bw() + ylim(300,600) +
  labs(title="GF13_Stocking_200")



#### average by RingDen by year for all GF13_200 spha  
avgGF13_200 <- ddply(.data=avg9154247, .(Year), .fun=summarise, RingDen = mean(RingDen))




#### Plot average RingDen by year for all GF13_200 spha
print(ggplot(avgGF13_200, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,550) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF13_Stocking_200"))







