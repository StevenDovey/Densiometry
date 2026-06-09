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

# From all GF13 data select all 500spha and convert from data frame to data table and exclude NA values for Ring_Den
GF13_Stocking_500 <- GF13[GF13$Stocking=="500",]
GF13_Stocking_500DT <- as.data.table(GF13_Stocking_500)
GF13_Stocking_500new <- GF13_Stocking_500DT[!is.na(Ring_Den)]


GF13_Stocking_500new[0,]
unique(GF13_Stocking_500new$Stocking)
head(GF13_Stocking_500new)

# GF13_Stocking_500 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF13_Stocking_500new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF13_Stocking_500spha")
###################################################################################
#Plot 33

# Plot 33 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF13_Stocking_500new, Plot==33), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 33")

# Plot 33 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg33 <- ddply(.data=subset(GF13_Stocking_500new, Plot==33), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg33)
avg33sd <- ddply(.data=subset(GF13_Stocking_500new, Plot==33), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg33upper <- avg33$RingDen + avg33sd 
avg33lower <- avg33$RingDen - avg33sd 
head(avg33sd)



# Plot 33 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg33, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 650)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,3333,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg33lower, ymax=avg33upper), avg33, alpha=0.33, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.33) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 33 ring mean density")


#############################################################################
#Plot 36
# Plot 36 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF13_Stocking_500new, Plot==36), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 36")

# Plot 36 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg36 <- ddply(.data=subset(GF13_Stocking_500new, Plot==36), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg36)
avg36sd <- ddply(.data=subset(GF13_Stocking_500new, Plot==36), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg36upper <- avg36$RingDen + avg36sd 
avg36lower <- avg36$RingDen - avg36sd 
head(avg36sd)



# Plot 36 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg36, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 650)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg36lower, ymax=avg36upper), avg36, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 36 ring mean density")




#############################################################################

#############################################################################




# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg3336 <- ddply(.data=subset(GF13_Stocking_500new, Plot==33|36), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg11154115$Plot<-as.character(avg3336$Plot)
avg3336$upper <- avg3336$RingDen + avg3336$SD 
avg3336$lower <- avg3336$RingDen - avg3336$SD 
head(avg3336)

#2 lines below needed to prevent legend changing to a colour bar when 4 plots see Woodhill GF13200spha file
avtemp<-avg3336
avtemp$Plot<-as.character(avtemp$Plot)
#######
ggplot(avtemp, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="left")+
  theme_bw() + ylim(300,600) +
  labs(title="GF13_Stocking_500")



#### average by RingDen by year for all GF13_500 spha  
avgGF13_500 <- ddply(.data=avg3336, .(Year), .fun=summarise, RingDen = mean(RingDen))




#### Plot average RingDen by year for all GF13_500 spha
print(ggplot(avgGF13_500, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,600) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF13_Stocking_500"))







