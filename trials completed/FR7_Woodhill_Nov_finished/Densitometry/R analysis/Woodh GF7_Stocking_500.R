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

# From all GF7 data select all 500spha and convert from data frame to data table and exclude NA values for Ring_Den
GF7_Stocking_500 <- GF7[GF7$Stocking=="500",]
GF7_Stocking_500DT <- as.data.table(GF7_Stocking_500)
GF7_Stocking_500new <- GF7_Stocking_500DT[!is.na(Ring_Den)]


GF7_Stocking_500new[0,]
unique(GF7_Stocking_500new$Stocking)
head(GF7_Stocking_500new)

# GF7_Stocking_500 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF7_Stocking_500new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF7_Stocking_500spha")
###################################################################################
#Plot 37

# Plot 37 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF7_Stocking_500new, Plot==37), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 37")

# Plot 37 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg37 <- ddply(.data=subset(GF7_Stocking_500new, Plot==37), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg37)
avg37sd <- ddply(.data=subset(GF7_Stocking_500new, Plot==37), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg37upper <- avg37$RingDen + avg37sd 
avg37lower <- avg37$RingDen - avg37sd 
head(avg37sd)



# Plot 37 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg37, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 650)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg37lower, ymax=avg37upper), avg37, alpha=0.37, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.37) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 37 ring mean density")


#############################################################################
#Plot 38
# Plot 38 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF7_Stocking_500new, Plot==38), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 38")

# Plot 38 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg38 <- ddply(.data=subset(GF7_Stocking_500new, Plot==38), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg38)
avg38sd <- ddply(.data=subset(GF7_Stocking_500new, Plot==38), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg38upper <- avg38$RingDen + avg38sd 
avg38lower <- avg38$RingDen - avg38sd 
head(avg38sd)



# Plot 38 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg38, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 650)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg38lower, ymax=avg38upper), avg38, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 38 ring mean density")




#############################################################################

#############################################################################




# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg3738 <- ddply(.data=subset(GF7_Stocking_500new, Plot==37|38), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg11154115$Plot<-as.character(avg3738$Plot)
avg3738$upper <- avg3738$RingDen + avg3738$SD 
avg3738$lower <- avg3738$RingDen - avg3738$SD 
head(avg3738)

#2 lines below needed to prevent legend changing to a colour bar when 4 plots see Woodhill GF7200spha file
avtemp<-avg3738
avtemp$Plot<-as.character(avtemp$Plot)
#######
ggplot(avtemp, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="left")+
  theme_bw() + ylim(300,600) +
  labs(title="GF7_Stocking_500")



#### average by RingDen by year for all GF7_500 spha  
avgGF7_500 <- ddply(.data=avg3738, .(Year), .fun=summarise, RingDen = mean(RingDen))




#### Plot average RingDen by year for all GF7_500 spha
print(ggplot(avgGF7_500, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,600) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF7_Stocking_500"))







