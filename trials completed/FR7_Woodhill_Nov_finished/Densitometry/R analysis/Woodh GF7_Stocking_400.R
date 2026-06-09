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

# From all GF7 data select all 400spha and convert from data frame to data table and exclude NA values for Ring_Den
GF7_Stocking_400 <- GF7[GF7$Stocking=="400",]
GF7_Stocking_400DT <- as.data.table(GF7_Stocking_400)
GF7_Stocking_400new <- GF7_Stocking_400DT[!is.na(Ring_Den)]


GF7_Stocking_400new[0,]
unique(GF7_Stocking_400new$Stocking)
head(GF7_Stocking_400new)

# GF7_Stocking_400 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF7_Stocking_400new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF7_Stocking_400spha")
###################################################################################
#Plot 19

# Plot 19 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF7_Stocking_400new, Plot==19), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 19")

# Plot 19 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg19 <- ddply(.data=subset(GF7_Stocking_400new, Plot==19), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg19)
avg19sd <- ddply(.data=subset(GF7_Stocking_400new, Plot==19), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg19upper <- avg19$RingDen + avg19sd 
avg19lower <- avg19$RingDen - avg19sd 
head(avg19sd)



# Plot 19 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg19, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 650)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg19lower, ymax=avg19upper), avg19, alpha=0.19, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.19) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 19 ring mean density")


#############################################################################
#Plot 21
# Plot 21 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF7_Stocking_400new, Plot==21), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 21")

# Plot 21 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg21 <- ddply(.data=subset(GF7_Stocking_400new, Plot==21), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg21)
avg21sd <- ddply(.data=subset(GF7_Stocking_400new, Plot==21), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg21upper <- avg21$RingDen + avg21sd 
avg21lower <- avg21$RingDen - avg21sd 
head(avg21sd)



# Plot 21 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg21, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 650)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg21lower, ymax=avg21upper), avg21, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 21 ring mean density")




#############################################################################

#############################################################################




# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg1921 <- ddply(.data=subset(GF7_Stocking_400new, Plot==19|21), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg11154115$Plot<-as.character(avg1921$Plot)
avg1921$upper <- avg1921$RingDen + avg1921$SD 
avg1921$lower <- avg1921$RingDen - avg1921$SD 
head(avg1921)

#2 lines below needed to prevent legend changing to a colour bar when 4 plots see Woodhill GF7200spha file
avtemp<-avg1921
avtemp$Plot<-as.character(avtemp$Plot)
#######
ggplot(avtemp, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="left")+
  theme_bw() + ylim(300,600) +
  labs(title="GF7_Stocking_400")



#### average by RingDen by year for all GF7_400 spha  
avgGF7_400 <- ddply(.data=avg1921, .(Year), .fun=summarise, RingDen = mean(RingDen))




#### Plot average RingDen by year for all GF7_400 spha
print(ggplot(avgGF7_400, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,600) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF7_Stocking_400"))







