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
#GF21

# From rawdata select all GF21
GF21 <- rawdata[rawdata$Breed=="GF21_268",]
head(GF21)
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
#Plot 19

# Plot 19 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF21_Stocking_400new, Plot==19), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 19")

# Plot 19 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg19 <- ddply(.data=subset(GF21_Stocking_400new, Plot==19), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg19)
avg19sd <- ddply(.data=subset(GF21_Stocking_400new, Plot==19), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg19upper <- avg19$RingDen + avg19sd 
avg19lower <- avg19$RingDen - avg19sd 
head(avg19sd)



# Plot 19 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg19, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg19lower, ymax=avg19upper), avg19, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 19 ring mean density")


#############################################################################
#Plot 22
# Plot 22 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF21_Stocking_400new, Plot==22), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 22")

# Plot 22 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg22 <- ddply(.data=subset(GF21_Stocking_400new, Plot==22), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg22)
avg22sd <- ddply(.data=subset(GF21_Stocking_400new, Plot==22), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg22upper <- avg22$RingDen + avg22sd 
avg22lower <- avg22$RingDen - avg22sd 
head(avg22sd)



# Plot 22 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg22, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg22lower, ymax=avg22upper), avg22, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 22 ring mean density")




#############################################################################

#############################################################################




# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg1922 <- ddply(.data=subset(GF21_Stocking_400new, Plot==19|22), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg1922$Plot<-as.character(avg1922$Plot)
avg1922$upper <- avg1922$RingDen + avg1922$SD 
avg1922$lower <- avg1922$RingDen - avg1922$SD 
head(avg1922)

ggplot(avg1922, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="right")+
  theme_bw() + ylim(300,510) +
  labs(title="GF21_Stocking_400")



#### average by RingDen by year for all GF21_400 spha  
avgGF21_400 <- ddply(.data=avg1922, .(Year), .fun=summarise, RingDen = mean(RingDen))




#### Plot average RingDen by year for all GF21_400 spha
print(ggplot(avgGF21_400, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,510) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF21_Stocking_400"))







