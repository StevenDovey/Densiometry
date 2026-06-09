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
#GF7

# From rawdata select all GF7
GF7 <- rawdata[rawdata$Breed=="GF7_climbing_select",]
head(GF7)
str(GF7)
nrow(GF7)
tail(GF7)

# From all GF7 data select all 200spha and convert from data frame to data table and exclude NA values for Ring_Den
GF7_Stocking_200 <- GF7[GF7$Stocking=="200",]
GF7_Stocking_200DT <- as.data.table(GF7_Stocking_200)
GF7_Stocking_200new <- GF7_Stocking_200DT[!is.na(Ring_Den)]


GF7_Stocking_200new[0,]
unique(GF7_Stocking_200new$Stocking)
head(GF7_Stocking_200new)

# GF7_Stocking_200 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF7_Stocking_200new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF7_Stocking_200spha")
###################################################################################
#Plot 11

# Plot 11 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF7_Stocking_200new, Plot==11), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 11")

# Plot 11 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg11 <- ddply(.data=subset(GF7_Stocking_200new, Plot==11), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg11)
avg11sd <- ddply(.data=subset(GF7_Stocking_200new, Plot==11), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg11upper <- avg11$RingDen + avg11sd 
avg11lower <- avg11$RingDen - avg11sd 
head(avg11sd)



# Plot 11 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg11, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg11lower, ymax=avg11upper), avg11, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 11 ring mean density")


#############################################################################
#Plot 15
# Plot 15 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF7_Stocking_200new, Plot==15), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 15")

# Plot 15 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg15 <- ddply(.data=subset(GF7_Stocking_200new, Plot==15), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg15)
avg15sd <- ddply(.data=subset(GF7_Stocking_200new, Plot==15), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg15upper <- avg15$RingDen + avg15sd 
avg15lower <- avg15$RingDen - avg15sd 
head(avg15sd)



# Plot 15 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg15, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg15lower, ymax=avg15upper), avg15, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 15 ring mean density")




#############################################################################
#Plot 41
# Plot 41 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF7_Stocking_200new, Plot==41), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 41")

# Plot 41 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg41 <- ddply(.data=subset(GF7_Stocking_200new, Plot==41), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg41)
avg41sd <- ddply(.data=subset(GF7_Stocking_200new, Plot==41), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg41upper <- avg41$RingDen + avg41sd 
avg41lower <- avg41$RingDen - avg41sd 
head(avg41sd)



# Plot 41 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg41, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg15lower, ymax=avg15upper), avg15, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 41 ring mean density")




#############################################################################
#Plot 45
# Plot 45 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF7_Stocking_200new, Plot==45), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 45")



# Plot 45 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg45 <- ddply(.data=subset(GF7_Stocking_200new, Plot==45), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg45)
avg45sd <- ddply(.data=subset(GF7_Stocking_200new, Plot==45), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg45upper <- avg45$RingDen + avg45sd 
avg45lower <- avg45$RingDen - avg45sd 
head(avg45sd)



# Plot 45 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg45, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg15lower, ymax=avg15upper), avg15, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 45 ring mean density")




#############################################################################




# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg11154145 <- ddply(.data=subset(GF7_Stocking_200new, Plot==11|15|41|45), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg11154115$Plot<-as.character(avg11154145$Plot)
avg11154145$upper <- avg11154145$RingDen + avg11154145$SD 
avg11154145$lower <- avg11154145$RingDen - avg11154145$SD 
head(avg11154145)

ggplot(avg11154145, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="right")+
  theme_bw() + ylim(300,500) +
  labs(title="GF7_Stocking_200")

#### average by RingDen by year for all GF7_200 spha  
avgGF7_200 <- ddply(.data=avg11154145, .(Year), .fun=summarise, RingDen = mean(RingDen))




#### Plot average RingDen by year for all GF7_200 spha
print(ggplot(avgGF7_200, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,510) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF7_Stocking_200"))







