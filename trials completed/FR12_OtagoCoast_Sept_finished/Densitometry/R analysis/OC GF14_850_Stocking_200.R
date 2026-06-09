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
#GF14_850

# From rawdata select all GF14_850 
GF14_850 <- rawdata[rawdata$Breed=="GF14_850",]
head(GF14_850)
str(GF14_850)
nrow(GF14_850)
tail(GF14_850)

# From all GF14_850 data select all 200spha
GF14_850_Stocking_200 <- GF14_850[GF14_850$Stocking=="200",]

GF14_850_Stocking_200[0,]
unique(GF14_850_Stocking_200$Stocking)
head(GF14_850_Stocking_200)

# GF14_850_Stocking_200 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF14_850_Stocking_200, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF14_850_Stocking_200spha")
###################################################################################
#Plot 10

# Plot 10 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF14_850_Stocking_200, Plot==10), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 10")

# Plot 10 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg10 <- ddply(.data=subset(GF14_850_Stocking_200, Plot==10), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg10)
avg10sd <- ddply(.data=subset(GF14_850_Stocking_200, Plot==10), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg10upper <- avg10$RingDen + avg10sd 
avg10lower <- avg10$RingDen - avg10sd 
head(avg10sd)



####Thales alternative way 
###new_data <- subset(GF14_850_Stocking_200, Plot==10)
###avg10sd <- ddply(.data=new_data, .(Year), .fun=summarise, SD = sd(Ring_Den))
###avg10upper <- avg10$RingDen + avg10sd$SD 
###avg10lower <- avg10$RingDen - avg10sd$SD  

## Adds 3 columns to "avg10" 
#avg10$sd <- ddply(.data=subset(GF14_850_Stocking_200, Plot==10), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]
#avg10$upper <- avg10$RingDen + avg10$sd
#avg10$lower <- avg10$RingDen - avg10$sd  
#(avg10)



# Plot 10 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg10, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg10lower, ymax=avg10upper), avg10, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 10 ring mean density")


#############################################################################
#Plot 30
# Plot 30 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF14_850_Stocking_200, Plot==30), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 30")


# Plot 30 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg30 <- ddply(.data=subset(GF14_850_Stocking_200, Plot==30), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
avg30sd <- ddply(.data=subset(GF14_850_Stocking_200, Plot==30), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]
avg30upper <- avg30$RingDen + avg30sd 
avg30lower <- avg30$RingDen - avg30sd 
head(avg30sd)
(avg30sd)
# Plot 30 - Provides graph of average ring mean density for plot with upper and lower confidence limits
ggplot(avg30, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg30lower, ymax=avg30upper), avg30, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 30 ring mean density")


#############
#############################################################################
#Plot 36
# Plot 36 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF14_850_Stocking_200, Plot==36), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 36")

# Plot 36 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg36 <- ddply(.data=subset(GF14_850_Stocking_200, Plot==36), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
avg36sd <- ddply(.data=subset(GF14_850_Stocking_200, Plot==36), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]
avg36upper <- avg36$RingDen + avg36sd 
avg36lower <- avg36$RingDen - avg36sd 
head(avg36sd)

# Plot 36 - Provides graph of average ring mean density for plot with upper and lower confidence limits
ggplot(avg36, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg36lower, ymax=avg36upper), avg36, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 36 ring mean density")

##########################################################################

# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg103036 <- ddply(.data=subset(GF14_850_Stocking_200, Plot==10|30|36), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg103036$Plot<-as.character(avg103036$Plot)
avg103036$upper <- avg103036$RingDen + avg103036$SD 
avg103036$lower <- avg103036$RingDen - avg103036$SD 
head(avg103036)

ggplot(avg103036, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="right")+
  theme_bw() + ylim(300,500) +
  labs(title="GF14_850_Stocking_200")

#### average by RingDen by year for all GF14_850_200 spha  
avgGF14_200 <- ddply(.data=avg103036, .(Year), .fun=summarise, RingDen = mean(RingDen))

#### Plot average RingDen by year for all GF14_850_200 spha
ggplot(avgGF14_200, aes(x=Year, y=RingDen,)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black') +
  theme_bw() + ylim(300,500) + 
  labs(title="GF14_850_Stocking_200")
#

ggplot(avg36, aes(x=Year, y=RingDen, colour=Plot)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = )) +
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg36lower, ymax=avg36upper), avg36, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 36 ring mean density")

avg103036dt <- data.table(avg103036)

class(avg103036dt)
avg103036dt[,mean(RingDen)]

