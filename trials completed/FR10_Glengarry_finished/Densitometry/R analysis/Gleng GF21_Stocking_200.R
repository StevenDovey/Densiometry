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

# From all GF21 data select all 200spha and convert from data frame to data table and exclude NA values for Ring_Den
GF21_Stocking_200 <- GF21[GF21$Stocking=="200",]
GF21_Stocking_200DT <- as.data.table(GF21_Stocking_200)
GF21_Stocking_200new <- GF21_Stocking_200DT[!is.na(Ring_Den)]


GF21_Stocking_200new[0,]
unique(GF21_Stocking_200new$Stocking)
head(GF21_Stocking_200new)

# GF21_Stocking_200 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF21_Stocking_200new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF21_Stocking_200spha")
###################################################################################
#Plot 10

# Plot 10 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF21_Stocking_200new, Plot==10), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 10")

# Plot 10 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg10 <- ddply(.data=subset(GF21_Stocking_200new, Plot==10), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg10)
avg10sd <- ddply(.data=subset(GF21_Stocking_200new, Plot==10), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg10upper <- avg10$RingDen + avg10sd 
avg10lower <- avg10$RingDen - avg10sd 
head(avg10sd)



# Plot 10 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg10, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg10lower, ymax=avg10upper), avg10, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 10 ring mean density")


#############################################################################
#Plot 13
# Plot 13 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF21_Stocking_200new, Plot==13), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 13")

# Plot 13 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg13 <- ddply(.data=subset(GF21_Stocking_200new, Plot==13), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg13)
avg13sd <- ddply(.data=subset(GF21_Stocking_200new, Plot==13), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg13upper <- avg13$RingDen + avg13sd 
avg13lower <- avg13$RingDen - avg13sd 
head(avg13sd)



# Plot 13 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg13, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg13lower, ymax=avg13upper), avg13, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 13 ring mean density")




#############################################################################
#Plot 44
# Plot 44 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF21_Stocking_200new, Plot==44), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 44")

# Plot 44 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg44 <- ddply(.data=subset(GF21_Stocking_200new, Plot==44), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg44)
avg44sd <- ddply(.data=subset(GF21_Stocking_200new, Plot==44), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg44upper <- avg44$RingDen + avg44sd 
avg44lower <- avg44$RingDen - avg44sd 
head(avg44sd)



# Plot 44 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg44, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg44lower, ymax=avg44upper), avg44, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 44 ring mean density")




#############################################################################
#Plot 48
# Plot 48 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF21_Stocking_200new, Plot==48), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 48")



# Plot 48 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg48 <- ddply(.data=subset(GF21_Stocking_200new, Plot==48), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg48)
avg48sd <- ddply(.data=subset(GF21_Stocking_200new, Plot==48), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg48upper <- avg48$RingDen + avg48sd 
avg48lower <- avg48$RingDen - avg48sd 
head(avg48sd)



# Plot 48 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg48, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg48lower, ymax=avg48upper), avg48, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 48 ring mean density")




#############################################################################




# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg10134448 <- ddply(.data=subset(GF21_Stocking_200new, Plot==10|13|44|48), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg11154115$Plot<-as.character(avg10134448$Plot)
avg10134448$upper <- avg10134448$RingDen + avg10134448$SD 
avg10134448$lower <- avg10134448$RingDen - avg10134448$SD 
head(avg10134448)

ggplot(avg10134448, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="right")+
  theme_bw() + ylim(300,500) +
  labs(title="GF21_Stocking_200")

#### average by RingDen by year for all GF21_200 spha  
avgGF21_200 <- ddply(.data=avg10134448, .(Year), .fun=summarise, RingDen = mean(RingDen))




#### Plot average RingDen by year for all GF21_200 spha
print(ggplot(avgGF21_200, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,510) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF21_Stocking_200"))







