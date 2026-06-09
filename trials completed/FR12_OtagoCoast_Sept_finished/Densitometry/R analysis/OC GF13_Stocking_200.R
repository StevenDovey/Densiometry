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
#GF13

# From rawdata select all GF13
GF13 <- rawdata[rawdata$Breed=="GF13_LI28_870_long_internode",]
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
#Plot 26

# Plot 26 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF13_Stocking_200new, Plot==26), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 26")

# Plot 26 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg26 <- ddply(.data=subset(GF13_Stocking_200new, Plot==26), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg26)
avg26sd <- ddply(.data=subset(GF13_Stocking_200new, Plot==26), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg26upper <- avg26$RingDen + avg26sd 
avg26lower <- avg26$RingDen - avg26sd 
head(avg26sd)



# Plot 26 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg26, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg26lower, ymax=avg26upper), avg26, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 26 ring mean density")


#############################################################################
#Plot 40
# Plot 40 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF13_Stocking_200new, Plot==40), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 40")

# Plot 40 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg40 <- ddply(.data=subset(GF13_Stocking_200new, Plot==40), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg40)
avg40sd <- ddply(.data=subset(GF13_Stocking_200new, Plot==40), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg40upper <- avg40$RingDen + avg40sd 
avg40lower <- avg40$RingDen - avg40sd 
head(avg40sd)


# Plot 40 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg40, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg40lower, ymax=avg40upper), avg40, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 40 ring mean density")



#############################################################################
#Plot 48
# Plot 48 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF13_Stocking_200new, Plot==48), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 48")

# Plot 48 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg48 <- ddply(.data=subset(GF13_Stocking_200new, Plot==48), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg48)
avg48sd <- ddply(.data=subset(GF13_Stocking_200new, Plot==48), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
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
avg264048 <- ddply(.data=subset(GF13_Stocking_200new, Plot==26|40|48), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg264048$Plot<-as.character(avg264048$Plot)
avg264048$upper <- avg264048$RingDen + avg264048$SD 
avg264048$lower <- avg264048$RingDen - avg264048$SD 
head(avg264048)

ggplot(avg264048, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="right")+
  theme_bw() + ylim(300,510) +
  labs(title="GF13_Stocking_200")

#### average by RingDen by year for all GF13_200 spha  
avgGF13_200 <- ddply(.data=avg264048, .(Year), .fun=summarise, RingDen = mean(RingDen))



#### Plot average RingDen by year for all GF13_200 spha
print(ggplot(avgGF13_200, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,510) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF13_Stocking_200"))





