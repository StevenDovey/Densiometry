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
#GF21

# From rawdata select all GF21
GF21 <- rawdata[rawdata$Breed=="GF21_268",]
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
#Plot 35

# Plot 35 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF21_Stocking_200new, Plot==35), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 35")

# Plot 35 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg35 <- ddply(.data=subset(GF21_Stocking_200new, Plot==35), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg35)
avg35sd <- ddply(.data=subset(GF21_Stocking_200new, Plot==35), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg35upper <- avg35$RingDen + avg35sd 
avg35lower <- avg35$RingDen - avg35sd 
head(avg35sd)



# Plot 35 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg35, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg35lower, ymax=avg35upper), avg35, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 35 ring mean density")


#############################################################################
#Plot 43
# Plot 43 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF21_Stocking_200new, Plot==43), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 43")

# Plot 43 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg43 <- ddply(.data=subset(GF21_Stocking_200new, Plot==43), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg43)
avg43sd <- ddply(.data=subset(GF21_Stocking_200new, Plot==43), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg43upper <- avg43$RingDen + avg43sd 
avg43lower <- avg43$RingDen - avg43sd 
head(avg43sd)


# Plot 43 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg43, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg43lower, ymax=avg43upper), avg43, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 43 ring mean density")



#############################################################################

# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg3543 <- ddply(.data=subset(GF21_Stocking_200new, Plot==35|43), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg3543$Plot<-as.character(avg3543$Plot)
avg3543$upper <- avg3543$RingDen + avg3543$SD 
avg3543$lower <- avg3543$RingDen - avg3543$SD 
head(avg3543)

ggplot(avg3543, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="right")+
  theme_bw() + ylim(300,510) +
  labs(title="GF21_Stocking_200")

#### average by RingDen by year for all GF21_200 spha  
avgGF21_200 <- ddply(.data=avg3543, .(Year), .fun=summarise, RingDen = mean(RingDen))



#### Plot average RingDen by year for all GF21_200 spha
print(ggplot(avgGF21_200, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,510) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF21_Stocking_200"))





