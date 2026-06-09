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

# From all GF21 data select all 500spha and convert from data frame to data table and exclude NA values for Ring_Den
GF21_Stocking_500 <- GF21[GF21$Stocking=="500",]
GF21_Stocking_500DT <- as.data.table(GF21_Stocking_500)
GF21_Stocking_500new <- GF21_Stocking_500DT[!is.na(Ring_Den)]


GF21_Stocking_500new[0,]
unique(GF21_Stocking_500new$Stocking)
head(GF21_Stocking_500new)

# GF21_Stocking_500 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF21_Stocking_500new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF21_Stocking_500spha")
###################################################################################
#Plot 27

# Plot 27 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF21_Stocking_500new, Plot==27), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 27")

# Plot 27 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg27 <- ddply(.data=subset(GF21_Stocking_500new, Plot==27), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg27)
avg27sd <- ddply(.data=subset(GF21_Stocking_500new, Plot==27), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg27upper <- avg27$RingDen + avg27sd 
avg27lower <- avg27$RingDen - avg27sd 
head(avg27sd)



# Plot 27 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg27, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg27lower, ymax=avg27upper), avg27, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 27 ring mean density")


#############################################################################
#Plot 39
# Plot 39 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF21_Stocking_500new, Plot==39), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 39")

# Plot 39 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg39 <- ddply(.data=subset(GF21_Stocking_500new, Plot==39), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg39)
avg39sd <- ddply(.data=subset(GF21_Stocking_500new, Plot==39), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg39upper <- avg39$RingDen + avg39sd 
avg39lower <- avg39$RingDen - avg39sd 
head(avg39sd)


# Plot 39 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg39, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg39lower, ymax=avg39upper), avg39, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 39 ring mean density")



#############################################################################

# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg2739 <- ddply(.data=subset(GF21_Stocking_500new, Plot==27|39), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg2739$Plot<-as.character(avg2739$Plot)
avg2739$upper <- avg2739$RingDen + avg2739$SD 
avg2739$lower <- avg2739$RingDen - avg2739$SD 
head(avg2739)

ggplot(avg2739, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="right")+
  theme_bw() + ylim(300,510) +
  labs(title="GF21_Stocking_500")

#### average by RingDen by year for all GF21_500 spha  
avgGF21_500 <- ddply(.data=avg2739, .(Year), .fun=summarise, RingDen = mean(RingDen))



#### Plot average RingDen by year for all GF21_500 spha
print(ggplot(avgGF21_500, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,510) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF21_Stocking_500"))





