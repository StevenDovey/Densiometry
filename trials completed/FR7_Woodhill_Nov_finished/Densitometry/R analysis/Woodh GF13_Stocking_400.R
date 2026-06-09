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

# From all GF13 data select all 400spha and convert from data frame to data table and exclude NA values for Ring_Den
GF13_Stocking_400 <- GF13[GF13$Stocking=="400",]
GF13_Stocking_400DT <- as.data.table(GF13_Stocking_400)
GF13_Stocking_400new <- GF13_Stocking_400DT[!is.na(Ring_Den)]


GF13_Stocking_400new[0,]
unique(GF13_Stocking_400new$Stocking)
head(GF13_Stocking_400new)

# GF13_Stocking_400 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF13_Stocking_400new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF13_Stocking_400spha")
###################################################################################
#Plot 20

# Plot 20 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF13_Stocking_400new, Plot==20), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 20")

# Plot 20 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg20 <- ddply(.data=subset(GF13_Stocking_400new, Plot==20), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg20)
avg20sd <- ddply(.data=subset(GF13_Stocking_400new, Plot==20), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg20upper <- avg20$RingDen + avg20sd 
avg20lower <- avg20$RingDen - avg20sd 
head(avg20sd)



# Plot 20 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg20, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 650)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg20lower, ymax=avg20upper), avg20, alpha=0.20, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.20) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 20 ring mean density")


#############################################################################
#Plot 24
# Plot 24 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF13_Stocking_400new, Plot==24), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 24")

# Plot 24 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg24 <- ddply(.data=subset(GF13_Stocking_400new, Plot==24), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg24)
avg24sd <- ddply(.data=subset(GF13_Stocking_400new, Plot==24), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg24upper <- avg24$RingDen + avg24sd 
avg24lower <- avg24$RingDen - avg24sd 
head(avg24sd)



# Plot 24 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg24, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 650)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg24lower, ymax=avg24upper), avg24, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 24 ring mean density")




#############################################################################

#############################################################################




# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg2024 <- ddply(.data=subset(GF13_Stocking_400new, Plot==20|24), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg11154115$Plot<-as.character(avg2024$Plot)
avg2024$upper <- avg2024$RingDen + avg2024$SD 
avg2024$lower <- avg2024$RingDen - avg2024$SD 
head(avg2024)

#2 lines below needed to prevent legend changing to a colour bar when 4 plots see Woodhill GF13200spha file
avtemp<-avg2024
avtemp$Plot<-as.character(avtemp$Plot)
#######
ggplot(avtemp, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="left")+
  theme_bw() + ylim(300,600) +
  labs(title="GF13_Stocking_400")



#### average by RingDen by year for all GF13_400 spha  
avgGF13_400 <- ddply(.data=avg2024, .(Year), .fun=summarise, RingDen = mean(RingDen))




#### Plot average RingDen by year for all GF13_400 spha
print(ggplot(avgGF13_400, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,600) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF13_Stocking_400"))







