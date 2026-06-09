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
#Plot 12

# Plot 12 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF7_Stocking_200new, Plot==12), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 12")

# Plot 12 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg12 <- ddply(.data=subset(GF7_Stocking_200new, Plot==12), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg12)
avg12sd <- ddply(.data=subset(GF7_Stocking_200new, Plot==12), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg12upper <- avg12$RingDen + avg12sd 
avg12lower <- avg12$RingDen - avg12sd 
head(avg12sd)



# Plot 12 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg12, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 600)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg12lower, ymax=avg12upper), avg12, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 12 ring mean density")


#############################################################################
#Plot 14
# Plot 14 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF7_Stocking_200new, Plot==14), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 14")

# Plot 14 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg14 <- ddply(.data=subset(GF7_Stocking_200new, Plot==14), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg14)
avg14sd <- ddply(.data=subset(GF7_Stocking_200new, Plot==14), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg14upper <- avg14$RingDen + avg14sd 
avg14lower <- avg14$RingDen - avg14sd 
head(avg14sd)



# Plot 14 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg14, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 600)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg14lower, ymax=avg14upper), avg14, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 14 ring mean density")




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
ggplot(avg45, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 600)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg45lower, ymax=avg45upper), avg45, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 45 ring mean density")




#############################################################################
#Plot 48
# Plot 48 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF7_Stocking_200new, Plot==48), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 48")



# Plot 48 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg48 <- ddply(.data=subset(GF7_Stocking_200new, Plot==48), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg48)
avg48sd <- ddply(.data=subset(GF7_Stocking_200new, Plot==48), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg48upper <- avg48$RingDen + avg48sd 
avg48lower <- avg48$RingDen - avg48sd 
head(avg48sd)



# Plot 48 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg48, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 600)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg48lower, ymax=avg48upper), avg48, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 48 ring mean density")




#############################################################################




# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg12144548 <- ddply(.data=subset(GF7_Stocking_200new, Plot==12|14|45|48), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg11154115$Plot<-as.character(avg12144548$Plot)
avg12144548$upper <- avg12144548$RingDen + avg12144548$SD 
avg12144548$lower <- avg12144548$RingDen - avg12144548$SD 
head(avg12144548)

#2 lines below needed to prevent legend changing to a colour bar 
avtemp<-avg12144548
avtemp$Plot<-as.character(avtemp$Plot)
#
ggplot(avtemp, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="left")+
  theme_bw() + ylim(300,600) +
  labs(title="GF7_Stocking_200")



#### average by RingDen by year for all GF7_200 spha  
avgGF7_200 <- ddply(.data=avg12144548, .(Year), .fun=summarise, RingDen = mean(RingDen))




#### Plot average RingDen by year for all GF7_200 spha
print(ggplot(avgGF7_200, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,550) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF7_Stocking_200"))







