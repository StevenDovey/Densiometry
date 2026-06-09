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

# From all GF7 data select all 600spha and convert from data frame to data table and exclude NA values for Ring_Den
GF7_Stocking_600 <- GF7[GF7$Stocking=="600",]
GF7_Stocking_600DT <- as.data.table(GF7_Stocking_600)
GF7_Stocking_600new <- GF7_Stocking_600DT[!is.na(Total_Area)]


GF7_Stocking_600new[0,]
unique(GF7_Stocking_600new$Stocking)
head(GF7_Stocking_600new)
################



avgGF7_500All <- ddply(.data=GF7_Stocking_500new, .(Year), .fun=summarise, Total_Area = mean(Total_Area))

avgGF7_600All <- ddply(.data=GF7_Stocking_600new, .(Year), .fun=summarise, Total_Area = mean(Total_Area))


#############



# GF7_Stocking_600 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF7_Stocking_600new, aes(x=Year, y=Total_Area, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Basal area", title = "GF7_Stocking_600spha")
###################################################################################
#Plot 25

# Plot 25 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF7_Stocking_600new, Plot==25), aes(x=Year, y=Total_Area, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Basal area", title = "Plot 25")

# Plot 25 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg25 <- ddply(.data=subset(GF7_Stocking_600new, Plot==25), .(Year), .fun=summarise, RingBA = mean(Total_Area))
(avg25)
avg25sd <- ddply(.data=subset(GF7_Stocking_600new, Plot==25), .(Year), .fun=summarise, SD = sd(Total_Area))[,2]# [,2] makes it a vec
avg25upper <- avg25$RingBA + avg25sd 
avg25lower <- avg25$RingBA - avg25sd 
head(avg25sd)



# Plot 25 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg25, aes(x=Year, y=RingBA)) + coord_cartesian(ylim = c(0, 1500)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg25lower, ymax=avg25upper), avg25, alpha=0.25, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.25) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Basal area", title = "Plot 25 basal area")


#############################################################################
#Plot 31

# Plot 31 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF7_Stocking_600new, Plot==31), aes(x=Year, y=Total_Area, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Basal area", title = "Plot 31")

# Plot 31 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg31 <- ddply(.data=subset(GF7_Stocking_600new, Plot==31), .(Year), .fun=summarise, RingBA = mean(Total_Area))
(avg31)
avg31sd <- ddply(.data=subset(GF7_Stocking_600new, Plot==31), .(Year), .fun=summarise, SD = sd(Total_Area))[,2]# [,2] makes it a vec
avg31upper <- avg31$RingBA + avg31sd 
avg31lower <- avg31$RingBA - avg31sd 
head(avg31sd)



# Plot 31 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg31, aes(x=Year, y=RingBA)) + coord_cartesian(ylim = c(0, 1500)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg31lower, ymax=avg31upper), avg31, alpha=0.31, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.31) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Basal area", title = "Plot 31 basal area")







#############################################################################

#############################################################################




# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg2531 <- ddply(.data=subset(GF7_Stocking_600new, Plot==25|31), .(Plot,Year), .fun=summarise, RingBA = mean(Total_Area),SD=sd(Ring_Den))
avg11154115$Plot<-as.character(avg2531$Plot)
avg2531$upper <- avg2531$RingBA + avg2531$SD 
avg2531$lower <- avg2531$RingBA - avg2531$SD 
head(avg2531)

#2 lines below needed to prevent legend changing to a colour bar when 4 plots see Woodhill GF7200spha file
avtemp<-avg2531
avtemp$Plot<-as.character(avtemp$Plot)
#######
ggplot(avtemp, aes(x=Year, y=RingBA, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="left")+
  theme_bw() + ylim(0,1000) +
  labs(title="GF7_Stocking_600")



#### average by RingDen by year for all GF7_600 spha  
avgGF7_600 <- ddply(.data=avg2531, .(Year), .fun=summarise, RingBA = mean(RingBA))




#### Plot average RingDen by year for all GF7_600 spha
print(ggplot(avgGF7_600, aes(x=Year, y=RingBA,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(0,1000) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF7_Stocking_600"))







