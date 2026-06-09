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
#GF7

# From rawdata select all GF7
GF7 <- rawdata[rawdata$Breed=="GF7_climbing_select",]
head(GF)
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
#Plot 47

# Plot 47 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF7_Stocking_200new, Plot==47), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 47")

# Plot 47 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg47 <- ddply(.data=subset(GF7_Stocking_200new, Plot==47), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg47)
avg47sd <- ddply(.data=subset(GF7_Stocking_200new, Plot==47), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg47upper <- avg47$RingDen + avg47sd 
avg47lower <- avg47$RingDen - avg47sd 
head(avg47sd)



# Plot 47 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg47, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg47lower, ymax=avg47upper), avg47, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 47 ring mean density")


#############################################################################
#Plot 21
# Plot 21 - Individually graphs pith to bark density for each tree for 1 plot
#ggplot(subset(GF14_850_Stocking_500new, Plot==21), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
#  geom_line() +
#  facet_wrap(. ~ Tree) +
#  theme_bw() +
#  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
#  labs(colour = "Tree", fill = "Tree") +
#  labs(x = "Year", y = "Ring mean density", title = "Plot 21")




#############################################################################

# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
#avg121 <- ddply(.data=subset(GF14_850_Stocking_500new, Plot==1|21), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
#avg121$Plot<-as.character(avg121$Plot)
#avg121$upper <- avg121$RingDen + avg121$SD 
#avg121$lower <- avg121$RingDen - avg121$SD 
#head(avg121)

#ggplot(avg121, aes(x=Year, y=RingDen, group=Plot)) + 
#  scale_x_continuous(breaks=seq(1988,2020,2)) + 
#  geom_line(aes(color=Plot), size=1)+
#  geom_point(aes(color=Plot))+
#  theme(legend.position="right")+
#  theme_bw() + ylim(300,500) +
#  labs(title="GF14_850_Stocking_500")

#### average by RingDen by year for all GF14_850_600 spha  
#avgGF14_500 <- ddply(.data=avg121, .(Year), .fun=summarise, RingDen = mean(RingDen))



#### average by RingDen by year for all GF14_850_600 spha  
#avgGF7_200 <- ddply(.data=avg121, .(Year), .fun=summarise, RingDen = mean(RingDen))

#### Plot average RingDen by year for all GF7_200 spha
print(ggplot(avg47, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,500) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF7_Stocking_200"))





