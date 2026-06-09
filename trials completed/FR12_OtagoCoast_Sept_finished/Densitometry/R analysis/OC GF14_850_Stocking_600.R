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

# From all GF14_850 data select all 600spha
GF14_850_Stocking_600 <- GF14_850[GF14_850$Stocking=="600",]

GF14_850_Stocking_600[0,]
unique(GF14_850_Stocking_600$Stocking)
head(GF14_850_Stocking_600)

# GF14_850_Stocking_600 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF14_850_Stocking_600, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF14_850_Stocking_600spha")
###################################################################################
#Plot 5

# Plot 5 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF14_850_Stocking_600, Plot==5), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 5")

# Plot 5 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg5 <- ddply(.data=subset(GF14_850_Stocking_600, Plot==5), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg5)
avg5sd <- ddply(.data=subset(GF14_850_Stocking_600, Plot==5), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg5upper <- avg5$RingDen + avg5sd 
avg5lower <- avg5$RingDen - avg5sd 
head(avg5sd)



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



# Plot 5 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg5, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg5lower, ymax=avg5upper), avg5, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 5 ring mean density")


#############################################################################
#Plot 42
# Plot 42 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF14_850_Stocking_600, Plot==42), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 42")


# Plot 42 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg42 <- ddply(.data=subset(GF14_850_Stocking_600, Plot==42), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
avg42sd <- ddply(.data=subset(GF14_850_Stocking_600, Plot==42), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]
avg42upper <- avg42$RingDen + avg42sd 
avg42lower <- avg42$RingDen - avg42sd 
head(avg42sd)
(avg42sd)
# Plot 42 - Provides graph of average ring mean density for plot with upper and lower confidence limits
ggplot(avg42, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg42lower, ymax=avg42upper), avg42, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 42 ring mean density")



#############################################################################

# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg542 <- ddply(.data=subset(GF14_850_Stocking_600, Plot==5|42), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg542$Plot<-as.character(avg542$Plot)
avg542$upper <- avg542$RingDen + avg542$SD 
avg542$lower <- avg542$RingDen - avg542$SD 
head(avg542)

ggplot(avg542, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="right")+
  theme_bw() + ylim(300,500) +
  labs(title="GF14_850_Stocking_600")

#### average by RingDen by year for all GF14_850_600 spha  
avgGF14_600 <- ddply(.data=avg542, .(Year), .fun=summarise, RingDen = mean(RingDen))

#### Plot average RingDen by year for all GF14_850_600 spha
print(ggplot(avgGF14_600, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,500) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF14_850_Stocking_600"))





