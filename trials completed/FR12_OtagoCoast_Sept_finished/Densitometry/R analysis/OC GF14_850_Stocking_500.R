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

# From all GF14_850 data select all 500spha and convert from data frame to data table and exclude NA values for Ring_Den
GF14_850_Stocking_500 <- GF14_850[GF14_850$Stocking=="500",]
GF14_850_Stocking_500DT <- as.data.table(GF14_850_Stocking_500)
GF14_850_Stocking_500new <- GF14_850_Stocking_500DT[!is.na(Ring_Den)]


GF14_850_Stocking_500new[0,]
unique(GF14_850_Stocking_500new$Stocking)
head(GF14_850_Stocking_500new)

# GF14_850_Stocking_500 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF14_850_Stocking_500new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF14_850_Stocking_500spha")
###################################################################################
#Plot 1

# Plot 1 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF14_850_Stocking_500new, Plot==1), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 1")

# Plot 1 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg1 <- ddply(.data=subset(GF14_850_Stocking_500new, Plot==1), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg1)
avg1sd <- ddply(.data=subset(GF14_850_Stocking_500new, Plot==1), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg1upper <- avg1$RingDen + avg1sd 
avg1lower <- avg1$RingDen - avg1sd 
head(avg1sd)



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



# Plot 1 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg1, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg1lower, ymax=avg1upper), avg1, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 1 ring mean density")


#############################################################################
#Plot 21
# Plot 21 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF14_850_Stocking_500new, Plot==21), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 21")


# Plot 21 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg21 <- ddply(.data=subset(GF14_850_Stocking_500new, Plot==21), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
avg21sd <- ddply(.data=subset(GF14_850_Stocking_500new, Plot==21), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]
avg21upper <- avg21$RingDen + avg21sd 
avg21lower <- avg21$RingDen - avg21sd 
head(avg21sd)
(avg21sd)
# Plot 21 - Provides graph of average ring mean density for plot with upper and lower confidence limits
ggplot(avg21, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg21lower, ymax=avg21upper), avg21, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 21 ring mean density")



#############################################################################

# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg121 <- ddply(.data=subset(GF14_850_Stocking_500new, Plot==1|21), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg121$Plot<-as.character(avg121$Plot)
avg121$upper <- avg121$RingDen + avg121$SD 
avg121$lower <- avg121$RingDen - avg121$SD 
head(avg121)

ggplot(avg121, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="right")+
  theme_bw() + ylim(300,500) +
  labs(title="GF14_850_Stocking_500")

#### average by RingDen by year for all GF14_850_600 spha  
avgGF14_500 <- ddply(.data=avg121, .(Year), .fun=summarise, RingDen = mean(RingDen))

#### Plot average RingDen by year for all GF14_850_500 spha
print(ggplot(avgGF14_500, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,500) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF14_850_Stocking_500"))





