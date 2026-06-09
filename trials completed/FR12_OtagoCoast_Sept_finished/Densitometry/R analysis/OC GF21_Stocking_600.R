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

# From all GF21 data select all 600spha and convert from data frame to data table and exclude NA values for Ring_Den
GF21_Stocking_600 <- GF21[GF21$Stocking=="600",]
GF21_Stocking_600DT <- as.data.table(GF21_Stocking_600)
GF21_Stocking_600new <- GF21_Stocking_600DT[!is.na(Ring_Den)]


GF21_Stocking_600new[0,]
unique(GF21_Stocking_600new$Stocking)
head(GF21_Stocking_600new)

# GF21_Stocking_600 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF21_Stocking_600new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF21_Stocking_600spha")
###################################################################################
#Plot 7

# Plot 7 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF21_Stocking_600new, Plot==7), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 7")

# Plot 7 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg7 <- ddply(.data=subset(GF21_Stocking_600new, Plot==7), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg7)
avg7sd <- ddply(.data=subset(GF21_Stocking_600new, Plot==7), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg7upper <- avg7$RingDen + avg7sd 
avg7lower <- avg7$RingDen - avg7sd 
head(avg7sd)



# Plot 7 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg7, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg7lower, ymax=avg7upper), avg7, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 7 ring mean density")


#############################################################################
#Plot 29
# Plot 29 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF21_Stocking_600new, Plot==29), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 29")

# Plot 29 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg29 <- ddply(.data=subset(GF21_Stocking_600new, Plot==29), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg29)
avg29sd <- ddply(.data=subset(GF21_Stocking_600new, Plot==29), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg29upper <- avg29$RingDen + avg29sd 
avg29lower <- avg29$RingDen - avg29sd 
head(avg29sd)


# Plot 29 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg29, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg29lower, ymax=avg29upper), avg29, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 29 ring mean density")



#############################################################################

# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg729 <- ddply(.data=subset(GF21_Stocking_600new, Plot==7|29), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg729$Plot<-as.character(avg729$Plot)
avg729$upper <- avg729$RingDen + avg729$SD 
avg729$lower <- avg729$RingDen - avg729$SD 
head(avg729)

ggplot(avg729, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="right")+
  theme_bw() + ylim(300,510) +
  labs(title="GF21_Stocking_600")

#### average by RingDen by year for all GF21_600 spha  
avgGF21_600 <- ddply(.data=avg729, .(Year), .fun=summarise, RingDen = mean(RingDen))



#### Plot average RingDen by year for all GF21_600 spha
print(ggplot(avgGF21_600, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,510) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF21_Stocking_600"))





