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

# From all GF13 data select all 600spha and convert from data frame to data table and exclude NA values for Ring_Den
GF13_Stocking_600 <- GF13[GF13$Stocking=="600",]
GF13_Stocking_600DT <- as.data.table(GF13_Stocking_600)
GF13_Stocking_600new <- GF13_Stocking_600DT[!is.na(Ring_Den)]


GF13_Stocking_600new[0,]
unique(GF13_Stocking_600new$Stocking)
head(GF13_Stocking_600new)

# GF13_Stocking_600 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF13_Stocking_600new, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF13_Stocking_600spha")
###################################################################################
#Plot 25

# Plot 25 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF13_Stocking_600new, Plot==25), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 25")

# Plot 25 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg25 <- ddply(.data=subset(GF13_Stocking_600new, Plot==25), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg25)
avg25sd <- ddply(.data=subset(GF13_Stocking_600new, Plot==25), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg25upper <- avg25$RingDen + avg25sd 
avg25lower <- avg25$RingDen - avg25sd 
head(avg25sd)



# Plot 25 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg25, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg25lower, ymax=avg25upper), avg25, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 25 ring mean density")


#############################################################################
#Plot 32
# Plot 32 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF13_Stocking_600new, Plot==32), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 32")

# Plot 32 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg32 <- ddply(.data=subset(GF13_Stocking_600new, Plot==32), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg32)
avg32sd <- ddply(.data=subset(GF13_Stocking_600new, Plot==32), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg32upper <- avg32$RingDen + avg32sd 
avg32lower <- avg32$RingDen - avg32sd 
head(avg32sd)


# Plot 32 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg32, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg32lower, ymax=avg32upper), avg32, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 32 ring mean density")



#############################################################################

# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg2532 <- ddply(.data=subset(GF13_Stocking_600new, Plot==25|32), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg2532$Plot<-as.character(avg2532$Plot)
avg2532$upper <- avg2532$RingDen + avg2532$SD 
avg2532$lower <- avg2532$RingDen - avg2532$SD 
head(avg2532)

ggplot(avg2532, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="right")+
  theme_bw() + ylim(300,510) +
  labs(title="GF13_Stocking_600")

#### average by RingDen by year for all GF13_600 spha  
avgGF13_600 <- ddply(.data=avg2532, .(Year), .fun=summarise, RingDen = mean(RingDen))



#### Plot average RingDen by year for all GF13_600 spha
print(ggplot(avgGF13_600, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,510) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF13_Stocking_600"))





