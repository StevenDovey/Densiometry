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

# From all GF14_850 data select all 400spha
GF14_850_Stocking_400 <- GF14_850[GF14_850$Stocking=="400",]

GF14_850_Stocking_400[0,]
unique(GF14_850_Stocking_400$Stocking)
head(GF14_850_Stocking_400)

# GF14_850_Stocking_400 combinations - Individual plot graphs for each tree sampled for pith to bark density 
ggplot(GF14_850_Stocking_400, aes(x=Year, y=Ring_Den, colour=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Plot) +
  labs(colour = "Tree") +
  labs(x = "Year", y = "Ring mean density ", title = "GF14_850_Stocking_400spha")
###################################################################################
#Plot 8

# Plot 8 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF14_850_Stocking_400, Plot==8), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 8")

# Plot 8 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg8 <- ddply(.data=subset(GF14_850_Stocking_400, Plot==8), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
(avg8)
avg8sd <- ddply(.data=subset(GF14_850_Stocking_400, Plot==8), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]# [,2] makes it a vec
avg8upper <- avg8$RingDen + avg8sd 
avg8lower <- avg8$RingDen - avg8sd 
head(avg8sd)



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



# Plot 8 - Provides graph of average ring mean density for plot with upper and lower levels for sd
ggplot(avg8, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg8lower, ymax=avg8upper), avg8, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 8 ring mean density")


#############################################################################
#Plot 45
# Plot 45 - Individually graphs pith to bark density for each tree for 1 plot
ggplot(subset(GF14_850_Stocking_400, Plot==45), aes(x=Year, y=Ring_Den, colour=as.factor(Tree), fill=as.factor(Tree))) + 
  geom_line() +
  facet_wrap(. ~ Tree) +
  theme_bw() +
  geom_smooth(method = "lm", alpha=0.1) + #aplha controls transparancy
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 45")


# Plot 45 - Produces averages and stdevs by ring for ring density and upper and lower confidence limits
avg45 <- ddply(.data=subset(GF14_850_Stocking_400, Plot==45), .(Year), .fun=summarise, RingDen = mean(Ring_Den))
avg45sd <- ddply(.data=subset(GF14_850_Stocking_400, Plot==45), .(Year), .fun=summarise, SD = sd(Ring_Den))[,2]
avg45upper <- avg45$RingDen + avg45sd 
avg45lower <- avg45$RingDen - avg45sd 
head(avg45sd)
(avg45sd)
# Plot 45 - Provides graph of average ring mean density for plot with upper and lower confidence limits
ggplot(avg45, aes(x=Year, y=RingDen)) + coord_cartesian(ylim = c(300, 550)) + theme_bw() +
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(colour='black', size=1, linetype='dashed') +
  geom_ribbon(aes(x=Year, ymin=avg45lower, ymax=avg45upper), avg45, alpha=0.2, colour='red', fill='green') +
  #facet_wrap(. ~ Tree) +
  #geom_smooth(method = "lm", alpha=0.2) +
  labs(colour = "Tree", fill = "Tree") +
  labs(x = "Year", y = "Ring mean density", title = "Plot 45 ring mean density")



#############################################################################

# Summarising several plots of same treatment - generate averages for each plot of same treatment and graph
avg845 <- ddply(.data=subset(GF14_850_Stocking_400, Plot==8|45), .(Plot,Year), .fun=summarise, RingDen = mean(Ring_Den),SD=sd(Ring_Den))
avg845$Plot<-as.character(avg845$Plot)
avg845$upper <- avg845$RingDen + avg845$SD 
avg845$lower <- avg845$RingDen - avg845$SD 
head(avg845)

ggplot(avg845, aes(x=Year, y=RingDen, group=Plot)) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) + 
  geom_line(aes(color=Plot), size=1)+
  geom_point(aes(color=Plot))+
  theme(legend.position="right")+
  theme_bw() +
  labs(title="GF14_850_Stocking_400")

#### average by RingDen by year for all GF14_850_400 spha  
avgGF14_400 <- ddply(.data=avg845, .(Year), .fun=summarise, RingDen = mean(RingDen))

#### Plot average RingDen by year for all GF14_850_400 spha
print(ggplot(avgGF14_400, aes(x=Year, y=RingDen,)) +
  geom_line(colour='black') +
  theme_bw() + ylim(300,500) + 
  scale_x_continuous(breaks=seq(1988,2020,2)) +  
  labs(title="GF14_850_Stocking_400"))
str(avgGF14_400)

#ylim(300,500)


