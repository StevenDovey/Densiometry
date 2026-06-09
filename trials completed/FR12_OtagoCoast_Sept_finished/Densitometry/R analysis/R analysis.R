rm(list=ls())
setwd("Q:/Forest Systems/Projects/Silviculture breeds trials/trials completed/FR12_OtagoCoast_Sept_finished/Densitometry/R analysis")
# Set libraries
library(ggplot2)


# Read Otago Coast wood properties file
rawdata <- read.csv('Otago Coast FR12 densitometer data.csv')
head(rawdata)
str(rawdata)
nrow(rawdata)
tail(rawdata)

GF14_850 <- rawdata[rawdata$Breed=="GF14_850",]
head(GF14_850)
str(GF14_850)
nrow(GF14_850)
tail(GF14_850)

GF14_850_Stocking_200 <- GF14_850[GF14_850$Stocking=="200",]
head(GF14_850_Stocking_200)
nrow(GF14_850_Stocking_200)

detach()
attach(GF14_850_Stocking_200)

Plot_Tree <- paste(Plot, Tree,sep="_")
head(Plot_Tree)
length(Plot_Tree)

GF14_850_year_den <- cbind.data.frame(Plot_Tree, Year, Ring_Den)
head(GF14_850_year_den)

ggplot(data= GF14_850_year_den) +
  geom_line(mapping = aes(x=Year, y=Ring_Den, color= Plot_Tree))




rawdata$breed