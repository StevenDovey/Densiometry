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

# From rawdata select all GF14_850 
GF14_850 <- rawdata[rawdata$Breed=="GF14_850",]
head(GF14_850)
str(GF14_850)
nrow(GF14_850)
tail(GF14_850)

# From all GF14_850 data select all 200spha
GF14_850_Stocking_200 <- GF14_850[GF14_850$Stocking=="200",]
head(GF14_850_Stocking_200)
nrow(GF14_850_Stocking_200)

# From the GF14_850_Stocking_200 selection separate out Plot and Tree info with "_" between values
#GF14_850_Stocking_200_Plot_Tree <- paste(GF14_850_Stocking_200$Plot, GF14_850_Stocking_200$Tree,sep="_")
#head(GF14_850_Stocking_200_Plot_Tree)
#length(GF14_850_Stocking_200_Plot_Tree)

# Create dataframe with cbind function to produce all GF14_850 with plot_tree, year and ring density - to be used for graph 
GF14_850_Stocking_200_year_den <- cbind.data.frame(GF14_850_Stocking_200$Plot, GF14_850_Stocking_200$Tree, GF14_850_Stocking_200$Year, GF14_850_Stocking_200$Ring_Den)
head(GF14_850_Stocking_200_year_den)
str(GF14_850_Stocking_200_year_den)
nrow(GF14_850_Stocking_200_year_den)
colnames(GF14_850_Stocking_200_year_den) <- c("Plot","Tree","Year","Ring_Dens")
GF14_850_Stocking_200_year_den$Plot <- as.factor(GF14_850_Stocking_200_year_den$Plot)
GF14_850_Stocking_200_year_den$Tree <- as.factor(GF14_850_Stocking_200_year_den$Tree)

plot10 <- GF14_850_Stocking_200_year_den[GF14_850_Stocking_200_year_den$Plot == "10",]
plot(GF14_850_Stocking_200_year_den$Ring_Dens[GF14_850_Stocking_200_year_den$Plot == "10"] ~ GF14_850_Stocking_200_year_den$Year[GF14_850_Stocking_200_year_den$Plot == "10"])

ring_dens_lm <- lm(plot10$Ring_Dens~plot10$Year)
ring_dens_lm$coefficients
abline(ring_dens_lm$coefficients)

str(plot10)
plot10$Tree <- droplevels(plot10$Tree)
min(plot10$Ring_Dens)
levels(plot10$Tree)
i <- 3
for (i in levels(plot10$Tree))
{
  dat <- plot10[plot10$Tree == i,]
  plot(dat$Ring_Dens~dat$Year, type = "l", xlim = c(1990,2016), ylim = c(250,500) )
  #lm <- lm(dat$Ring_Dens~dat$Year)
  #abline(lm$coefficients)
  par(new = T)
}
par(new = F)
ring_dens_lm <- lm(plot10$Ring_Dens~plot10$Year)
ring_dens_lm$coefficients
abline(ring_dens_lm$coefficients)
abline(h = mean(plot10$Ring_Dens))
