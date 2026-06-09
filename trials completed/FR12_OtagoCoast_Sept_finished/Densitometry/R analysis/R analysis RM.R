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

#######################################


##########################################################
# From all GF14_850 200spha data select "Thin" trees
GF14_850_Stocking_200_Thin <- GF14_850_Stocking_200[GF14_850_Stocking_200$Thinning=="Thin",]
head(GF14_850_Stocking_200_Thin)
nrow(GF14_850_Stocking_200_Thin)

# From the GF14_850_Stocking_200_Thin selection separate out Plot and Tree info with "_" between values
GF14_850_Stocking_200_Thin_Plot_Tree <- paste(GF14_850_Stocking_200_Thin$Plot, GF14_850_Stocking_200_Thin$Tree,sep="_")
head(GF14_850_Stocking_200_Thin_Plot_Tree)
length(GF14_850_Stocking_200_Thin_Plot_Tree)

# Create dataframe with cbind function to produce all GF14_850 with plot_tree, year and ring density - to be used for graph 
GF14_850_Stocking_200_Thin_year_den <- cbind.data.frame(GF14_850_Stocking_200_Thin_Plot_Tree, GF14_850_Stocking_200_Thin$Year, GF14_850_Stocking_200_Thin$Ring_Den)
head(GF14_850_Stocking_200_Thin_year_den)
nrow(GF14_850_Stocking_200_Thin_year_den)

ggplot(data = GF14_850_Stocking_200_Thin_year_den) +
  geom_line(mapping = aes(x=GF14_850_Stocking_200_Thin$Year, y=GF14_850_Stocking_200_Thin$Ring_Den, color= GF14_850_Stocking_200_Thin_Plot_Tree))
#########################

# Create dataframe with cbind function to produce all GF14_850 with plot, plot_tree, year and ring density - to be used for graph 
GF14_850_Stocking_200_year_den <- cbind.data.frame(GF14_850_Stocking_200$Plot, GF14_850_Stocking_200_Plot_Tree, GF14_850_Stocking_200$Year, GF14_850_Stocking_200$Ring_Den)
head(GF14_850_Stocking_200_year_den)
nrow(GF14_850_Stocking_200_year_den)

ggplot(data = GF14_850_Stocking_200_year_den) +
  geom_line(mapping = aes(x=GF14_850_Stocking_200$Year, y=GF14_850_Stocking_200$Ring_Den, color= GF14_850_Stocking_200$Plot))
#
