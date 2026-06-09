rm(list=objects())
setwd("Q:/Forest Systems/Projects/Silviculture breeds trials/FR121_2_Kinleith_finished/Nov 2014 standing tree sampling")
streedata <-read.table('standing tree data.csv',sep=',',header=TRUE)
head(streedata)
# Read the Atiamuri wood properties and seedlot data
wd.prop.atia<-read.csv('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\FR121_2_Kinleith_finished\\Nov 2014 standing tree sampling\\Standing tree data.csv')
wd.prop.atia<-wd.prop.atia[-374,] # drop that empty row that was annoying
wd.prop.atia$Breed<-as.character(wd.prop.atia$Breed)
#wd.prop.atia$Breed<- ifelse(wd.prop.atia$Breed %in% c('GF25', 'GF21') == "TRUE", "GF20+", wd.prop.atia$Breed)
wd.prop.atia$Breed<-as.factor(wd.prop.atia$Breed)
levels(wd.prop.atia$Breed)
# Plot the density of the ST300 and density data
# The red vertical line indicates the threshold cut off for inclusion in the structural products... this should be reviewed
png('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\Velocity.png', w=20, h=15, units="cm", res=300)
layout(matrix(1:2, nrow = 1))
hist(wd.prop.atia$ST300.Average, col = "grey", prob = T, main ="Atiamuri", xlab = "Mean ST300 Velocity", ylim=c(0, 1.8))
lines(density(wd.prop.atia$ST300.Average, adjust=2, na.rm=T), lty="dotted")
abline(v=3.8815,col="red")
abline(v=mean(wd.prop.atia$ST300.Average, na.rm =T), col="blue")
text(3.8814, 1.6, "3.88", font = 2, cex = 1.4, col = "red")
text(mean(wd.prop.atia$ST300.Average, na.rm=T), 1.6, round(mean(wd.prop.atia$ST300.Average, na.rm=T),2), font = 2, cex = 1.4, col = "blue")
box()
