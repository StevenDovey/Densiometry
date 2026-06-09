# ***********************************************************************
# Initial single tree analysis for Glengarry data
# J.Dash - April 2016
# Jonathan.dash@scionresearch.com 
# ***********************************************************************

# Set libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(RODBC)
library(XLConnect)


# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}


se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))


# Read wood properties file
setwd('Q:\\Forest Systems\\Projects\\FR_10_Glengarry\\Standing tree wood properties\\')
wd.prop <- readWorksheetFromFile("Standing tree summary.xlsx",
                                 sheet=1)

# Make a plot identifier that can link to the inventory data
wd.prop$PlotName<-substr(wd.prop$Plot_Id, 13, 17)
wd.prop$PlotName<-as.factor(gsub("/", "_", wd.prop$PlotName))

# Trim out the white space caused by the way I've generated the plot lookup name
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
wd.prop$PlotName<-trim(wd.prop$PlotName)

#***************************************************************************
# First look at Value outs 
#**************************************************************************
setwd('Q:\\Forest Systems\\Projects\\FR_10_Glengarry\\PHI\\')


# Make a lookup table containing breed and sph treatment
wd.prop.lkup<- wd.prop %>% group_by(PlotName) %>% summarise (breed=max(Breed), sph.treatment=mean(Stocking))


lg.det<-read.csv('LogDetail.csv') # Get the log detail table
str(lg.det)
lg.det$val_vol<-lg.det$Value / lg.det$Volume # Calculate the value ver m3


lg.det.lkup<-merge(lg.det, wd.prop.lkup, by.x='PlotId', by.y="PlotName")

# external and internal attributes 
lg.det1<-subset(lg.det.lkup, YieldRequest == 'Strategy2')

lg.det1<-subset(lg.det1, Value >0.01)

# Calculate the values per tree
t1<- lg.det1 %>% group_by(PlotId, Tree) %>% summarise(TotalValue = sum(Value), 
                                                      PieceSize = sum(Volume), 
                                                      MeanValue=mean(Value),
                                                      MVm3= sum(Value) / sum (Volume),
                                                      Seedlot = max(breed),
                                                      sph.treatment = mean(sph.treatment))



#t1 has been verified as containing the correct summaries.
t1$Seedlot<-as.factor(t1$Seedlot)
t1$sph.treatment<-as.factor(t1$sph.treatment)

#Graph the data by the predictors
layout(matrix(2:1, ncol=2, byrow=T))
boxplot(TotalValue ~ Seedlot, data=t1)
boxplot(TotalValue ~ sph.treatment, data=t1)

plot(TotalValue ~ sph.treatment, data=t1)
str(t1)

#Fit a base level ANOVA as the inital model
m1<-lm(TotalValue ~ Seedlot * sph.treatment, data=t1, na.action = na.exclude)
summary(m1)
anova(m1)

#Diagnostic plots
layout(matrix(1:4, ncol=2, byrow=T))
plot(m1)

#Remove the interaction as it is not significant and refit the linear model
m2<-lm(TotalValue ~ Seedlot + sph.treatment, data=t1, na.action = na.exclude)
summary(m2)
anova(m2)

#Diagnostic plots for the updated model
layout(matrix(1:4, ncol=2, byrow=T))
plot(m2)


#Tukey test


library(multcomp)
summary(glht(model = m2, linfct = mcp(Seedlot = "Tukey")))
plot(glht(model = m2, linfct = mcp(Seedlot = "Tukey")))

boxplot(TotalValue ~ Seedlot, data=t1)

## Boxplot showing significance groups for the seedlot data
png('Value_Seedlot_tree.png', w=17, h=17, units="cm", res=300)
bp<-boxplot(TotalValue ~ Seedlot, data = t1, las = 1, 
            col = "lightgray", las = 1, boxwex = 0.5,
            ylab = "Tree value ($)", xlab = "Seedlot",
            ylim = c(0,1000))
#text(x = 1:5, y=bp$stats[5,] +100, c("a", "a", "b", "c", "b"))
text(x = 1:5, y=rep(800,5), c("a", "a", "b", "c", "b"))
dev.off()

summary(glht(model = m2, linfct = mcp(sph.treatment = "Tukey")))
plot(glht(model = m2, linfct = mcp(sph.treatment = "Tukey")))

## Boxplot showing significance groups for the silviculture
png('Value_Treatment_tree.png', w=17, h=17, units="cm", res=300)
bp2<-boxplot(TotalValue ~ sph.treatment, data = t1, las = 1, 
            col = "lightgray", las = 1, boxwex = 0.5,
            ylab = "Tree value ($)", xlab = "Stand density (sph)",
            ylim = c(0,1000))

text(x = 1:5, y=rep(800,5), c("a", "b", "c", "c", "d"))
dev.off()

###############################################################################
# Repeat the above anlalysis but with mean value per m3 rather than tree value
###############################################################################

#Graph the data by the predictors
layout(matrix(2:1, ncol=2, byrow=T))
boxplot(MVm3 ~ Seedlot, data=t1)
boxplot(MVm3 ~ sph.treatment, data=t1)

#Fit a base level ANOVA as the inital model
m1<-lm(MVm3 ~ Seedlot * sph.treatment, data=t1, na.action = na.exclude)
summary(m1)
anova(m1)

#Diagnostic plots
layout(matrix(1:4, ncol=2, byrow=T))
plot(m1)

#Remove the interaction as it is not significant and refit the linear model
m2<-lm(MVm3 ~ Seedlot + sph.treatment, data=t1, na.action = na.exclude)
summary(m2)
anova(m2)

#Diagnostic plots for the updated model
layout(matrix(1:4, ncol=2, byrow=T))
plot(m2)

summary(glht(model = m2, linfct = mcp(Seedlot = "Tukey")))
plot(glht(model = m2, linfct = mcp(Seedlot = "Tukey")))

## Boxplot showing significance groups for the seedlot data
png('MeanValue_m3_Seedlot_tree.png', w=17, h=17, units="cm", res=300)
bp<-boxplot(MVm3 ~ Seedlot, data = t1, las = 1, 
            col = "lightgray", las = 1, boxwex = 0.5,
            ylab = expression(paste("Mean value $/", m^{3})),
            xlab = "Seedlot",
            ylim = c(0,150))
#text(x = 1:5, y=bp$stats[5,] +100, c("a", "a", "b", "c", "b"))
text(x = 1:5, y=rep(140,5), c("a", "b", "ac", "b", "c"))
dev.off()

summary(glht(model = m2, linfct = mcp(sph.treatment = "Tukey")))
plot(glht(model = m2, linfct = mcp(sph.treatment = "Tukey")))

## Boxplot showing significance groups for the silviculture
png('MeanValue_m3_Treatment_tree.png', w=17, h=17, units="cm", res=300)
bp2<-boxplot(MVm3 ~ sph.treatment, data = t1, las = 1, 
             col = "lightgray", las = 1, boxwex = 0.5,
             ylab = expression(paste("Mean value $/", m^{3})), 
             xlab = "Stand density (sph)",
             ylim = c(0,150))

text(x = 1:5, y=rep(140,5), c("a", "b", "b", "a", "b"))
dev.off()

