wd.prop.atia$Breed<-as.character(wd.prop.atia$Breed)
#wd.prop.atia$Breed<- ifelse(wd.prop.atia$Breed %in% c('GF25', 'GF21') == "TRUE", "GF20+", wd.prop.atia$Breed)
wd.prop.atia$Breed<-as.factor(wd.prop.atia$Breed)
levels(wd.prop.atia$Breed)
# Plot the density of the ST300 and density data
# The red vertical line indicates the threshold cut off for inclusion in the structural products... this should be reviewed
png('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\graphical_outputs\\Velocity.png', w=20, h=15, units="cm", res=300)
layout(matrix(1:2, nrow = 1))
hist(wd.prop.atia$ST300.Average, col = "grey", prob = T, main ="Atiamuri", xlab = "Mean ST300 Velocity", ylim=c(0, 1.8))
lines(density(wd.prop.atia$ST300.Average, adjust=2, na.rm=T), lty="dotted")
abline(v=3.8815,col="red")
abline(v=mean(wd.prop.atia$ST300.Average, na.rm =T), col="blue")
text(3.8814, 1.6, "3.88", font = 2, cex = 1.4, col = "red")
text(mean(wd.prop.atia$ST300.Average, na.rm=T), 1.6, round(mean(wd.prop.atia$ST300.Average, na.rm=T),2), font = 2, cex = 1.4, col = "blue")
box()
hist(wd.prop.glen$Vel.avg, col = "grey", prob = T, main ="Glengarry", xlab = "Mean ST300 Velocity", ylim=c(0, 1.8))
lines(density(wd.prop.glen$Vel.avg, adjust=2, na.rm=T), lty="dotted")
abline(v=3.8815,col="red")
abline(v=mean(wd.prop.glen$Vel.avg, na.rm =T), col="blue")
text(3.8814, 1.6, "3.88", font = 2, cex = 1.4, col = "red")
text(mean(wd.prop.glen$Vel.avg, na.rm=T), 1.6, round(mean(wd.prop.glen$Vel.avg, na.rm=T),2), font = 2, cex = 1.4, col = "blue")
box()
dev.off()
###########################################################################
# Read the Inventory data
###########################################################################
#First read the Glengarry yields
glen.st.summ<-read.csv('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\FR_10_Glengarry\\PHI\\StandSummary.csv')
glen.lg.det<-read.csv('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\FR_10_Glengarry\\PHI\\LogDetail.csv') # Get the log detail table
# Make a lookup table containing breed and sph treatment for the Glengarry data
glen.wd.prop.lkup<- wd.prop.glen %>% group_by(PlotName, Breed) %>% summarise ( sph.treatment=mean(Stocking),
                                                                               Velocity = mean(Vel.avg, na.rm=TRUE),
                                                                               Density = mean(Den.avg, na.rm=TRUE))
# Add in the  sph treatment and seedlot from the lookup table for the Glengarry data
glen.lg.det.lkup<-merge(glen.lg.det, glen.wd.prop.lkup, by.x='PlotId', by.y="PlotName")
glen.st.summ.lkup<-merge(glen.st.summ, glen.wd.prop.lkup, by.x='PlotId', by.y="PlotName")
# Read the Atiamuri data
atia.st.summ<-read.csv('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\FR121_2_Kinleith\\Nov 2014 standing tree sampling\\PHI_Analysis\\StandSummary.csv')
atia.lg.det<-read.csv('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\FR121_2_Kinleith\\Nov 2014 standing tree sampling\\PHI_Analysis\\LogDetail.csv') # Get the log detail table
# Make a lookup table containing breed and sph treatment for the Atiamuri data
atia.wd.prop.lkup<- wd.prop.atia %>% group_by(Plot, Breed) %>% summarise (sph.treatment=mean(Stocking),
                                                                          Velocity = mean(ST300.Average, na.rm=TRUE),
                                                                          Density = mean(Den.Average, na.rm=TRUE))
# Add in the  sph treatment and seedlot from the lookup table for the Atiamuri data
atia.lg.det.lkup<-merge(atia.lg.det, atia.wd.prop.lkup, by.x='PlotId', by.y="Plot")
atia.st.summ.lkup<-merge(atia.st.summ, atia.wd.prop.lkup, by.x='PlotId', by.y="Plot")
##############################################################################################
# Now I'll make a plot level data table from all sites
##############################################################################################
str(atia.st.summ.lkup)
str(glen.st.summ.lkup)
# Get the naming and the structure of both
colnames(glen.st.summ.lkup)[colnames(glen.st.summ.lkup) == 'breed'] <- 'Breed' # rename the breed column to Breed
# Make covariates and plot name as factors
glen.st.summ.lkup$sph.treatment<-as.factor(glen.st.summ.lkup$sph.treatment)
glen.st.summ.lkup$Breed<-as.factor(glen.st.summ.lkup$Breed)
glen.st.summ.lkup$PlotId<-as.factor(glen.st.summ.lkup$PlotId)
glen.st.summ.lkup$Site<-as.factor('Glengarry')
atia.st.summ.lkup$sph.treatment<-as.factor(atia.st.summ.lkup$sph.treatment)
atia.st.summ.lkup$Breed<-as.factor(atia.st.summ.lkup$Breed)
atia.st.summ.lkup$PlotId<-as.factor(atia.st.summ.lkup$PlotId)
atia.st.summ.lkup$Site<-as.factor('Atiamuri')
st.summ.combined<-rbind(glen.st.summ.lkup, atia.st.summ.lkup)
st.summ.combined$MeanValue_m3<-st.summ.combined$TotalValue / st.summ.combined$TotalRecoverableVolume
str(st.summ.combined)
stmd<-read.csv('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\overview_phi_analysis\\StemDetail.csv')
stmd<-subset(stmd, YieldRequest == "Branching") # just use one yield request
# Make a lookup table containing breed and sph treatment for the Atiamuri data and include tree
atia.wd.prop.lkup.tr<- wd.prop.atia %>% group_by(Plot, Breed, TreeNo) %>% summarise (sph.treatment=mean(Stocking),
                                                                                     Velocity = mean(ST300.Average, na.rm=TRUE),
                                                                                     Density = mean(Den.Average, na.rm=TRUE))
# Make a lookup table containing breed and sph treatment for the Glengarry data and include tree
glen.wd.prop.lkup.tr<- wd.prop.glen %>% group_by(PlotName, Breed, TreeNo) %>% summarise (sph.treatment=mean(Stocking),
                                                                                         Velocity = mean(Vel.avg, na.rm=TRUE),
                                                                                         Density = mean(Den.avg, na.rm=TRUE))
# Split the master by site so that I can bring in the look up data
stmd.atia<-subset(stmd, Population == 'ATIA_PHI_323_0006_EST_0990_INV_SEP_14_JD')
stmd.glen<-subset(stmd, Population == 'Glengarry')
stmd.atia$Site<-factor('Atiamuri')
stmd.glen$Site<-factor('Glengarry')
# Add in the  sph treatment and seedlot from the lookup table for the Glengarry data
glen.wd.prop.lkup.tr$PlotName<-as.factor(glen.wd.prop.lkup.tr$PlotName)
glen.wd.prop.lkup.tr$TreeNo<-as.factor(glen.wd.prop.lkup.tr$TreeNo)
stmd.glen<-merge(stmd.glen, glen.wd.prop.lkup.tr, by.x=c('PlotId', 'Tree'), by.y=c("PlotName", 'TreeNo'))
# Get the naming and the structure of both
colnames(stmd.glen)[colnames(stmd.glen) == 'breed'] <- 'Breed' # rename the breed column to Breed
# Add in the  sph treatment and seedlot from the lookup table for the Atiamuri data
atia.wd.prop.lkup.tr$Plot<-as.factor(atia.wd.prop.lkup.tr$Plot)
atia.wd.prop.lkup.tr$TreeNo<-as.factor(atia.wd.prop.lkup.tr$TreeNo)
stmd.atia<-merge(stmd.atia, atia.wd.prop.lkup.tr, by.x=c('PlotId', 'Tree'), by.y=c("Plot", 'TreeNo'))
stmd<-rbind(stmd.glen, stmd.atia)
stmd<-merge(stmd, bv, by.x = c('Site', 'Breed'), by.y = c('Site', 'Breed'))
# Remove the stocking treatments that are not widespread
stmd<-subset(stmd, sph.treatment !=500)
stmd<-subset(stmd, sph.treatment !=300)
stmd<-subset(stmd, sph.treatment !=1000)
stmd<-subset(stmd, sph.treatment !=100)
stmd$sph.treatment<-factor(stmd$sph.treatment)
stmd$Breed<-factor(stmd$Breed)
stmd$Population<-ifelse(stmd$Population == 'ATIA_PHI_323_0006_EST_0990_INV_SEP_14_JD', '121/1', '10')
stmd$Population<-factor(stmd$Population)
levels(stmd$Population)
stmd<-stmd[complete.cases(stmd),]
stmd$Breed <- with(stmd, reorder(Breed, Velocity, mean)) # Order by the mean of the values
png('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\graphical_outputs\\Paper1Figures\\Figure4_Final.png', w=18, h=17, units="cm", res=300)
layout(matrix(1:4, nrow = 2), widths = c(60,40))
op <- par(mar = rep(0, 4), oma = c(4, 4, 3, 1), tcl = 0.35, mgp = c(2, 0.4, 0))
#op <- par(mar = c(0, 0.5, 0, 0), oma = c(4, 4, 3, 1), tcl = 0.35, mgp = c(2, 0.4, 0))
cax <- 1.1
boxplot(Velocity ~ Breed, data =  stmd, ylim = c(3,5.5), boxwex = 0.6, yaxs = "i",
        xaxs = "i",
        ylab = "Seedlot", xlab = "", las = 1, axes = F, cex.axis = cax, col = "lightgray", axes=F)
text(x = 1:5, y=rep(5.4,5), c("a", "a", "ab", "bc", "c")) # add the letters for the differences
mtext(text=expression(paste('Mean Velocity ( ', m.s^{-1}, ')')), side = 2, line = 2.6, cex = 1)
axis(2, lab = c(3.5,4.0,4.5,5.0,5.5), at =c(3.5,4.0,4.5,5.0,5.5))
#axis(1, lab=levels(br$Breed), at = c(1,2,3,4,5,6),  cex.axis = 1)
axis(1, lab = F)
#axis(1, cex.axis = 1, labels = levels(stmd$Breed), at = c(1,2,3,4,5))
#mtext(text = "Seedlot", side = 1,  las=1, line=2.6)
box()
boxplot(Density ~ Breed, data =  stmd, ylim = c(300,550), boxwex = 0.6, yaxs = "i",
        xaxs = "i",
        ylab = "Seedlot", xlab = "", las = 1, axes = F, cex.axis = cax, col = "lightgray", axes=F)
text(x = 1:5, y=rep(540,5), c("a", "b", "b", "c", "bc")) # add the letters for the differences
mtext(text=expression(paste('Density( ', m.s^{-1}, ')')), side = 2, line = 2.6, cex = 1)
axis(2, lab = T)
#axis(1, lab=levels(br$Breed), at = c(1,2,3,4,5,6),  cex.axis = 1)
#axis(1, lab = F)
axis(1, cex.axis = 1, labels = levels(stmd$Breed), at = c(1,2,3,4,5))
mtext(text = "Seedlot", side = 1,  las=1, line=2.6)
layout(matrix(1:4, nrow = 2), widths = c(60,40))
op <- par(mar = rep(0, 4), oma = c(4, 4, 3, 1), tcl = 0.35, mgp = c(2, 0.4, 0))
#op <- par(mar = c(0, 0.5, 0, 0), oma = c(4, 4, 3, 1), tcl = 0.35, mgp = c(2, 0.4, 0))
cax <- 1.1
boxplot(Velocity ~ Breed, data =  stmd, ylim = c(3,5.5), boxwex = 0.6, yaxs = "i",
        xaxs = "i",
        ylab = "Seedlot", xlab = "", las = 1, axes = F, cex.axis = cax, col = "lightgray", axes=F)
dev.off()
layout(matrix(1:4, nrow = 2), widths = c(60,40))
op <- par(mar = rep(0, 4), oma = c(4, 4, 3, 1), tcl = 0.35, mgp = c(2, 0.4, 0))
#op <- par(mar = c(0, 0.5, 0, 0), oma = c(4, 4, 3, 1), tcl = 0.35, mgp = c(2, 0.4, 0))
cax <- 1.1
boxplot(Velocity ~ Breed, data =  stmd, ylim = c(3,5.5), boxwex = 0.6, yaxs = "i",
        xaxs = "i",
        ylab = "Seedlot", xlab = "", las = 1, axes = F, cex.axis = cax, col = "lightgray", axes=F)
text(x = 1:5, y=rep(5.4,5), c("a", "a", "ab", "bc", "c")) # add the letters for the differences
wd.prop.atia$Breed<- ifelse(wd.prop.atia$Breed %in% c('GF25', 'GF21') == "TRUE", "GF20+", wd.prop.atia$Breed)
###########################################################################
# Read wood properties data
###########################################################################
# Read Glengarry wood properties file

wd.prop.glen<- readWorksheetFromFile('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\FR_10_Glengarry\\Standing tree wood properties\\Standing tree summary.xlsx',
                                     sheet=1)
# Make a plot identifier that can link to the inventory data
wd.prop.glen$PlotName<-substr(wd.prop.glen$Plot_Id, 13, 17)
wd.prop.glen$PlotName<-as.factor(gsub("/", "_", wd.prop.glen$PlotName))
# Trim out the white space caused by the way I've generated the plot lookup name
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
wd.prop.glen$PlotName<-trim(wd.prop.glen$PlotName)
wd.prop.glen$Breed<-as.character(wd.prop.glen$Breed)
wd.prop.glen$Breed<- ifelse(wd.prop.glen$Breed %in% c('GF25', 'GF21') == "TRUE", "GF20+", wd.prop.glen$Breed)
wd.prop.glen$Breed<-as.factor(wd.prop.glen$Breed)
# Read the Atiamuri wood properties and seedlot data
wd.prop.atia<-read.csv('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\FR121_2_Kinleith\\Nov 2014 standing tree sampling\\Standing tree data.csv')
wd.prop.atia<-wd.prop.atia[-374,] # drop that empty row that was annoying
wd.prop.atia$Breed<-as.character(wd.prop.atia$Breed)
wd.prop.atia$Breed<- ifelse(wd.prop.atia$Breed %in% c('GF25', 'GF21') == "TRUE", "GF20+", wd.prop.atia$Breed)
wd.prop.atia$Breed<-as.factor(wd.prop.atia$Breed)
levels(wd.prop.atia$Breed)
stmd<-read.csv('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\overview_phi_analysis\\StemDetail.csv')
stmd<-subset(stmd, YieldRequest == "Branching") # just use one yield request
# Make a lookup table containing breed and sph treatment for the Atiamuri data and include tree
atia.wd.prop.lkup.tr<- wd.prop.atia %>% group_by(Plot, Breed, TreeNo) %>% summarise (sph.treatment=mean(Stocking),
                                                                                     Velocity = mean(ST300.Average, na.rm=TRUE),
                                                                                     Density = mean(Den.Average, na.rm=TRUE))
# Make a lookup table containing breed and sph treatment for the Glengarry data and include tree
glen.wd.prop.lkup.tr<- wd.prop.glen %>% group_by(PlotName, Breed, TreeNo) %>% summarise (sph.treatment=mean(Stocking),
                                                                                         Velocity = mean(Vel.avg, na.rm=TRUE),
                                                                                         Density = mean(Den.avg, na.rm=TRUE))
# Split the master by site so that I can bring in the look up data
stmd.atia<-subset(stmd, Population == 'ATIA_PHI_323_0006_EST_0990_INV_SEP_14_JD')
stmd.glen<-subset(stmd, Population == 'Glengarry')
stmd.atia$Site<-factor('Atiamuri')
stmd.glen$Site<-factor('Glengarry')
# Add in the  sph treatment and seedlot from the lookup table for the Glengarry data
glen.wd.prop.lkup.tr$PlotName<-as.factor(glen.wd.prop.lkup.tr$PlotName)
glen.wd.prop.lkup.tr$TreeNo<-as.factor(glen.wd.prop.lkup.tr$TreeNo)
stmd.glen<-merge(stmd.glen, glen.wd.prop.lkup.tr, by.x=c('PlotId', 'Tree'), by.y=c("PlotName", 'TreeNo'))
# Get the naming and the structure of both
colnames(stmd.glen)[colnames(stmd.glen) == 'breed'] <- 'Breed' # rename the breed column to Breed
# Add in the  sph treatment and seedlot from the lookup table for the Atiamuri data
atia.wd.prop.lkup.tr$Plot<-as.factor(atia.wd.prop.lkup.tr$Plot)
atia.wd.prop.lkup.tr$TreeNo<-as.factor(atia.wd.prop.lkup.tr$TreeNo)
stmd.atia<-merge(stmd.atia, atia.wd.prop.lkup.tr, by.x=c('PlotId', 'Tree'), by.y=c("Plot", 'TreeNo'))
# merge them back together
stmd<-rbind(stmd.glen, stmd.atia)
# merge in the breeding values
stmd<-merge(stmd, bv, by.x = c('Site', 'Breed'), by.y = c('Site', 'Breed'))
stmd<-subset(stmd, sph.treatment !=500)
stmd<-subset(stmd, sph.treatment !=300)
stmd<-subset(stmd, sph.treatment !=1000)
stmd<-subset(stmd, sph.treatment !=100)
stmd$sph.treatment<-factor(stmd$sph.treatment)
stmd$Breed<-factor(stmd$Breed)
stmd$Population<-ifelse(stmd$Population == 'ATIA_PHI_323_0006_EST_0990_INV_SEP_14_JD', '121/1', '10')
stmd$Population<-factor(stmd$Population)
levels(stmd$Population)
stmd<-stmd[complete.cases(stmd),]
stmd$Breed <- with(stmd, reorder(Breed, Velocity, mean)) # Order by the mean of the values
layout(matrix(1:4, nrow = 2), widths = c(60,40))
op <- par(mar = rep(0, 4), oma = c(4, 4, 3, 1), tcl = 0.35, mgp = c(2, 0.4, 0))
#op <- par(mar = c(0, 0.5, 0, 0), oma = c(4, 4, 3, 1), tcl = 0.35, mgp = c(2, 0.4, 0))
cax <- 1.1
boxplot(Velocity ~ Breed, data =  stmd, ylim = c(3,5.5), boxwex = 0.6, yaxs = "i",
        xaxs = "i",
        ylab = "Seedlot", xlab = "", las = 1, axes = F, cex.axis = cax, col = "lightgray", axes=F)
text(x = 1:5, y=rep(5.4,5), c("a", "a", "ab", "bc", "c")) # add the letters for the differences
mtext(text=expression(paste('Mean Velocity ( ', m.s^{-1}, ')')), side = 2, line = 2.6, cex = 1)
axis(2, lab = c(3.5,4.0,4.5,5.0,5.5), at =c(3.5,4.0,4.5,5.0,5.5))
#axis(1, lab=levels(br$Breed), at = c(1,2,3,4,5,6),  cex.axis = 1)
axis(1, lab = F)
#axis(1, cex.axis = 1, labels = levels(stmd$Breed), at = c(1,2,3,4,5))
#mtext(text = "Seedlot", side = 1,  las=1, line=2.6)
box()
boxplot(Density ~ Breed, data =  stmd, ylim = c(300,550), boxwex = 0.6, yaxs = "i",
        xaxs = "i",
        ylab = "Seedlot", xlab = "", las = 1, axes = F, cex.axis = cax, col = "lightgray", axes=F)
text(x = 1:5, y=rep(540,5), c("a", "b", "b", "c", "bc")) # add the letters for the differences
mtext(text=expression(paste('Density( ', kg.s^{-1}, ')')), side = 2, line = 2.6, cex = 1)
axis(2, lab = T)
#axis(1, lab=levels(br$Breed), at = c(1,2,3,4,5,6),  cex.axis = 1)
#axis(1, lab = F)
axis(1, cex.axis = 1, labels = levels(stmd$Breed), at = c(1,2,3,4,5))
mtext(text = "Seedlot", side = 1,  las=1, line=2.6)
box()
boxplot(Velocity ~ sph.treatment, data =  stmd, ylim = c(3,5.5),boxwex = 0.5, yaxs = "i",
        xaxs = "i", las=1,
        ylab = "", xlab = "Treatment (sph)", axes = F, col = "lightgray")
text(x = 1:3, y=rep(5.4,3), c("a", "b", "b")) # add the letters for the differences
#axis(1, cex.axis = 1, labels = levels(br$sph.treatment), at = c(1,2,3,4))
axis(2, lab = F)
axis(1, lab = F, at = c(1,2,3))
#axis(1, cex.axis = 1, labels = levels(stmd$sph.treatment), at = c(1,2,3))
#mtext(text = "Treatment (sph)", side = 1,  las=1, line=2.6)
box()
boxplot(Density ~ sph.treatment, data =  stmd, ylim = c(300,550), boxwex = 0.6, yaxs = "i",
        xaxs = "i",
        ylab = "Seedlot", xlab = "", las = 1, axes = F, cex.axis = cax, col = "lightgray", axes=F)
text(x = 1:3, y=rep(540,3), c("a", "ab", "b")) # add the letters for the differences
#mtext(text=expression(paste('Density( ', m.s^{-1}, ')')), side = 2, line = 2.6, cex = 1)
axis(2, lab = F)
axis(1, lab=levels(br$sph.treatment), at = c(1,2,3),  cex.axis = 1)
axis(1, cex.axis = 1, labels = levels(stmd$sph.treatment), at = c(1,2,3))
mtext(text = "Treatment (sph)", side = 1,  las=1, line=2.6)
box()
stmd$Breed <- with(stmd, reorder(Breed, Velocity, mean)) # Order by the mean of the values
png('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\graphical_outputs\\Paper1Figures\\Figure4_Final.png', w=18, h=17, units="cm", res=300)
layout(matrix(1:4, nrow = 2), widths = c(60,40))
op <- par(mar = rep(0, 4), oma = c(4, 4, 3, 1), tcl = 0.35, mgp = c(2, 0.4, 0))
#op <- par(mar = c(0, 0.5, 0, 0), oma = c(4, 4, 3, 1), tcl = 0.35, mgp = c(2, 0.4, 0))
cax <- 1.1
boxplot(Velocity ~ Breed, data =  stmd, ylim = c(3,5.5), boxwex = 0.6, yaxs = "i",
        xaxs = "i",
        ylab = "Seedlot", xlab = "", las = 1, axes = F, cex.axis = cax, col = "lightgray", axes=F)
text(x = 1:5, y=rep(5.4,5), c("a", "a", "ab", "bc", "c")) # add the letters for the differences
mtext(text=expression(paste('Mean Velocity ( ', m.s^{-1}, ')')), side = 2, line = 2.6, cex = 1)
axis(2, lab = c(3.5,4.0,4.5,5.0,5.5), at =c(3.5,4.0,4.5,5.0,5.5))
#axis(1, lab=levels(br$Breed), at = c(1,2,3,4,5,6),  cex.axis = 1)
axis(1, lab = F)
#axis(1, cex.axis = 1, labels = levels(stmd$Breed), at = c(1,2,3,4,5))
#mtext(text = "Seedlot", side = 1,  las=1, line=2.6)
box()
boxplot(Density ~ Breed, data =  stmd, ylim = c(300,550), boxwex = 0.6, yaxs = "i",
        xaxs = "i",
        ylab = "Seedlot", xlab = "", las = 1, axes = F, cex.axis = cax, col = "lightgray", axes=F)
text(x = 1:5, y=rep(540,5), c("a", "b", "b", "c", "bc")) # add the letters for the differences
mtext(text=expression(paste('Density( ', kg.s^{-1}, ')')), side = 2, line = 2.6, cex = 1)
axis(2, lab = T)
#axis(1, lab=levels(br$Breed), at = c(1,2,3,4,5,6),  cex.axis = 1)
#axis(1, lab = F)
axis(1, cex.axis = 1, labels = levels(stmd$Breed), at = c(1,2,3,4,5))
mtext(text = "Seedlot", side = 1,  las=1, line=2.6)
box()
boxplot(Velocity ~ sph.treatment, data =  stmd, ylim = c(3,5.5),boxwex = 0.5, yaxs = "i",
        xaxs = "i", las=1,
        ylab = "", xlab = "Treatment (sph)", axes = F, col = "lightgray")
text(x = 1:3, y=rep(5.4,3), c("a", "b", "b")) # add the letters for the differences
#axis(1, cex.axis = 1, labels = levels(br$sph.treatment), at = c(1,2,3,4))
axis(2, lab = F)
axis(1, lab = F, at = c(1,2,3))
#axis(1, cex.axis = 1, labels = levels(stmd$sph.treatment), at = c(1,2,3))
#mtext(text = "Treatment (sph)", side = 1,  las=1, line=2.6)
box()
boxplot(Density ~ sph.treatment, data =  stmd, ylim = c(300,550), boxwex = 0.6, yaxs = "i",
        xaxs = "i",
        ylab = "Seedlot", xlab = "", las = 1, axes = F, cex.axis = cax, col = "lightgray", axes=F)
text(x = 1:3, y=rep(540,3), c("a", "ab", "b")) # add the letters for the differences
#mtext(text=expression(paste('Density( ', m.s^{-1}, ')')), side = 2, line = 2.6, cex = 1)
axis(2, lab = F)
#axis(1, lab=levels(br$sph.treatment), at = c(1,2,3),  cex.axis = 1)
#axis(1, lab = F)
axis(1, cex.axis = 1, labels = levels(stmd$sph.treatment), at = c(1,2,3))
mtext(text = "Treatment (sph)", side = 1,  las=1, line=2.6)
box()
dev.off()
# ****************************************************************************************************************
# Silvicultrual breeds trials
# This script will form the basis of the meta-analysis including all of the silvicultural breeds trials. The
# response variable in this analysis will be $ value based on outputs from YTGEN. All plot and tree values have
# been grown to age 28 using the 300-index growth model
# I'm going to use absolute file paths as there is data in several directories... if the folder structure changes
# this will break.
# J.Dash, Scion, May, 2016, jonathan.dash@scionresearch.com
# ****************************************************************************************************************
# Set libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(XLConnect)
###Get a few helper functions
# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}
# Function that returns standard error
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
###########################################################################
# Read wood properties data
###########################################################################
# Read Glengarry wood properties file
wd.prop.glen<- readWorksheetFromFile('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\FR_10_Glengarry\\Standing tree wood properties\\Standing tree summary.xlsx',
                                     sheet=1)
# Make a plot identifier that can link to the inventory data
wd.prop.glen$PlotName<-substr(wd.prop.glen$Plot_Id, 13, 17)
wd.prop.glen$PlotName<-as.factor(gsub("/", "_", wd.prop.glen$PlotName))
# Trim out the white space caused by the way I've generated the plot lookup name
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
wd.prop.glen$PlotName<-trim(wd.prop.glen$PlotName)
wd.prop.glen$Breed<-as.character(wd.prop.glen$Breed)
#wd.prop.glen$Breed<- ifelse(wd.prop.glen$Breed %in% c('GF25', 'GF21') == "TRUE", "GF20+", wd.prop.glen$Breed)
wd.prop.glen$Breed<-as.factor(wd.prop.glen$Breed)
# Read the Atiamuri wood properties and seedlot data
wd.prop.atia<-read.csv('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\FR121_2_Kinleith\\Nov 2014 standing tree sampling\\Standing tree data.csv')
wd.prop.atia<-wd.prop.atia[-374,] # drop that empty row that was annoying
wd.prop.atia$Breed<-as.character(wd.prop.atia$Breed)
#wd.prop.atia$Breed<- ifelse(wd.prop.atia$Breed %in% c('GF25', 'GF21') == "TRUE", "GF20+", wd.prop.atia$Breed)
wd.prop.atia$Breed<-as.factor(wd.prop.atia$Breed)
levels(wd.prop.atia$Breed)
# Plot the density of the ST300 and density data
# The red vertical line indicates the threshold cut off for inclusion in the structural products... this should be reviewed
png('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\graphical_outputs\\Velocity.png', w=20, h=15, units="cm", res=300)
layout(matrix(1:2, nrow = 1))
hist(wd.prop.atia$ST300.Average, col = "grey", prob = T, main ="Atiamuri", xlab = "Mean ST300 Velocity", ylim=c(0, 1.8))
lines(density(wd.prop.atia$ST300.Average, adjust=2, na.rm=T), lty="dotted")
abline(v=3.8815,col="red")
abline(v=mean(wd.prop.atia$ST300.Average, na.rm =T), col="blue")
text(3.8814, 1.6, "3.88", font = 2, cex = 1.4, col = "red")
text(mean(wd.prop.atia$ST300.Average, na.rm=T), 1.6, round(mean(wd.prop.atia$ST300.Average, na.rm=T),2), font = 2, cex = 1.4, col = "blue")
box()
hist(wd.prop.glen$Vel.avg, col = "grey", prob = T, main ="Glengarry", xlab = "Mean ST300 Velocity", ylim=c(0, 1.8))
lines(density(wd.prop.glen$Vel.avg, adjust=2, na.rm=T), lty="dotted")
abline(v=3.8815,col="red")
abline(v=mean(wd.prop.glen$Vel.avg, na.rm =T), col="blue")
text(3.8814, 1.6, "3.88", font = 2, cex = 1.4, col = "red")
text(mean(wd.prop.glen$Vel.avg, na.rm=T), 1.6, round(mean(wd.prop.glen$Vel.avg, na.rm=T),2), font = 2, cex = 1.4, col = "blue")
box()
dev.off()
###########################################################################
# Read the Inventory data
###########################################################################
#First read the Glengarry yields
glen.st.summ<-read.csv('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\FR_10_Glengarry\\PHI\\StandSummary.csv')
glen.lg.det<-read.csv('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\FR_10_Glengarry\\PHI\\LogDetail.csv') # Get the log detail table
# Make a lookup table containing breed and sph treatment for the Glengarry data
glen.wd.prop.lkup<- wd.prop.glen %>% group_by(PlotName, Breed) %>% summarise ( sph.treatment=mean(Stocking),
                                                                               Velocity = mean(Vel.avg, na.rm=TRUE),
                                                                               Density = mean(Den.avg, na.rm=TRUE))
# Add in the  sph treatment and seedlot from the lookup table for the Glengarry data
glen.lg.det.lkup<-merge(glen.lg.det, glen.wd.prop.lkup, by.x='PlotId', by.y="PlotName")
glen.st.summ.lkup<-merge(glen.st.summ, glen.wd.prop.lkup, by.x='PlotId', by.y="PlotName")
# Read the Atiamuri data
atia.st.summ<-read.csv('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\FR121_2_Kinleith\\Nov 2014 standing tree sampling\\PHI_Analysis\\StandSummary.csv')
atia.lg.det<-read.csv('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\FR121_2_Kinleith\\Nov 2014 standing tree sampling\\PHI_Analysis\\LogDetail.csv') # Get the log detail table
# Make a lookup table containing breed and sph treatment for the Atiamuri data
atia.wd.prop.lkup<- wd.prop.atia %>% group_by(Plot, Breed) %>% summarise (sph.treatment=mean(Stocking),
                                                                          Velocity = mean(ST300.Average, na.rm=TRUE),
                                                                          Density = mean(Den.Average, na.rm=TRUE))
# Add in the  sph treatment and seedlot from the lookup table for the Atiamuri data
atia.lg.det.lkup<-merge(atia.lg.det, atia.wd.prop.lkup, by.x='PlotId', by.y="Plot")
atia.st.summ.lkup<-merge(atia.st.summ, atia.wd.prop.lkup, by.x='PlotId', by.y="Plot")
##############################################################################################
# Now I'll make a plot level data table from all sites
##############################################################################################
str(atia.st.summ.lkup)
str(glen.st.summ.lkup)
# Get the naming and the structure of both
colnames(glen.st.summ.lkup)[colnames(glen.st.summ.lkup) == 'breed'] <- 'Breed' # rename the breed column to Breed
# Make covariates and plot name as factors
glen.st.summ.lkup$sph.treatment<-as.factor(glen.st.summ.lkup$sph.treatment)
glen.st.summ.lkup$Breed<-as.factor(glen.st.summ.lkup$Breed)
glen.st.summ.lkup$PlotId<-as.factor(glen.st.summ.lkup$PlotId)
glen.st.summ.lkup$Site<-as.factor('Glengarry')
atia.st.summ.lkup$sph.treatment<-as.factor(atia.st.summ.lkup$sph.treatment)
atia.st.summ.lkup$Breed<-as.factor(atia.st.summ.lkup$Breed)
atia.st.summ.lkup$PlotId<-as.factor(atia.st.summ.lkup$PlotId)
atia.st.summ.lkup$Site<-as.factor('Atiamuri')
st.summ.combined<-rbind(glen.st.summ.lkup, atia.st.summ.lkup)
st.summ.combined$MeanValue_m3<-st.summ.combined$TotalValue / st.summ.combined$TotalRecoverableVolume
str(st.summ.combined)
##############################################################################################
# Now examine and then analyse the data across sites
##############################################################################################
plot(TotalValue ~ YieldRequest, data = st.summ.combined)
plot(TotalValue ~ Breed, data = st.summ.combined)
plot(TotalValue ~ Site, data = st.summ.combined)
plot(TotalValue ~ sph.treatment, data = st.summ.combined)
#Use the yield request that accounts for internal and external wood properties
st.summ.combined.2<-subset(st.summ.combined, YieldRequest == 'Strategy2')
#Group TotalValue by breed and sph for graphing
grouped3<- st.summ.combined.2%>% group_by(Breed, sph.treatment) %>%
  summarise (mean = mean(TotalValue), sum=sum(TotalValue), n=length(TotalValue), sd=sd(TotalValue), se=se(TotalValue))
limits <- aes(ymax = mean + se, ymin=mean - se)
#png('Mean_value_breed__sph_internal.png', w = 17, h=10, units="cm", res=300)
ggplot(grouped3, aes(y=mean, x=Breed)) +
  geom_point() +
  geom_errorbar(limits, width=0.2) +
  labs(x="Breed", y=expression(paste('Total Value ($ / ', m^{3}, ')'))) +
  facet_grid(. ~ sph.treatment) +
  theme_light() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
dev.off()
#Remove treatments for 1000, 300, 500  and 100 because these are not replicated
st.summ.combined.2<-subset(st.summ.combined.2, sph.treatment !=500)
st.summ.combined.2<-subset(st.summ.combined.2, sph.treatment !=300)
st.summ.combined.2<-subset(st.summ.combined.2, sph.treatment !=1000)
st.summ.combined.2<-subset(st.summ.combined.2, sph.treatment !=100)
st.summ.combined.2$sph.treatment<-factor(st.summ.combined.2$sph.treatment)
st.summ.combined.2$Breed<-factor(st.summ.combined.2$Breed)
str(st.summ.combined.2)
#######################################################################################################################
###########################################################################
# Read wood properties data
###########################################################################
# Read Glengarry wood properties file
wd.prop.glen<- readWorksheetFromFile('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\FR_10_Glengarry\\Standing tree wood properties\\Standing tree summary.xlsx',
                                     sheet=1)
# Make a plot identifier that can link to the inventory data
wd.prop.glen$PlotName<-substr(wd.prop.glen$Plot_Id, 13, 17)
wd.prop.glen$PlotName<-as.factor(gsub("/", "_", wd.prop.glen$PlotName))
# Trim out the white space caused by the way I've generated the plot lookup name
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
wd.prop.glen$PlotName<-trim(wd.prop.glen$PlotName)
wd.prop.glen$Breed<-as.character(wd.prop.glen$Breed)
#wd.prop.glen$Breed<- ifelse(wd.prop.glen$Breed %in% c('GF25', 'GF21') == "TRUE", "GF20+", wd.prop.glen$Breed)
wd.prop.glen$Breed<-as.factor(wd.prop.glen$Breed)
# Read the Atiamuri wood properties and seedlot data
wd.prop.atia<-read.csv('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\FR121_2_Kinleith\\Nov 2014 standing tree sampling\\Standing tree data.csv')
wd.prop.atia<-wd.prop.atia[-374,] # drop that empty row that was annoying
wd.prop.atia$Breed<-as.character(wd.prop.atia$Breed)
#wd.prop.atia$Breed<- ifelse(wd.prop.atia$Breed %in% c('GF25', 'GF21') == "TRUE", "GF20+", wd.prop.atia$Breed)
wd.prop.atia$Breed<-as.factor(wd.prop.atia$Breed)
levels(wd.prop.atia$Breed)
glen.st.summ<-read.csv('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\FR_10_Glengarry\\PHI\\StandSummary.csv')
# Make a lookup table containing breed and sph treatment for the Glengarry data
glen.wd.prop.lkup<- wd.prop.glen %>% group_by(PlotName, Breed) %>% summarise ( sph.treatment=mean(Stocking),
                                                                               Velocity = mean(Vel.avg, na.rm=TRUE),
                                                                               Density = mean(Den.avg, na.rm=TRUE))
# Add in the  sph treatment and seedlot from the lookup table for the Glengarry data
glen.lg.det.lkup<-merge(glen.lg.det, glen.wd.prop.lkup, by.x='PlotId', by.y="PlotName")
glen.st.summ.lkup<-merge(glen.st.summ, glen.wd.prop.lkup, by.x='PlotId', by.y="PlotName")
atia.st.summ<-read.csv('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\FR121_1_Kinleith\\Nov 2014 standing tree sampling\\PHI_Analysis\\StandSummary.csv')
atia.wd.prop.lkup<- wd.prop.atia %>% group_by(Plot, Breed) %>% summarise (sph.treatment=mean(Stocking),
                                                                          Velocity = mean(ST300.Average, na.rm=TRUE),
                                                                          Density = mean(Den.Average, na.rm=TRUE))
# Add in the  sph treatment and seedlot from the lookup table for the Atiamuri data
atia.lg.det.lkup<-merge(atia.lg.det, atia.wd.prop.lkup, by.x='PlotId', by.y="Plot")
atia.st.summ.lkup<-merge(atia.st.summ, atia.wd.prop.lkup, by.x='PlotId', by.y="Plot")
colnames(glen.st.summ.lkup)[colnames(glen.st.summ.lkup) == 'breed'] <- 'Breed' # rename the breed column to Breed
glen.st.summ.lkup$sph.treatment<-as.factor(glen.st.summ.lkup$sph.treatment)
glen.st.summ.lkup$Breed<-as.factor(glen.st.summ.lkup$Breed)
glen.st.summ.lkup$PlotId<-as.factor(glen.st.summ.lkup$PlotId)
glen.st.summ.lkup$Site<-as.factor('Glengarry')
atia.st.summ.lkup$sph.treatment<-as.factor(atia.st.summ.lkup$sph.treatment)
atia.st.summ.lkup$Breed<-as.factor(atia.st.summ.lkup$Breed)
atia.st.summ.lkup$PlotId<-as.factor(atia.st.summ.lkup$PlotId)
atia.st.summ.lkup$Site<-as.factor('Atiamuri')
st.summ.combined<-rbind(glen.st.summ.lkup, atia.st.summ.lkup)
st.summ.combined$MeanValue_m3<-st.summ.combined$TotalValue / st.summ.combined$TotalRecoverableVolume
st.summ.combined.2<-subset(st.summ.combined, YieldRequest == 'Strategy2')
st.summ.combined.2<-subset(st.summ.combined.2, sph.treatment !=500)
bv<-read.csv('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\overview_phi_analysis\\breedingvalues.csv')
st.summ.bv<-merge(st.summ.combined.2, bv, by.x = c('Site', 'Breed'), by.y = c('Site', 'Breed'))
ggplot(st.summ.bv, aes(x=DBH_trait, y=TotalValue/1000)) + geom_point() +
  facet_wrap(~sph.treatment) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(x = 'GF+ dbh trait', y = 'Total Value ($000s)')
png('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\graphical_outputs\\TotValue_brtrait.png', w=20, h=15, units="cm", res=300)
ggplot(st.summ.bv, aes(x=Br_Trait, y=TotalValue)) + geom_point() +
  facet_wrap(~sph.treatment) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(x = 'GF+ br trait', y = 'Total Value ($000s)')
dev.off()
png('Q:\\Forest Systems\\Projects\\Silviculture breeds trials\\graphical_outputs\\TotValue_swtrait.png', w=20, h=15, units="cm", res=300)
ggplot(st.summ.bv, aes(x=Sw_Trait, y=TotalValue)) + geom_point() +
  facet_wrap(~sph.treatment) +
  geom_smooth(method="lm")  +
  theme_bw() +
  labs(x = 'GF+ sw trait', y = 'Total Value ($000s)')
dev.off()
