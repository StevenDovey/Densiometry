
# ***********************************************************************
# Initial analysis for Glengarry data
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
wd.prop$PlotName<-gsub("/", "_", wd.prop$PlotName)

# Trim out the white space caused by the way I've generated the plot lookup name
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
wd.prop$PlotName<-trim(wd.prop$PlotName)

summary(wd.prop)

# Densityplot density grouped by Breed-stocking 
ggplot(wd.prop, aes(x=Den.avg, group=Breed.Stocking)) + 
  geom_density(aes(group=Breed.Stocking, colour=Breed.Stocking, fill=Breed.Stocking),  alpha=.3)  + 
  theme_light()


# Densityplot density grouped by Breed
#png('density_breed.png', w = 17, h=17, units="cm", res=300)
ggplot(wd.prop, aes(x=Den.avg, group=Breed)) + 
  geom_density(aes(group=Breed, colour=Breed, fill=Breed),  alpha=.3)  + 
  theme_light() +
  labs(x="Outerwood density")

#dev.off()


# Densityplot velocity grouped by Breed
#png('velocity_breed.png', w = 17, h=17, units="cm", res=300)
ggplot(wd.prop, aes(x=Vel.avg, group=Breed)) + 
  geom_density(aes(group=Breed, colour=Breed, fill=Breed),  alpha=.3)  + 
  theme_light() +
  labs(x="Velocity")
#dev.off()



#***************************************************************************
# First look at Value outs 
#**************************************************************************
setwd('Q:\\Forest Systems\\Projects\\FR_10_Glengarry\\PHI\\')

# Stand summary contains a one line summary per plot detailing the yields measured at Glengarry
# YieldRequest is the key to the cutting strategy Strategy1 does not account for wood properties
# Strategy 2 does.
st.summ<-read.csv('StandSummary.csv')
pr.summ<-read.csv('ProductYields.csv')

# Quick check to see if the value is different between the two strategies
st.s.val<- st.summ %>% group_by (YieldRequest) %>% summarise(Value=mean(TotalValue))
View(st.s.val) # They are different and consistent Strategy is more valuable.

# Can we bring in GF and stocking treatment from the wood properties summary

# Make a lookup table containing breed and sph treatment
wd.prop.lkup<- wd.prop %>% group_by(PlotName) %>% summarise (breed=max(Breed), sph.treatment=mean(Stocking))

# merge these into a table for graphing
st.sum.lkup<-merge(st.summ, wd.prop.lkup, by.x='PlotId', by.y="PlotName")
st.sum.lkup$mean_value<-st.sum.lkup$TotalValue/st.sum.lkup$TotalRecoverableVolume
pr.sum.lkup<-merge(pr.summ, wd.prop.lkup, by.x='PlotId', by.y="PlotName")
pr.sum.lkup<-subset(pr.sum.lkup, IsRecoverable == 1) #Remove non recoverable products such as break, top, stump

#st.sum.lkup$breed<-as.factor(st.sum.lkup$breed)


#Means by breed
#write.csv(st.sum.lkup, 'df.csv', row.names = FALSE)
mean.val_brd<- group_by(st.sum.lkup, breed) %>%  
  summarise(mean_value = mean(mean_value), sum = sum(mean_value), n = length(mean_value))

grouped<- mean.val_brd%>% group_by(breed) %>%
  summarise (mean = mean(mean_value), sum=sum(mean_value), n=length(mean_value))

                                                                  
# Boxplot of value by breed with stocking as a facet - Strategy 1 (No Wood properties)
st.sum.lkup_st1<-subset(st.sum.lkup, YieldRequest == 'Strategy1')
ggplot(st.sum.lkup_st1, aes(as.factor(breed), TotalValue)) + 
  geom_boxplot() +
  theme_light () + 
  labs(x="Breed", y="Total Value ($)") +
  facet_grid(. ~ sph.treatment)

# Boxplot of mean value by breed with stocking as a facet - Strategy 1 (No Wood properties)
st.sum.lkup_st1<-subset(st.sum.lkup, YieldRequest == 'Strategy1')
ggplot(st.sum.lkup_st1, aes(as.factor(breed), mean_value)) + 
  geom_boxplot() +
  theme_light () + 
  labs(x="Breed", y="Mean Value ($/m3)") +
  facet_grid(. ~ sph.treatment)

# Boxplot of mean value by breed with stocking as a facet - Strategy 2 (Wood properties)
st.sum.lkup_st2<-subset(st.sum.lkup, YieldRequest == 'Strategy2')
ggplot(st.sum.lkup_st2, aes(as.factor(breed), mean_value)) + 
  geom_boxplot() +
  theme_light () + 
  labs(x="Breed", y="Mean Value ($/m3)") +
  facet_grid(. ~ sph.treatment)

# Boxplot of value by breed with stocking as a facet - Strategy 2 (Wood properties)
st.sum.lkup_st2<-subset(st.sum.lkup, YieldRequest == 'Strategy2')
ggplot(st.sum.lkup_st2, aes(as.factor(breed), TotalValue)) + 
  geom_boxplot() +
  theme_light () + 
  labs(x="Breed", y="Total Value ($)") +
  facet_grid(. ~ sph.treatment)

# Boxplot of TRV by breed with stocking as a facet - Strategy 2 (Wood properties)
st.sum.lkup_st2<-subset(st.sum.lkup, YieldRequest == 'Strategy2')
ggplot(st.sum.lkup_st2, aes(as.factor(breed), TotalRecoverableVolume)) + 
  geom_boxplot() +
  theme_light () + 
  labs(x="Breed", y="Total Volume") +
  facet_grid(. ~ sph.treatment)

# Boxplot of value by breed with strategy as a facet
ggplot(st.sum.lkup, aes(as.factor(breed), TotalValue)) + 
  geom_boxplot() +
  theme_light () + 
  labs(x="Breed", y="Total Value ($)") +
  facet_grid(. ~ YieldRequest)

# Boxplot of TRV by breed with strategy as a facet
ggplot(st.sum.lkup, aes(as.factor(breed), TotalRecoverableVolume)) + 
  geom_boxplot() +
  theme_light () + 
  labs(x="Breed", y="Total Volume") +
  facet_grid(. ~ YieldRequest)


# Summarise product volumes by  yield requst, breed and treatment 
x<- pr.sum.lkup %>% group_by(YieldRequest, breed, sph.treatment, GradeName) %>% 
  summarise(Volume = mean(Volume), Volume_se = mean(VolumeStdError))

x2<-subset(x, YieldRequest == 'Strategy2')

ggplot(x2, aes(x=breed, y=Volume, fill=GradeName)) + 
  geom_bar(stat="identity") +
  facet_grid(. ~ sph.treatment) +
  scale_color_hue()


# Fit an ANOVA to test for the effect of seedlot on mean value

# Subset the dataset to only the strategy 2
st.sum.lkup2<- subset(st.sum.lkup, YieldRequest == "Strategy2")
st.sum.lkup2$sph.treatment<-as.factor(st.sum.lkup2$sph.treatment)

#my.lm<-lm(mean_value ~ breed * sph.treatment, data=st.sum.lkup2)
#my.anova<-anova(my.lm)
#plot(my.lm)
#plot(my.anova)

my.aov<-aov(mean_value ~ breed * sph.treatment, data=st.sum.lkup2)
#print(my.aov)
summary(my.aov)
par(mfrow=c(2,2))
plot(my.aov)
dev.off()

# Subset the dataset to only the strategy 1
st.sum.lkup1<- subset(st.sum.lkup, YieldRequest == "Strategy1")
st.sum.lkup1$sph.treatment<-as.factor(st.sum.lkup1$sph.treatment)

my.lm<-lm(mean_value ~ breed * sph.treatment, data=st.sum.lkup1)
my.anova<-anova(my.lm)

print(my.anova)
















