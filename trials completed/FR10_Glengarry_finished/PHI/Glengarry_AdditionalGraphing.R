t<-read.csv("Q:\\Forest Systems\\Projects\\FR_10_Glengarry\\PHI\\df.csv")
df<-subset(t, YieldRequest == 'Strategy1')
df2<-subset(t, YieldRequest == 'Strategy2')

avg.log.price<-96

library(XLConnect)
library(dplyr)
library(ggplot2)
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

#External Only

#Grouped by breed mean value
grouped<- df%>% group_by(breed) %>%
  summarise (mean = mean(mean_value), sum=sum(mean_value), n=length(mean_value), sd=sd(mean_value), se=se(mean_value))

# Define the top and bottom of the errorbars
limits <- aes(ymax = mean + se, ymin=mean - se)

setwd('Q:\\Forest Systems\\Projects\\FR_10_Glengarry\\PHI\\')
png('Mean_value_breed_external.png', w = 10, h=10, units="cm", res=300)
ggplot(grouped, aes(y=mean, x=breed)) +
  geom_point() +
  geom_errorbar(limits, width=0.2) +
  labs(x="Breed", y=expression(paste('Mean Value ($ / ', m^{3}, ')'))) +
  theme_light()
dev.off()


#Grouped by breed and sph mean value
grouped3<- df%>% group_by(breed, sph.treatment) %>%
  summarise (mean = mean(mean_value), sum=sum(mean_value), n=length(mean_value), sd=sd(mean_value), se=se(mean_value))

limits <- aes(ymax = mean + se, ymin=mean - se)
png('Mean_value_breed__sph_external.png', w = 17, h=10, units="cm", res=300)
ggplot(grouped3, aes(y=mean, x=breed)) +
  geom_point() +
  geom_errorbar(limits, width=0.2) +
  labs(x="Breed", y=expression(paste('Mean Value ($ / ', m^{3}, ')'))) +
  facet_grid(. ~ sph.treatment) +
  theme_light() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
dev.off()

#Grouped by breed
grouped2<- df%>% group_by(breed) %>%
  summarise (mean = mean(TotalValue), sum=sum(TotalValue), n=length(TotalValue), sd=sd(TotalValue), se=se(TotalValue))

png('Total_value_breed_external.png', w = 10, h=10, units="cm", res=300)
ggplot(grouped2, aes(y=mean, x=breed)) +
  geom_point() +
  geom_errorbar(limits, width=0.2) +
  labs(x="Breed", y='Total Standing Value ($)') +
  theme_light()
dev.off()

#Grouped by breed and sph total value
grouped4<- df%>% group_by(breed, sph.treatment) %>%
  summarise (mean = mean(TotalValue), sum=sum(TotalValue), n=length(TotalValue), sd=sd(TotalValue), se=se(TotalValue))
limits <- aes(ymax = mean + se, ymin=mean - se)

png('Total_value_breed__sph_external.png', w = 17, h=10, units="cm", res=300)
ggplot(grouped4, aes(y=mean, x=breed)) +
  geom_point() +
  geom_errorbar(limits, width=0.2) +
  labs(x="Breed", y='Total Standing Value ($)') +
  theme_light() +
  facet_grid(. ~ sph.treatment) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
dev.off()



#################################################
# 
# Now with the internal wood properties
#
#################################################

#Grouped by breed mean value
grouped<- df2%>% group_by(breed) %>%
  summarise (mean = mean(mean_value), sum=sum(mean_value), n=length(mean_value), sd=sd(mean_value), se=se(mean_value))

# Define the top and bottom of the errorbars
limits <- aes(ymax = mean + se, ymin=mean - se)

setwd('Q:\\Forest Systems\\Projects\\FR_10_Glengarry\\PHI\\')
png('Mean_value_breed_internal.png', w = 10, h=10, units="cm", res=300)
ggplot(grouped, aes(y=mean, x=breed)) +
  geom_point() +
  geom_errorbar(limits, width=0.2) +
  labs(x="Breed", y=expression(paste('Mean Value ($ / ', m^{3}, ')'))) +
  theme_light()
dev.off()


#Grouped by breed and sph mean value
grouped3<- df2%>% group_by(breed, sph.treatment) %>%
  summarise (mean = mean(mean_value), sum=sum(mean_value), n=length(mean_value), sd=sd(mean_value), se=se(mean_value))

limits <- aes(ymax = mean + se, ymin=mean - se)
png('Mean_value_breed__sph_internal.png', w = 17, h=10, units="cm", res=300)
ggplot(grouped3, aes(y=mean, x=breed)) +
  geom_point() +
  geom_errorbar(limits, width=0.2) +
  labs(x="Breed", y=expression(paste('Mean Value ($ / ', m^{3}, ')'))) +
  facet_grid(. ~ sph.treatment) +
  theme_light() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
dev.off()

#Grouped by breed
grouped2<- df2%>% group_by(breed) %>%
  summarise (mean = mean(TotalValue), sum=sum(TotalValue), n=length(TotalValue), sd=sd(TotalValue), se=se(TotalValue))

png('Total_value_breed_internal.png', w = 10, h=10, units="cm", res=300)
ggplot(grouped2, aes(y=mean, x=breed)) +
  geom_point() +
  geom_errorbar(limits, width=0.2) +
  labs(x="Breed", y='Total Standing Value ($)') +
  theme_light()
dev.off()

#Grouped by breed and sph total value
grouped4<- df2%>% group_by(breed, sph.treatment) %>%
  summarise (mean = mean(TotalValue), sum=sum(TotalValue), n=length(TotalValue), sd=sd(TotalValue), se=se(TotalValue))
limits <- aes(ymax = mean + se, ymin=mean - se)

png('Total_value_breed__sph_internal.png', w = 17, h=10, units="cm", res=300)
ggplot(grouped4, aes(y=mean, x=breed)) +
  geom_point() +
  geom_errorbar(limits, width=0.2) +
  labs(x="Breed", y='Total Standing Value ($)') +
  theme_light() +
  facet_grid(. ~ sph.treatment) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
dev.off()



t2<-read.csv("Q:\\Forest Systems\\Projects\\FR_10_Glengarry\\PHI\\t1.csv")
# Form a dataframe for graphing
t1_plt<- t2 %>% group_by(Seedlot, sph.treatment) %>% summarise (TotalValue = mean(TotalValue),
                                                                TotalValue_se = se(TotalValue),
                                                                PieceSize = mean(PieceSize),
                                                                PieceSize_se = se(PieceSize),
                                                                MeanValue = mean(MeanValue),
                                                                MeanValue_se = se(MeanValue))


