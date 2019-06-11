source("DSBworkshop.R")  ## The source statement will call in the specified script

####  Part II:  Descriptive Statistics ####
## base R functions - can use "attach" to specify source data set ##
attach(ptLabsInit)
mean(ESR) # mean or average
sd(ESR) # standard deviation
min(ESR) # minimum
max(ESR) # maximum 
length(ESR) # N
#str(ptLabsInit)
## create customized function for SEM ##
sem <- function(x) sd(x)/sqrt(length(x))
sem(ESR) # standard error of mean

## can be more efficient to use 'psych' package for full reprot ##
## apply to either a single variable or entire data frame ##
library(psych)
describe(ESR) ## describe all records
describeBy(ESR, group = DiabSex) ## describe records by group
detach(ptLabsInit) ## remove 'attach' association
describeBy(ptLabsInit$ESR, group=ptLabsInit$DiabSex) ## equivalent syntax to above

## can also use 'dplyr' summarize() function - can customize variable names, limit output ##
library(dplyr)
ptLabsPost %>%
  summarize(N = n(), HgbA1c = round(mean(HbA1c), digits=2), min = min(HbA1c),
            max = max(HbA1c), SD = round(sd(HbA1c),digits=2),
            SEM= round(sd(HbA1c)/sqrt(n()), digits = 2))  ## SEM formula ##

# break it up by group #
ptLabsPost %>%
  group_by(DiabSex) %>%
  summarize(N = n(), HA1 = round(mean(HbA1c),digits=2), min = min(HbA1c),
            max = max(HbA1c), SD = round(sd(HbA1c),digits=2),
            SE= round(sd(HbA1c)/sqrt(n()),digits=2)) ## creates a "tibble" data object

####  Part III:  Data Viz w/ 'ggplot2' ####
library(ggplot2)
## Lets do frequency bar plots of BMI_Qual ##
ggplot(Labs,aes(x=HbA1c_Qual))+
  geom_bar(stat = "count")

## Let's color the counts by Treatment ##
ggplot(Labs,aes(x=HbA1c_Qual, fill=Treatment))+
  geom_bar(stat = "count")

## Let's rearrange this a bit so colors aren't stacked ##
ggplot(Labs,aes(x=HbA1c_Qual, fill=Treatment))+
  geom_bar(stat = "count", position = "dodge")
View(ptLabs)

## Bar Plots with Mean +/- SE ##
## Let's plot out the actual Initial HbA1c mean values by Coverage ##
ggplot(ptLabs, aes(x=Coverage, y=Initial_HbA1c, fill=Coverage))+
  geom_bar(stat="summary")+
  geom_errorbar(stat="summary")

# Update Colors, Labels #
ggplot(ptLabs, aes(x=Coverage, y=Initial_HbA1c, fill=Coverage))+
  geom_bar(stat="summary")+
  geom_errorbar(stat="summary")+
  scale_fill_brewer(palette = "YlOrRd")+ # predefined color pallette
  xlab("Insurance Coverage")+
  ylab("HbA1c % (Initial)")


## Scatter Plots -- BMI by Age ##
# the base R way to do this is a little faster at first but...#
plot(ptLabs$Initial_BMI~ptLabs$Initial_GlucF)
abline(lm(ptLabs$Initial_BMI~ptLabs$Initial_GlucF), col="red")

# ggplot2 is much more customizable #
ggplot(ptLabs, aes(x=Initial_BMI, y=Initial_GlucF))+
  geom_point()

## Let's add in patient Sex, and manually assign colors ##
ggplot(ptLabs, aes(x=Initial_BMI, y=Initial_GlucF,color=DiabSex))+
  geom_point()+
  scale_color_manual(values =c("firebrick1","royalblue"))

## Let's add in a continous variable instead, specify a color range, new shape ##
ggplot(ptLabs, aes(x=Initial_BMI, y=Initial_GlucF, color=Age))+
  geom_point(shape=18, size=2)+
  scale_color_continuous(low="forestgreen",high="yellow")

##  Let's divide into side-by-side panels, comparing HbA1c results ##
ggplot(ptLabs, aes(x=Initial_BMI, y=Initial_GlucF,color=DiabSex))+
  geom_point()+
  scale_color_manual(values =c("firebrick1","royalblue"))+
  facet_wrap(~Coverage)

##  Let's add in some basic trend-lines for each panel ##
ggplot(ptLabs, aes(x=Initial_BMI, y=Initial_GlucF,color=DiabSex))+
  geom_point()+
  scale_color_manual(values =c("firebrick1","royalblue"))+
  facet_wrap(~Coverage)+
  geom_smooth(method=lm, se=FALSE) ## se controls CI display

## Update labels, legend name, extrapolate ##
ggplot(ptLabs, aes(x=Initial_BMI, y=Initial_GlucF, color=DiabSex))+
  geom_point()+
  scale_color_manual(values =c("firebrick1","royalblue"), name="Sex")+
  facet_wrap(~Coverage)+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  xlab("Initial BMI")+
  ylab("Fasting Glucose (Initial) mg/dL")

##  Now display CI's, add a Title, customize Title display ##
ggplot(ptLabs, aes(x=Initial_BMI, y=Initial_GlucF,color=DiabSex))+
  geom_point()+
  scale_color_manual(values =c("red","blue"), name="Sex")+
  facet_wrap(~Coverage)+
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE, alpha=0.3)+ # alpha defines transparency
  xlab("Initial BMI")+
  ylab("Fasting Glucose (Initial) mg/dL")+
  ggtitle("BMI vs Fasting Glucose\n(Initial)")+
  theme(plot.title = element_text(hjust = 0.5))

## Add some color to the confidence intervals ##
ggplot(ptLabs, aes(x=Initial_BMI, y=Initial_GlucF,color=DiabSex))+
  geom_point()+
  scale_color_manual(values =c("red","blue"), name="Sex")+
  facet_wrap(~Coverage)+
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE, aes(fill=DiabSex), alpha=0.3)+ # fill CI w/ color
  scale_fill_manual(values = c("red","blue"), name="Sex")+
  xlab("Initial BMI")+
  ylab("Fasting Glucose (Initial) mg/dL")+
  ggtitle("BMI vs Fasting Glucose\n(Initial)")+
  theme(plot.title = element_text(hjust = 0.5))
View(ptLabs)
