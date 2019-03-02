setwd("C:/Users/Paul/Desktop/DSB Workshop")

####  pgSQL Interface  ####
## For data stored in a database, you'll need to extract it first.  ##
## R has a variety of drivers for connecting with different Db types. ##

library(RSQLite)  ## package for configuring internal SQLite engine in R
library(RPostgreSQL)  ## package to connect with pgSQL from R
library(DBI)  ## package to establish a database interface

drv <- dbDriver("PostgreSQL")
localdb <- dbConnect(drv, dbname= 'DSBworkshop',
                     host= 'localhost',
                     port = 5432,
                     user = 'postgres',
                     password= 'ENTER_YOUR_pgSQL_PASSWORD_HERE')

dbListTables(localdb) ## list of tables for the connected database
GTT_10 <- dbSendQuery(localdb,"SELECT * FROM gtt2_tbl LIMIT (10)") ## SQL Query
GTT_ds10 <- fetch(GTT_10, n=-1)  ## 'fetch' will instantiate the query results
#View(GTT_ds10)
dbDisconnect(localdb) ## disconnect from local database
detach(package:RPostgreSQL)  ## remove RPostgreSQL package from active environment

####  Part 0:  Reading in the Data  ####
## Common way to reading data into R is with 'read.csv' function ##
Dems <- read.csv("BLA_DiabDems.csv")
LabsGTT <- read.csv("BLA_DiabGTT.csv")
LabsInit <- read.csv("BLA_DiabLabs.csv")
LabsPost <- read.csv("BLA_DiabLabsPost.csv")

#View(Dems) ## spreadsheet View of the data frame
names(Dems) ## display variable names in the data frame
levels(Dems$Coverage) ## display levels within a variable

head(LabsInit)  ## first 6 results
tail(LabsPost, n = 10)  ## last 10 results

## Review column names for the two similar data sets ##
names(LabsInit)
names(LabsPost)  

## Use 'colnames' function to rename columns for LabsPost  - will help with merging, analysis ##
colnames(LabsPost) <- c("PatientID","Collected","HbA1c","GlucF",
                        "BMI","ESR","VitD","GlucR")

#names(LabsPost)

## Data can also be read into R from Excel directly with 'readxl' package ##
library(readxl)
x_Dems <- read_xlsx("BLA_Labs.xlsx") ## Default reads in first worksheet

x_LabsInit <- read_xlsx("BLA_Labs.xlsx", 
                        sheet = "Labs (Initial)")  ## Specify sheet from Excel file ##

x_LabsPost <- read_xlsx("BLA_Labs.xlsx", 
                        sheet = "Labs (Final)", 
                        cell_rows(1:750), col_names = TRUE)  ## Limit row input ##

####  Part I:  Data Management, Cleaning #####
## Generating summary stats often requires some pre-processing of data ##
## (1) combine data sets, (2) qualify data (age groups, criticals) ##

##  Left Join: Method #1 -- Using 'sqldf' package, SQLite syntax, to merge data sets ##
library(sqldf)
ptLabsInit <- sqldf("SELECT *
                     FROM Dems 
                     LEFT JOIN LabsInit
                     ON Dems.PTID = LabsInit.PatientID")
#View(ptLabsInit) 

##  Left Join: Method #2 -- Using 'dplyr' package, left_join() function and pipes ##
library(dplyr)
ptLabsPost <- Dems %>%
                  left_join(LabsPost, by= c("PTID"="PatientID"))
#View(ptLabsPost)

## Incomplete Records: Method #1 -- SQL report of records missing "Collected" ##
## References ptLabsInit data set
missingLabs <- sqldf("SELECT * FROM ptLabsInit
                  WHERE Collected IS NULL")
#View(missingLabs)

## Incomplete Records: Method #2 -- R script to create report of records missing "Collected" ##
## References ptLabsPost data set
missingLabsPost <- ptLabsPost[is.na(ptLabsPost$Collected),]
#View(missingLabsPost)

## Can eliminate entire row if data is missing for specific variable ##
## Exclamation mark (!) instructs R to do the "anti" of subsequent function call ##
ptLabsInit_a <- ptLabsInit[!is.na(ptLabsInit$Collected),]
#View(ptLabsInit_a)

## Can also eliminate rows with *any* missing data using case-wise deletion ##
ptLabsInit <- na.omit(ptLabsInit)
#View(ptLabsInit)

## Can also remove rows missing data using pipes (require 'dplyr' package) ##
ptLabsPost <- ptLabsPost %>%
                na.omit()
#View(ptLabsPost)

## Glucose Tolerance Tests (0 & 2 hr) ##
## For date calculations, should standardize dates to ISO date format (yyyy-mm-dd) ##
## very useful package for dealing with date/time data types, part of Tidyverse ##

library(lubridate) 
## "DiabDOB" variable in incorrect format (mm/dd/yyyy) ##
Dems$DiabDOB <- mdy(Dems$DiabDOB)
## "CollectDate" variable also in incorrect format (mm-dd-yyyy) ##
LabsGTT$CollectDate <- mdy(LabsGTT$CollectDate)
## This time, we will use an inner join to merge the data, also removes missing labs ##

## Can use inner join instead of left-join, automatically omits NA's ##
library(dplyr)
ptLabsGTT <- LabsGTT %>%
              inner_join(Dems, by = "PTID") %>%
              rename("Collected"="CollectDate") ## rename for consistency sake 




## Create an Age variable from merged data using 'lubridate' functions ##
ptLabsGTT$Age <- round(interval(ptLabsGTT$DiabDOB,ptLabsGTT$Collected) ## calculate interval
                       /years(1), ## parse interval into years
                       digits = 1)  ## round-off years to 1 decimal

## Let's create variable for screening GTT results - diabetes Dx if either glucose is elevated  ##
## Using 'ifelse()' allows for new values to be assigned conditionally ##
## Specify logical OR with '|' in formula ##

ptLabsGTT$GTTDx <- if_else(ptLabsGTT$Gluc0h >= 126|ptLabsGTT$Gluc2h >= 200,"Diab","Norm")

## Processing the Pre/Post Treatment labs ##
## Merge the rows for the Pre/Post Labs and an identifier column ##
library(dplyr)
Labs <- bind_rows(list(Initial=LabsInit, Post=LabsPost), .id= 'Treatment') ## "Treatment" identifies source table
#View(Labs)

## Assign interpretive values for the labs before additional processing ##
## ESR requires patient gender for interpretation, so save this one for later ##
Labs$HbA1c_Qual <- cut(Labs$HbA1c,
                          breaks = c(-Inf,4.5,6.0,Inf),
                          labels = c("Norm","Risk","Diab"))

Labs$BMI_Qual <- cut(Labs$BMI,
                        breaks = c(-Inf,18.5,25,30,Inf),
                        labels = c("UnderWt","Normal","OverWt","Obese"))

Labs$GlucF_Qual <- ifelse(Labs$GlucF >= 126, "Diab","Norm")  ## 'ifelse()' for binomial ##
Labs$GlucR_Qual <- ifelse(Labs$GlucR >= 200, "Diab","Norm")  ## 'ifelse()' for binomial ##

## Vitamin D - very rare for it to be toxic, so only concerned about low levels ##
## Quick plot in R to check all values first ##
hist(Labs$VitD)
Labs$VitD_Qual <- ifelse(Labs$VitD < 20, "Low","Norm")  ## 'ifelse()' for binomial ##
#View(ptLabs)

## Convert the shape of the data from "long" to "wide", merge with Demo table ##
library(tidyr)
ptLabs <- Labs %>%
              gather(lab_name, lab_value, c(3:14)) %>%  # gather dates, labs into one long column ##
              unite(Lab, Treatment, lab_name, sep="_") %>% # merge informative columns #
              spread(Lab, lab_value) %>%  # spread the long from long to wide format #
              inner_join(Dems,by = c("PatientID"="PTID")) %>% # join with Demographics table
              rename("PTID"="PatientID") # update naming convention for readability
## Cryptic error "attributes not identical... will be dropped" ##
## Apparently ths is known artifact of 'tidyr' processing, safe to ignore ##

## Data Check ##
View(ptLabs)
str(ptLabs)
## Somehow, numeric data type for lab values got lost... watch out for this when data munging ##
## 'sapply' function can apply another function over a data vector ##
ptLabs[,c(2,5,6,8,10,12,14,17,18,20,22,24)] <- sapply(ptLabs[,c(2,5,6,8,10,12,14,17,18,20,22,24)], as.numeric)  

## Create an Age variable for this data set ##
library(lubridate)
ptLabs$Age <- round(interval(ptLabs$DiabDOB,ptLabs$Initial_Collected) ## calculate interval from init ##
                       /years(1), ## parse interval into years
                       digits = 1)  ## round-off years to 1 decimal

## Let's make some age bins : 18-25, 25-40, 40-55, 55-70, 70+ (5 groups) ##
## Can do this with base R 'cut()' - highly readable ##
ptLabs$AgeQ <- cut(ptLabs$Age,
                       breaks = c(-Inf,25,40,55,70,Inf),
                       labels = c("18-25","25-40","40-55","55-70","70+"))

## Combine 'ifelse()', 'OR', and 'AND' logic for ESR ##
## Have to do this twice now because "Pre" and "Post" Treatments are now new columns ##
ptLabs$Initial_ESRq <- ifelse((ptLabs$Initial_ESR >= 22 & ptLabs$DiabSex == 'F') |
                                (ptLabs$Initial_ESR >= 29 & ptLabs$DiabSex == 'M'),
                            "Elev","Norm")

ptLabs$Post_ESRq <- ifelse((ptLabs$Post_ESR >= 22 & ptLabs$DiabSex == 'F')|
                                (ptLabs$Post_ESR >= 29 & ptLabs$DiabSex == 'M'),
                              "Elev","Norm")


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
####  Part IV:  T-tests, ANOVA Modeling  ####

## Diagnostics for normal distributions - assumed for parametric tests ##
shapiro.test(ptLabs$Initial_GlucR)
qqnorm(ptLabs$Initial_GlucR)
qqline(ptLabs$Initial_GlucR, col="blue")

# Welch's t-test #
t.test(ptLabs$Initial_GlucR ~ ptLabs$DiabSex)
# lapply to do check a variety of two-sample t-tests at once #
lapply(ptLabs[,c("Initial_GlucR","Initial_GlucF","Initial_HbA1c",
                 "Initial_VitD","Initial_ESR","Initial_BMI")],
       function(x) t.test(x~ptLabs$DiabSex))

# Paired t-test  - compare Initial vs Post random glucose #
t.test(ptLabs$Initial_GlucR,ptLabs$Post_GlucR, paired = TRUE)

# Univariate ANOVA #
library(car)
## set up linear model
model1 <- lm(ptLabs$Initial_GlucR ~ ptLabs$Coverage)
Anova(model1, type="III")  # model statistics
summary(model1) # coefficients, model R-square

# Multivariable ANOVA #
table(ptLabs$Coverage, ptLabs$DiabSex)
model2 <- lm(ptLabs$Initial_GlucR ~ ptLabs$Coverage + ptLabs$DiabSex)
Anova(model2) # ANOVA output 
summary(model2)  # coefficients, model R-square

# Check other cell-size combinations before modeling further ##
table(ptLabs$Coverage,ptLabs$AgeQ) ## lots of issues here
table(ptLabs$AgeQ,ptLabs$DiabSex) ## pretty good, though unbalanced
table(ptLabs$AgeQ,ptLabs$Initial_HbA1c_Qual)  ## some issues, rebin?

# Multivariable ANOVA with covariate #
model3 <- lm(ptLabs$Initial_GlucR ~ ptLabs$DiabSex + ptLabs$AgeQ +
             ptLabs$Initial_HbA1c)
Anova(model3)
summary(model3)

# Try out HbA1c_qual anyway, because rabbit holes are fun... #
model3a <- lm(ptLabs$Initial_GlucR ~ ptLabs$DiabSex + ptLabs$AgeQ +
               ptLabs$Initial_HbA1c_Qual)
Anova(model3a)
summary(model3a)

library(ggplot2)

ggplot(data=ptLabs, aes(x=Initial_HbA1c_Qual, y=Initial_GlucR, fill=Initial_HbA1c_Qual))+
  geom_bar(stat="summary")+
  geom_errorbar(stat="summary", fun.data=mean_se)+
  geom_point(position = "jitter", shape=24, color="black")

library(agricolae)
TukeyTests <- HSD.test(model3a,"ptLabs$Initial_HbA1c_Qual")
TukeyTests


## Basic t-test power recipe, assumes 5 reps/group ##

power.t.test(
  n=10,
  delta = 4,
  sd = 3,
  sig.level = 0.05,
  type="two.sample",
  alternative="two.sided"
)

## Paired t-test recipe, assumes 5 reps/group, better power ##

power.t.test(
  n=10,
  delta = 4,
  sd = 3,
  sig.level = 0.05,
  type="paired",
  alternative="two.sided"
)



## This ANOVA power recipe assumes balanced design, 250 reps/group ##
power.anova.test(
  groups = 3,
  n = 250,
  between.var = 3.5,
  within.var = 120.2,
  sig.level = 0.05)

## Alternately, can exclude N and specify desired power instead ##
power.anova.test(
  groups = 3,
  power = 0.95,
  between.var = 3.5,
  within.var = 120.2,
  sig.level = 0.05)

#### Part V:  Correlations - Simple & Matrix ####
# Base R is fine for single pair correlation analysis #
cor.test(ptLabs$Initial_ESR, ptLabs$Initial_VitD) # returns r and p-value
plot(ptLabs$Initial_VitD~ptLabs$Initial_ESR)
abline(lm(ptLabs$Initial_VitD~ptLabs$Initial_ESR), col="red")

# For more variables, better to use more specialized packages #
# Only numeric data types allowed (integer, float, rank) for correlations #
library(agricolae) 
library(dplyr)
#View(ptLabs)
ptLabsPost_qnt <- ptLabs %>%
                    select(14,17,18,20,22,24,31)
                    
#View(ptLabsPost_qnt)
correlation(ptLabsPost_qnt) # 'agricolae' package

## Let's make a heatmap of correlation coefficients ##
## Spiffy corrplot package##
library(corrplot)
ptLabs_res <- cor.mtest(ptLabsPost_qnt, conf.level=0.95)
ptLabs_cor <- cor(ptLabsPost_qnt)
corrplot(ptLabs_cor, p.mat = ptLabs_res$p, tl.srt = 30, tl.col = "black", 
         insig = "p-value", tl.pos="ld", tl.cex = 0.8,
         order = "original", method="shade",type="lower",
         cl.pos = "r", cl.ratio = 0.3)

# can customize display with hierarchical clustering #
corrplot(ptLabs_cor, p.mat = ptLabs_res$p, tl.srt = 30, tl.col = "black", 
         insig = "p-value", tl.pos="ld", tl.cex = 0.8,
         order = "hclust", method="shade",type="lower",
         cl.pos = "r", cl.ratio = 0.3)


#### Miscellaneous Notes, Odds & Ends ####

## Subsetting Data with Tidyverse - dplyr 'filter()' function  ##
PtLabs_F <- filter(ptLabs, ptLabs$DiabSex == 'F')
PtLabs_M <- filter(ptLabs, ptLabs$DiabSex == 'M')

##  Zoom in on specific model components ##
model <- lm(ptLabsInit$HbA1c~ptLabsInit$VitD)
summary(model) ## summary of linear model
model$coefficients  ## zoom in on specific model components

## ggplot barcharts with SE - alternate syntax, no error message  ##
ggplot(data=ptLabs, aes(x=Coverage,y=Initial_HbA1c,fill=Coverage))+
  stat_summary(fun.y = mean, geom = "bar")+
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  scale_fill_brewer(palette = "BuPu")  ## another built-in color palettes
