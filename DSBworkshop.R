setwd("C:/.../DSB Workshop")

####  pgSQL Interface  ####
## For data stored in a database, you'll need to extract it first.  ##
## R has a variety of drivers for connecting with different Db types. ##
## Comment/un-comment out entire sections with Ctrl-Shift + C ##
# library(RSQLite)  ## package for configuring internal SQLite engine in R
# library(RPostgreSQL)  ## package to connect with pgSQL from R
# library(DBI)  ## package to establish a database interface
# 
# drv <- dbDriver("PostgreSQL")
# localdb <- dbConnect(drv, dbname= 'DSBworkshop',
#                      host= 'localhost',
#                      port = 5432,
#                      user = 'postgres',
#                      password= 'BattleStar*84')
# 
# dbListTables(localdb) ## list of tables for the connected database
# GTT_10 <- dbSendQuery(localdb,"SELECT * FROM gtt2_tbl LIMIT (10)") ## SQL Query
# GTT_ds10 <- fetch(GTT_10, n=-1)  ## 'fetch' will instantiate the query results
# #View(GTT_ds10)
# dbDisconnect(localdb) ## disconnect from local database
# detach(package:RPostgreSQL)  ## remove RPostgreSQL package from active environment
# 
####  Part 0:  Reading in the Data  ####
## Common way to reading data into R is with 'read.csv' function ##
Dems <- read.csv("BLA_DiabDems.csv")
Dems_DNA <- Dems[,c("PTID","DNA_ID")]
LabsGTT <- read.csv("BLA_DiabGTT.csv")
LabsInit <- read.csv("BLA_DiabLabs.csv")
LabsPost <- read.csv("BLA_DiabLabsPost.csv")

#View(Dems) ## spreadsheet View of the data frame
#names(Dems) ## display variable names in the data frame
#levels(Dems$Coverage) ## display levels within a variable

head(LabsInit)  ## first 6 results
tail(LabsPost, n = 10)  ## last 10 results
#View(LabsInit)
# # Review column names for the two similar data sets ##
# names(LabsInit)
# names(LabsPost)

## Use 'colnames' function to rename columns for LabsPost  - will help with merging, analysis ##
colnames(LabsPost) <- c("PatientID","Collected","HbA1c","GlucF",
                        "BMI","ESR","VitD","GlucR")

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
str(Dems$PTID)
str(LabsPost$PatientID)
ptLabsPost <- Dems %>%
                  left_join(LabsPost, by= c("PTID"="PatientID"))
#View(ptLabsPost)  ## some patients with multiple labs

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
                       digits = 1) ## round-off years to 1 decimal

## Let's create variable for screening GTT results - diabetes Dx if either glucose is elevated  ##
## Using 'ifelse()' allows for new values to be assigned conditionally ##
## Specify logical OR with '|' in formula ##

ptLabsGTT$GTTDx <- if_else(ptLabsGTT$Gluc0h >= 126|ptLabsGTT$Gluc2h >= 200,"Diab","Norm")

## Processing the Pre/Post Treatment labs ##
## Merge the rows for the Pre/Post Labs and add an identifier column ##
library(dplyr)
Labs <- bind_rows(list(Initial=LabsInit, Post=LabsPost), .id= 'Treatment') ## "Treatment" identifies source table
#View(Labs)

## Assign interpretive values for the labs before additional processing ##
## ESR requires patient gender for interpretation, so save this one for later ##
Labs$HbA1c_Qual <- cut(Labs$HbA1c,
                          breaks = c(-Inf,4.5,6.0,Inf),
                          labels = c("Norm","Risk","Diab"))

Labs$HbA1c_QualA <- dplyr::if_else(Labs$HbA1c < 4.5, true="Norm",
                      if_else(Labs$HbA1c >= 4.5 & Labs$HbA1c <6.0,true="Risk",false="Diab"))

Labs$HbA1c_QualB <- base::ifelse(Labs$HbA1c <4.5, yes = "Norm",
                           ifelse(Labs$HbA1c >= 4.5 & Labs$HbA1c <6.0, yes="Risk", no="Diab"))

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
#View(ptLabs)
str(ptLabs)
## Somehow, numeric data type for lab values got lost... watch out for this when data munging ##
## 'sapply' function can apply another function over a data vector ##
ptLabs[,c(4,7,8,10,11,15,16,19,20,22,23,27)] <- sapply(ptLabs[,c(4,7,8,10,11,15,16,19,20,22,23,27)], as.numeric)  

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

View(ptLabs)

#### Miscellaneous Notes, Odds & Ends ####

## Subsetting Data with Tidyverse - dplyr 'filter()' function  ##
PtLabs_F <- filter(ptLabs, ptLabs$DiabSex == 'F')
PtLabs_M <- filter(ptLabs, ptLabs$DiabSex == 'M')

##  Zoom in on specific model components ##
model <- lm(ptLabsInit$HbA1c~ptLabsInit$VitD)
summary(model) ## summary of linear model
model$coefficients  ## zoom in on specific model components

## ggplot barcharts with SE - alternate syntax, no 'geom_', no error message  ##
library(ggplot2)
ggplot(data=ptLabs, aes(x=Coverage,y=Initial_HbA1c,fill=Coverage))+
  stat_summary(fun.y = mean, geom = "bar")+
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  scale_fill_brewer(palette = "BuPu")  ## another built-in color palettes
