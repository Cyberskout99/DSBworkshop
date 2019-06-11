setwd("C:/.../DSB Workshop")
source("DSBworkshop.R")

####  Part VI:  Regular Expressions (RegEx) ####
# GREP =  'general regular expression parser' #
# RegEx is used for searching/manipulating string data, common across platforms #
# Can specify exact match, wildcards, and other parameters #
# Generalized RegEx syntax is valid for all the RegEx functions in R #
# Reserved Characters: {} [] () ^ $ . | * + ? \ - #
# Same shortcuts as in other flavors of RegEx (i.e., \w, \b, \B, \d)

# grep function #
grep(pattern = "GGGCCT", x=Dems_DNA$DNA_ID )  ## returns indices for exact matches
grep(pattern = "GGG.CCC", x=Dems_DNA$DNA_ID ) ## returns indices, allows wild card
grep(pattern = "GGGTTT{2}", x=Dems_DNA$DNA_ID) ## returns indices, checks for repeats

grep(pattern = "GGGTTT{2}", x=Dems_DNA$DNA_ID, value = TRUE) ## value option returns string 

# lgrep function #
grepl(pattern = "GGGCCT", x=Dems_DNA$DNA_ID) ## returns logical vector for entire vector

# regexpr function #
head(regexpr(pattern = "GGGCCT", text = Dems_DNA$DNA_ID), n=25) ## returns character position for match #
                                                    ## returns -1 if no match #
                                                    ## stops at first match #

# gregexpr function #
head(gregexpr(pattern = "GGGCC", text = Dems_DNA$DNA_ID), n=25) ## returns character position, length for matches #
                                                    ## returns -1 if no matches #
                                                    ## returns all matches for input vector #

# regexec function #
head(regexec(pattern = "GGG.T", text = Dems_DNA$DNA_ID), n=10) # returns character position, length of match
                                                    ## returns -1 if no matches #
                                                    ## returns all matches for input vector #

#### Part VIIa: Regression - Linear Models ####
library(dplyr)
ptLabs2 <- ptLabs %>%
  filter(!is.na(Initial_HbA1c) & !is.na(Initial_GlucF))  ## prune out the missing data
#View(ptLabs2)
## customized function to display regression equation on figure ##
qqnorm(ptLabs$Initial_HbA1c)
qqline(ptLabs$Initial_HbA1c)
shapiro.test(ptLabs$Initial_HbA1c)
library(ggpubr) ## allows publication-quality text cusotmization for ggplot2 graphics ##
lm_eqn <- function(ptLabs2) {
  model <- lm(Initial_HbA1c~Initial_GlucF, data=ptLabs2);
  eqn <- substitute(italic(Initial_HbA1c) == a + b %.% italic(Initial_GlucF)*","~~italic(r)^2~"="~r2,
                    list(a = format(coef(model)[1], digits = 2),
                         b = format(coef(model)[2], digits = 2),
                         r2 = format(summary(model)$r.squared, digits=3)))
  as.character(as.expression(eqn))
}

## scatter plot with regression line, confidence interval ##
ggplot(data=ptLabs2, aes(x=Initial_HbA1c, y=Initial_GlucF))+
  geom_point()+
  stat_smooth(method = lm, level=0.95, fullrange = TRUE,  alpha=0.2)+
  stat_cor(label.x = 5, label.y = 200)+
  stat_regline_equation(label.x = 5, label.y = 180)

ggplot(data=ptLabs2, aes(x=Initial_HbA1c, y=Initial_GlucF, color=Initial_BMI_Qual))+
  geom_point()+
  stat_smooth(method = lm, level=0.95, fullrange = TRUE,  alpha=0.2)+
  stat_cor(label.x = c(0.5,2.5,5), label.y = c(200,185,200))+
  stat_regline_equation(label.x = c(0.5,2.5,5), label.y = c(180,165,180))

#### Part VIIb: Logistic Regression ####
## Let's assume Diab & Risk are both of interest ##
table(ptLabs2$Initial_HbA1c_Qual, ptLabs2$Coverage, useNA="ifany")
library(dplyr)  ## recode() function allows quick re-naming of values within a variable
ptLabs2$Initial_HbA1c_Qual <-  recode(ptLabs2$Initial_HbA1c_Qual, 'Diab'=1, 'Risk'=1, 'Norm'=0)  # we'll consider "at risk" as abnormal

model2 <- glm(Initial_HbA1c_Qual~Coverage, data=ptLabs2, family = "binomial")
summary(model2)
expb <- exp(coef(model2))
print(expb)
intexp <- exp(confint(model2))
print(intexp)

## Forest Plot ##
library(forestplot)
row_names <- c("Govt vs \nCommercial",
               "Other vs \nCommercial",
               "Uninsured vs \nCommercial")

test_data <- data.frame(
  beta=c(2.2,1.5,1.2),
  low=c(1.3,0.7,0.7),
  high=c(3.7,3.1,2.0))

## forestplot ##
forestplot(row_names,
           test_data$beta,
           test_data$low,
           test_data$high,
           zero = 1,
           cex = 1.25,
           lineheight = "auto",
           xlab = "Initial HbA1c by Coverage",
           xlog = TRUE,
           xticks.digits = 0, ##SigFigs
           ci.vertices = TRUE,  ##Adds "T" to CI ends
           lwd.ci = 1.5,
           lty.ci = 1,
           clr.line = "black",
           clr.marker = "blue",
           boxsize = 0.2,
           txt_gp = fpTxtGp(xlab = gpar(cex=1.25),
                            ticks = gpar(cex=1))) ## overrides auto-assigned size

#### Part VIIc: Poisson Regression ####
## Remaining Initial Labs - Convert to binomial ##
ptLabs2$Initial_BMI_Qual <-  recode( 
  ptLabs2$Initial_BMI_Qual, 'OverWt'=1, 'Obese'=1, 'Normal'=0, 'UnderWt'=0)  
ptLabs2$Initial_GlucF_Qual <-  recode(
  ptLabs2$Initial_GlucF_Qual, 'Diab'=1, 'Norm'=0)
ptLabs2$Initial_DiabSev <- ptLabs2$Initial_BMI_Qual + ptLabs2$Initial_GlucF_Qual + ptLabs2$Initial_HbA1c_Qual

## Post Labs - Convert to binomial ##
ptLabs2$Post_HbA1c_Qual <-  recode(
  ptLabs2$Post_HbA1c_Qual, 'Diab'=1, 'Risk'=1, 'Norm'=0)  # we'll consider "at risk" as abnormal
ptLabs2$Post_BMI_Qual <-  recode(
  ptLabs2$Post_BMI_Qual, 'OverWt'=1, 'Obese'=1, 'Normal'=0, 'UnderWt'=0)  
ptLabs2$Post_GlucF_Qual <-  recode(
  ptLabs2$Post_GlucF_Qual, 'Diab'=1, 'Risk'=1, 'Norm'=0)
ptLabs2$Post_DiabSev <- ptLabs2$Post_BMI_Qual + ptLabs2$Post_GlucF_Qual + ptLabs2$Post_HbA1c_Qual

## Merge the columns for the Pre/Post labs and then transform wide-to-long ##

ptLabsPR <- cbind(ptLabs2[,c("PTID","Initial_DiabSev","Post_DiabSev")])
ptLabsPRl <- ptLabsPR %>%
              gather(key="Time", value="Severity", Initial_DiabSev, Post_DiabSev)

## Let's create a vector describing of initial diabetes severity ##
PR_model <- glm(Severity~Time, family="poisson",
                data= ptLabsPRl)
summary(PR_model)
exp(PR_model$coefficients) ## display estimate
exp(confint(PR_model)) ## display CI for the estimate

table(ptLabsPRl$Severity,ptLabsPRl$Time, useNA="ifany")
## overall, it looks like counts decreased; however.... also some missing data ##
## original data set had missing HgbA1c, R unable to calculate severity for _DiabSev variables ##

ptLabsPRl <- na.omit(ptLabsPRl)

## Let's visualize this with ggplot2 - make a quick table first ##
LabsTable <- as.data.frame(table(ptLabsPRl$Severity,ptLabsPRl$Time))
colnames(LabsTable) <- c("Severity","Stage","Freq")
## Update the treatment stages for the graph #
LabsTable$Stage <- recode(
  LabsTable$Stage, "Initial_DiabSev"="Initial","Post_DiabSev"="Post")

ggplot(data=LabsTable, aes(x=Severity, y=Freq))+
  geom_bar(stat="identity",position="dodge", aes(fill=Stage), color="black")+
  scale_fill_manual(values=c("yellow","royalblue"))+
  ggtitle("Disease Severity")+
  xlab("Total Abnormal Screens\n(BMI, HbA1c, or Fasting Glucose)")+
  ylab("Frequency")+
  theme(plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey"),panel.grid.minor = element_line(colour = "grey"))+
  guides(fill=guide_legend(title="Stage"))

####  Part VIIIa Grouping : Principal Compenents Analysis ####
LabsNums <- cbind(ptLabs2$Initial_BMI, ptLabs2$Initial_ESR, ptLabs2$Initial_GlucF, ptLabs2$Initial_GlucR,
                  ptLabs2$Initial_HbA1c, ptLabs2$Initial_VitD, ptLabs2$Age)
namevar <- c("BMI","ESR","GlucF","GlucR","HbA1c","VitD","Age")
colnames(LabsNums) <- namevar
LabsNums <- LabsNums[complete.cases(LabsNums),]
View(LabsNums)

## compute PCA ##
PCAs1 <- prcomp(LabsNums, scale=TRUE) ## apply scaling
PCAs1

library(factoextra)
get_eig(PCAs1) # report eigenvalues
fviz_eig(PCAs1) # display scree plot
fviz_pca_var(PCAs1,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

####  Part VIIIa Grouping : Correspondence Analysis for Categorical Data ####

# ptLabs2$Coverage <- factor(ptLabs2$Coverage)

## Generate Heatmap to get Idea of data ##
table(ptLabs2$Coverage, ptLabs2$Initial_BMI_Qual)  ## Check the Data
## Rebin the underweight patient (n=1) ##
library(dplyr)
ptLabs2$Initial_BMI_Qual <- recode(ptLabs2$Initial_BMI_Qual, 'UnderWt'='Normal')
ptLabs2$Initial_BMI_Qual <- droplevels(factor(ptLabs2$Initial_BMI_Qual))
BMICovMap <- as.data.frame(table(ptLabs2$Initial_BMI_Qual,ptLabs2$Coverage))
colnames(BMICovMap) <- c("BMI","Coverage","Freq")

library(ggplot2)
ggplot(data=BMICovMap, aes(x=Coverage,y=BMI))+
  geom_tile(aes(fill=Freq))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

##  Generate contingency table for Correspondence Analysis ##
table(ptLabs2$Initial_BMI_Qual)
BMICov <- xtabs(~Initial_BMI_Qual+Coverage, data=ptLabs2)
print(BMICov)  ## tabular breakdown of ESR by Coverage
chisq.test(BMICov)  ## Chi-square statistics not appear significant (p=0.38)

library(ca)
model <- ca(BMICov)
#summary(model)  ## report of loadings
rco <- model$rowcoord ## extract row coordinates
cco <- model$colcoord ## extract column coordinates
#print(rco)
#print(cco)
rcodata <- data.frame(rco)
ccodata <- data.frame(cco)

### plot out the coordinates with ggplot2 ###
library(ggplot2)
ggplot()+
  geom_point(data=rcodata, aes(x=Dim1, y=Dim2), size=3, color="red", shape=16)+
  geom_point(data=ccodata, aes(x=Dim1, y=Dim2), size=3, color="blue", shape=16)+
  geom_text(data=rcodata, aes(x=Dim1, y=Dim2-0.07, label=rownames(rcodata)), size=3)+
  geom_text(data=ccodata, aes(x=Dim1, y=Dim2-0.07, label=rownames(ccodata)), size=4)+
  geom_hline(yintercept = 0, color = "black")+
  geom_vline(xintercept = 0, color = "black")+
  theme(legend.position="none")


