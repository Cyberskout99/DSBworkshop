setwd("C:/.../DSB Workshop")
source("DSBworkshop.R")

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
names(ptLabs)
ptLabsPost_qnt <- na.omit(ptLabs) %>%
  select("Post_BMI","Post_GlucF","Post_GlucR",
         "Post_HbA1c","Post_VitD","Post_ESR")

correlation(ptLabsPost_qnt) # 'agricolae' package

## Let's make a heatmap of correlation coefficients ##
library(corrplot)
View(ptLabsPost_qnt)
ptLabs_res <- cor.mtest(ptLabsPost_qnt, conf.level=0.95)
ptLabs_cor <- cor(ptLabsPost_qnt)

corrplot(ptLabs_cor, p.mat = ptLabs_res$p, tl.srt = 30, tl.col = "black", 
         insig = "blank", tl.pos="ld", tl.cex = 0.8,
         order = "original", method="shade",type="lower",
         cl.pos = "r", cl.ratio = 0.3, na.label = NA)

# can customize display with hierarchical clustering #
corrplot(ptLabs_cor, p.mat = ptLabs_res$p, tl.srt = 30, tl.col = "black", 
         insig = "p-value", tl.pos="ld", tl.cex = 0.8, 
         order = "hclust", method="shade",type="lower",
         cl.pos = "b", cl.ratio = 0.3)

