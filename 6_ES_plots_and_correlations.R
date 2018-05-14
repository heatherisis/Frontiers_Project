rm(list=ls())

library(plotrix)
library(dplyr)
library(fields)
library(car)
library(reshape2)
library(plyr)
library(magrittr)
library(tidyr)

#Read in ES data
data <- read.csv("cohort2_EF_data.csv")
str(data)

#Plots of effect sizes
#Boxplots:
#Is there an order effect? Boxplot depicting ES~Phase1~ across participants, boxplot depicting ES~Phase2~ across participants
#boxplot depicting difference between ES~Phase1~ and ES~Phase2~ across participants.
phasedata <- gather(data,Order, ES, c(ESPhase1, ESPhase2))
head(phasedata)
qplot(data=phasedata, x=Order, y=ES, geom="boxplot")
boxplot(phasedata$order_effect)

#Is there a condition effect? Boxplot depicting BF phase across participants, boxplot depicting TRAD phase across participants
phasedata <- gather(data,Type, ES, c(ES_TRAD, ES_BF))
head(phasedata)
qplot(data=phasedata, x=Type, y=ES, geom="boxplot")
boxplot(phasedata$BF_advantage)

#########################
#Effect size correlations
########################

#Option to run these without Madison, who is an outlier
#hist(data$ESall)
#data <- droplevels(data[data$ESall<20,])

#Correlation between ES~All~ and age
qplot(data=data, x=age_months, y=ESall, geom="point")
cor.test(data$age_months, data$ESall, type="spearman")
#NS

#Correlation between ES~All~ and baseline accuracy (participants who start out more accurate tend to gain more)
qplot(data=data, x=bl1_m, y=ESall, geom="point")
cor.test(data$bl1_m, data$ESall, type="spearman")
#NS

#Correlation between ES~All~ and perceptual acuity?
qplot(data=data, x=ACUITY_F_PRE, y=ESall, geom="point")
cor.test(data$ACUITY_F_PRE, data$ESall, type="spearman")
#NS

#Correlation between BF_advantage and perceptual acuity?
qplot(data=data, x=ACUITY_F_PRE, y=BF_advantage, geom="point")
cor.test(data$ACUITY_F_PRE, data$ESall, type="spearman")
#NS

#Correlation between ES~All~ and duration of previous treatment?

