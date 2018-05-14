rm(list=ls()) 
library(plyr)
library(modeest)
library(Hmisc)
library(data.table)
library(lme4)

#Read in unmerged data
unmergeddata <- read.csv("clean_data_unsummarized.csv")
#str(unmergeddata)
#dim(unmergeddata)

#Now fit logistic regression
#Because we will be comparing phase 1 and phase 2, use only treatment data
dataTX <- droplevels(unmergeddata[unmergeddata$condition!="PROBE",])
#dim(dataTX)
levels(dataTX$condition)
#Code by order
dataTX$phase <- 0
TRAD1 <- droplevels(subset(dataTX, tx_order=="TRAD_BF"))
TRAD1[which(TRAD1$condition=="Trad"),]$phase <- 1
TRAD1[which(TRAD1$condition=="BF"),]$phase <- 2
#head(TRAD1)
table(TRAD1$phase)
BF1 <- droplevels(subset(dataTX, tx_order=="BF_TRAD"))
BF1[which(BF1$condition=="BF"),]$phase <- 1
BF1[which(BF1$condition=="Trad"),]$phase <- 2

mydataTX <- rbind(TRAD1, BF1) 
table(mydataTX$phase)
#str(mydataTX)

#This model, including an interaction between condition and raw phase order, does not converge
#mymodfull <- glmer(response ~ condition*phase + (condition|subject) + (1|Word) + (1|userCode), data=mydataTX, family="binomial")

#Substitute order of treatment application for raw phase
mymod <- glmer(response ~ condition*tx_order + (condition|subject) + (1|Word) + (1|userCode), data=mydataTX, family="binomial")

#Compare mymod to a model without random slope
mymodnorandslope <- glmer(response ~ condition*tx_order + (1|subject) + (1|Word) + (1|userCode), data=mydataTX, family="binomial")
anova(mymod, mymodnorandslope)
#Significant, use model w/ random slope
#HEATHER: Check the above result- 
##HMC: mymod has a random slope at the subject level based on condition, while mymodnorandslope has a random effect at the subject level, 
##the significance says that including the random slope is helpful??

#Compare mymod to a model with no interaction term 
mymodnoint<- glmer(response ~ condition + tx_order + (condition|subject) + (1|Word) + (1|userCode), data=mydataTX, family="binomial")
anova(mymodnorandslope, mymodnoint)  #HMC:  updated data object to "mydataTX", having "dataTX" threw an error with anova
#Significant, include both effects and interaction
#HEATHER: Check the above result, HMC: wouldn't we want to compare mymodnoint with mymod, the one that only differs by one parameter?
anova(mymod, mymodnoint)  #Significant (barely), include interaction, in addition to random slope

summary(mymod)

#Summarize for plotting
sumdataTX <- dataTX %>%
  dplyr::group_by(subject, condition, tx_order) %>%
    dplyr::summarise(sum=sum(response), total=length(response))

sumdataTX$phat <- 100*(sumdataTX$sum/sumdataTX$total)   


head(sumdataTX)
sumdataTX$tx_order_plot <- mapvalues(sumdataTX$tx_order, from = c("BF_TRAD", "TRAD_BF"), 
                          to = c("Biofeedback/Traditional", "Traditional/Biofeedback"))
sumdataTX$Condition <- mapvalues(sumdataTX$condition, from = c("BF", "Trad"), 
                                     to = c("Biofeedback", "Traditional"))

p1 <- qplot(x = tx_order_plot, y = phat, fill = Condition, data = sumdataTX, geom = "boxplot", 
            main = expression(paste(hat(p), ' Accuracy by Treatment Order and Condition')), xlab = "Treatment Order", ylab = expression(hat(p)))
p1
