rm(list=ls())
setwd("C:/Users/tara/Dropbox/R/BFS/Cohort_2/Original_Data_Old")
#Do the same thing with "C:/Users/tara/Dropbox/R/BFS/Cohort_2/Data_Redo"
origwd <- getwd()

library(plyr)
library(modeest)
library(Hmisc)
library(data.table)

#Clean xp results from dumps 1-8
for(i in 1:8) {

 # if (i ==5){
#    next
#}
  setwd(origwd)
  setwd(file.path(getwd(), paste0("BFS2dump",i)))
 
  
  #Load in first set of results and clean them
  xp  = read.csv("xp.csv")
  dim(xp) 
  
  # setting up the numeric response variable
  xp$response = ""
  xp[xp$response1=="correct",]$response = 1
  xp[xp$response1=="incorrect",]$response = 0
  xp$response = as.numeric(xp$response)
  
  #Consider only the main trial data
  xp.stimuli = droplevels(subset(xp,view=="stimulus.ejs"))
  
  #Automated data cleaning method
  #Load merged catch trial data with pass/fail flagging
  catch = read.csv(file = "catchevaluation.csv")
  
  #Flag failed trials
  failed <- droplevels(subset(catch, Decision=="Discard"))
  drop1 <- levels(failed$userCode)
  
  #And incomplete trials
  incomplete <- droplevels(subset(catch,Total<20))
  drop2 <- levels(incomplete$userCode)
  
  #Drop rows corresponding with excluded raters
  xp.stim = droplevels(xp.stimuli[which(xp.stimuli$userCode%in%drop1 == FALSE),])
  xp.stim = droplevels(xp.stim[which(xp.stim$userCode%in%drop2 == FALSE),])
  
  #Load merged demographic data
  demo = read.csv(file = "demographics.csv")
  
  #Flag trials containing the string "test"
  testing <- droplevels(subset(demo, grepl("test", demo$headphones)==T))    
  drop3 <- levels(testing$userCode)
  
  #Drop rows corresponding with testers
  xp.stim = droplevels(xp.stim[which(xp.stim$userCode%in%drop3 == FALSE),])
  dim(xp.stim)
  #Get rid of any files for which the user reported a problem
  xp.stim <- droplevels(subset(xp.stim, xp.stim$response=="1" | xp.stim$response=="0",))
  str(xp.stim)
  
  
  setwd("C:/Users/tara/Dropbox/R/BFS/Cohort_2/Original_Data_Cleaned")
  
  write.table(xp.stim, file = paste0("BFS_data_dump",i), 
              row.names = T,col.names=NA, 
              quote = F, sep ="\t")
  
  
}

