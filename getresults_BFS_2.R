rm(list=ls())
setwd("C:/Users/tara/Dropbox/R/BFS/Cohort_2/Data_Redo_New")
origwd <- getwd()

for(i in 1:8) {

  setwd(origwd)
  dir.create(paste0("dump",i))
  setwd(file.path(getwd(), paste0("dump",i)))
  
# where your experiment is hosted
# substituting slashes, tildes, etc. with periods
experigen.sourceURL = paste0("https...s3.amazonaws.com.experigendata.testexperigen.web.indexdump",i,"BFS2V.html")

# this information comes from your settings.js file
experigen.experimentName = paste0("BFS2dump",i)
experigen.database = "http://54.213.121.222/experigen/"
setInternet2(use = FALSE)
options(timeout=100000000) # patience is golden
getOption("timeout")

# check for usage of the experiment (number of page views per participant)
experigen.users  =  paste(experigen.database, "users.cgi?experimentName=", 
                          experigen.experimentName, "&sourceurl=", experigen.sourceURL, sep="")
download.file(experigen.users, "users.csv") 
users = read.csv("users.csv", sep="\t")

(users)
# read the experimental results from the server
experigen.url  =  paste(experigen.database, "makecsv.cgi?experimentName=", 
                        experigen.experimentName, "&sourceurl=", experigen.sourceURL, sep="")
download.file(experigen.url, destfile = "./xp.csv")
download.file(paste(experigen.url, "&file=demographics.csv", sep=""),"demographics.csv")
download.file(paste(experigen.url, "&file=eligibilitydec.csv", sep=""),"eligibilitydec.csv")
download.file(paste(experigen.url, "&file=suggestions.csv", sep=""),"suggestions.csv")
#xp  = read.csv("xp.csv", sep=",")
xp  = read.csv("xp.csv", sep="\t")
#meta = read.csv("demographics.csv", sep=",")
meta = read.csv("demographics.csv", sep="\t")
#elig = read.csv("eligibilitydec.csv", sep=",")
elig = read.csv("eligibilitydec.csv", sep="\t")
#sugg = read.csv("suggestions.csv", sep=",")
sugg = read.csv("suggestions.csv", sep="\t")

# setting up the numeric response variable
xp$response = ""
xp[xp$response1=="correct",]$response = 1
xp[xp$response1=="incorrect",]$response = 0
xp$response = factor(xp$response)

library(plyr)


# setting new tables
# views -- table of views and n's
# xp.train -- only the training block
# xp.elig -- only the eligibility block
# xp.stim -- only the main stimuli
# xp.catch -- only catch items
(views = ddply(xp, .(view), summarise, n=length(view)))
xp.train = subset(xp,view=="training.ejs")
xp.elig = subset(xp,view=="eligibility.ejs")
xp.stim = subset(xp,view=="stimulus.ejs")
xp.catch = subset(xp,view=="catch.ejs")


## users by eligibility trials
ddply(xp.elig, .(userCode), summarise, n=length(userCode))

## catch percentages
xp.catch$correct = (as.character(xp.catch$Accuracy) == as.character(xp.catch$response))
xp.catch[xp.catch$correct==T,]$correct = 1
xp.catch[xp.catch$correct==F,]$correct = 0
(catcheval = ddply(xp.catch, .(userCode,userFileName), summarise,
                   Correct=sum(correct),
                   Total=length(correct),
                   Percentage=(sum(correct)/length(correct))*100))
catcheval$Decision = "Keep"
if(nrow(catcheval[as.numeric(catcheval$Percentage)<80,])!=0){
catcheval[as.numeric(catcheval$Percentage)<80,]$Decision = "Discard"
}

# assuming all went well, write to disk
# so that the results are saved even after the database server is gone
# it would be unwise not to keep a local copy of your results

#Make sure folder is set to correct dump number, else you will overwrite!

head(xp)
write.csv(xp, "xp.csv")
write.csv(xp.train, "xp_train.csv")
write.csv(xp.elig, "xp_elig.csv")
write.csv(xp.stim, "xp_stim.csv")
write.csv(xp.catch, "xp_catch.csv")
write.csv(meta, "demographics.csv")
write.csv(users, "users.csv")
write.csv(elig, "eligibilitydec.csv")
write.csv(sugg, "suggestions.csv")
write.csv(catcheval, "catchevaluation.csv")

#merge(xp.elig,catcheval)

# optional cleanup: remove all variables that begin with "experigen."
rm(list=ls(pattern="^experigen."))
rm(list=ls(pattern="^xp"))

}