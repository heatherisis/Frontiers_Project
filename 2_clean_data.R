rm(list=ls())

library(plotrix)
library(dplyr)
library(fields)
library(car)
library(reshape2)
library(plyr)
getwd()
#data  = read.csv("BFS2_trials_summarized", sep="\t", header=T)
#To clean the unsummarized data, uncomment this:
data  = read.csv("BFS2_trials_unsummarized", sep="\t", header=T)

str(data)
#data$X.1 <- NULL
dim(data)
which(is.na(data$mean))


#Subject as factor
data$subject <- as.factor(data$subject)

#Create a session column
data$sessioncode <- data$Filename
data$study <- as.factor(sapply(strsplit(as.character(data$sessioncode), "\\_"), function(x) x[[1]]))
data$session <- as.factor(sapply(strsplit(as.character(data$sessioncode), "\\_"), function(x) x[[3]]))
levels(data$session)

#Cleanup: There is inconsistency in whether Tx is written Tx or TX
#fixme <- data[which(grepl("TX", data$session)==T),]
data$session <- recode(data$session, c("'Tx3'='TX3'"))
data$session <- recode(data$session, c("'tx11'='TX11'"))
data$session <- recode(data$session, c("'tx12'='TX12'"))
data$session <- recode(data$session, c("'Tx13'='TX13'"))
data$session <- recode(data$session, c("'Tx17'='TX17'"))
data$session <- recode(data$session, c("'Tx18'='TX18'"))
data$session <- recode(data$session, c("'Tx19'='TX19'"))
data$session <- recode(data$session, c("'Tx20'='TX20'"))
levels(data$session)

#Create a session type column 
data$sessiontype <- as.factor(substring(data$session,1,2))  
levels(data$sessiontype)

#Create condition and prepost columns
#Previously these were all starting out as NA; info was filled in to the condition and prepost
#columns for treatment sessions only (not probe sessions). However, the NAs were causing problems,
#so now the default coding is "PROBE."
data$condition <- "PROBE"
data$prepost <- "PROBE"

#Fill in condition and pre/post columns for TREATMENT DATA only
dataBL <- droplevels(data[which(data$sessiontype=="BL"),])
dataMN <- droplevels(data[which(data$sessiontype=="MN"),])
dataMP <- droplevels(data[which(data$sessiontype=="MP"),])
dataTX <- droplevels(data[which(data$sessiontype=="TX"),])

dataTX$prepost <- sapply(strsplit(as.character(dataTX$sessioncode), "\\_"), function(x) x[[4]])
dataTX$condition <- as.factor(sapply(strsplit(as.character(dataTX$sessioncode), "\\_"), function(x) x[[5]]))

#Use tables to check if cleanup is needed
table(dataTX$prepost)
table(dataTX$condition)

#Argh, some of the TX sessions have "Probe" or "pre/post" where they ought to have BF/TRAD
#set aside data that don't need changing
gooddata <- filter(dataTX, condition=="BF"|condition=="Trad")
dim(gooddata)
#Pull out the data to change
checkme <- filter(dataTX, condition!="BF"&condition!="Trad")
dim(checkme)
#Pull out the subset with "pre" or "post" instead of Trad/BF
prepost <- filter(checkme, condition=="post"|condition=="pre")
#All of the pre/post errors are from one session, so it's easy to fix:
prepost$prepost <- prepost$condition
prepost$condition <- "Trad"
#Pull out the subset with "Probe"
probe <- filter(checkme, condition=="Probe")
#These are also all from one session. According to the calendar, this was a BF session.
probe$condition <- "BF"
#The remaining subset has no _ between BF and word
nounderscore <- checkme[which(grepl("BF", checkme$condition)==T),]
nounderscore$condition <- as.factor(substring(nounderscore$condition,1,2))  
dataTX <- droplevels(rbind(gooddata, prepost, probe, nounderscore))
table(dataTX$condition)

#Now fix the prepost column
table(dataTX$prepost)
#Need to standardize capitalization
dataTX$prepost <- recode(dataTX$prepost, c("'post'='Post'"))
dataTX$prepost <- recode(dataTX$prepost, c("'pre'='Pre'"))

#Rebind all
data <- rbind(dataBL, dataMP, dataMN, dataTX)
data$session <- as.factor(data$session)
data$condition <- as.factor(data$condition)
data$prepost <- as.factor(data$prepost)
levels(data$condition)
levels(data$prepost)

#Add demographic info
demog = read.csv("BFS2_demog.csv")
demog$subject <- as.factor(demog$subject)
#UNCOMMENT THIS FOR THE UNMERGED DATA
#where subject is coded as Subject
data$subject <- data$Subject
data <- left_join(data, demog, by="subject")
data$subject <- as.factor(data$subject)
str(data)

#Make session an ordered factor
levels(data$session)
data$session = ordered(data$session, levels = c("BL1","BL2","BL3","BL4","BL5","TX1","TX2","TX3","TX4","TX5","TX6","TX7","TX8","TX9","TX10",
                                                "MP1","MP2","MP3","TX11","TX12",
                                                "TX13","TX14","TX15","TX16","TX17","TX18","TX19","TX20","MN1","MN2","MN3"))   
str(data)

#Save cleaned data
#write.csv(data, "clean_data.csv")
#To save cleaned unsummarized data, uncomment this:
write.csv(data, "clean_data_unsummarized.csv")


