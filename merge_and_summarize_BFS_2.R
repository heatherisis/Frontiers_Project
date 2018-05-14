rm(list=ls()) 
library(plyr)
library(modeest)
library(Hmisc)
library(data.table)

setwd("C:/Users/tara/Dropbox/R/BFS/Cohort_2/Original_Data_Cleaned")

##Combine all .txt files in directory of ORIG files
files <- as.vector(Sys.glob("*"))
files
data <- lapply(files, read.csv, header = TRUE, skipNul=TRUE, sep="\t") 
str(data)
mydata <- as.data.frame(rbindlist(data, fill=TRUE))
str(mydata)
nrow(mydata)
head(mydata)

setwd("C:/Users/tara/Dropbox/R/BFS/Cohort_2/All_Data_Merged/Cleaned_Original_Data/")

write.table(mydata, file = "BFS_trials_merged_ORIG", 
           row.names = T,col.names=NA, 
            quote = F, sep ="\t")


#Combine orig files with REDO files

setwd("C:/Users/tara/Dropbox/R/BFS/Cohort_2/Data_Redo_Cleaned")

files <- as.vector(Sys.glob("*"))
files
dataR <- lapply(files, read.csv, header = TRUE, skipNul=TRUE, sep="\t") 
str(dataR)
mydataR <- as.data.frame(rbindlist(dataR, fill=TRUE))
str(mydataR)
nrow(mydataR)

setwd("C:/Users/tara/Dropbox/R/BFS/Cohort_2/All_Data_Merged/Cleaned_Redo_Data")

write.table(mydataR, file = "BFS_trials_merged_REDO", 
            row.names = T,col.names=NA, 
            quote = F, sep ="\t")


mydataR <- mydataR[,order(names(mydataR))]

mydata <- mydata[,order(names(mydata))]

cols <- colnames(mydata)
Rcols <- colnames(mydataR)

which(cols%nin%Rcols)
cols[46]
mydata$soundproblemyn <- NULL
cols[62]
mydata$X_ <- NULL

which(Rcols%nin%cols)
Rcols[46]
mydataR$sourceurl <- NULL

xp.stim <- rbind(mydata, mydataR)
dim(xp.stim)

#Check again for files for which the user reported a problem
#All good this time
xp.stim <- droplevels(subset(xp.stim, xp.stim$response=="1" | xp.stim$response=="0",))
dim(xp.stim)
str(xp.stim)

mode = ddply(xp.stim, .(Filename), summarise, v=mfv(response)[1])
mean = ddply(xp.stim, .(Filename), summarise, v=mean(response)[1])
correct = ddply(xp.stim, .(Filename), summarise, v=sum(response)[1])
total = ddply(xp.stim, .(Filename), summarise, v=length(response)[1])
block = ddply(xp.stim, .(Filename), summarise, v=unique(Block)[1])
subject = ddply(xp.stim, .(Filename), summarise, v=unique(Subject)[1])
#sessionType = ddply(xp.stim, .(Filename), summarise, v=unique(SessionTypeNumber)[1])
# sessionNo = ddply(xp.stim, .(Filename), summarise, v=unique(TreatmentProbeNumber)[1])
#session = ddply(xp.stim, .(Filename), summarise, v=unique(Session)[1])
#study = ddply(xp.stim, .(Filename), summarise, v=unique(Study)[1])
word = ddply(xp.stim, .(Filename), summarise, v=unique(Word))
category = ddply(xp.stim, .(Filename), summarise, v=unique(Category))
target = ddply(xp.stim, .(Filename), summarise, v=unique(Target)[1])
subcategory = ddply(xp.stim, .(Filename), summarise, v=unique(Subcategory)[1])
conson = ddply(xp.stim, .(Filename), summarise, v=unique(C_or_V)[1])
length = ddply(xp.stim, .(Filename), summarise, v=unique(Length)[1])
complexity = ddply(xp.stim, .(Filename), summarise, v=unique(Complexity)[1])
backness = ddply(xp.stim, .(Filename), summarise, v=unique(Backness)[1])
experiment = ddply(xp.stim, .(Filename), summarise, v=unique(experimentName)[1])

#Concatenate as dataframe
sumdata <- data.frame(mode$Filename, mode$v, mean$v, correct$v, total$v, 
                     block$v, 
                     subject$v, 
                     #sessionType$v, sessionNo$v, 
                     # session$v, 
                     #study$v, 
                     word$v, category$v, 
                     target$v, subcategory$v, conson$v, length$v, complexity$v, backness$v
                     , experiment$v
)

sumdata <- plyr::rename(sumdata, c("mode.Filename"="Filename","mode.v"="mode", "mean.v"="mean",
                                 "correct.v"="correct", "total.v"="total", 
                                      "block.v"="block",
                                 "subject.v"="subject", 
                                # "session.v"="session", 
                                 #"study.v"="study",
                                 #  "sessionType.v"="sessionType", "sessionNo.v"="sessionNo",
                                 "word.v"="word", "category.v"="category", "target.v"="target", 
                                 "subcategory.v"="subcategory",
                                 "conson.v"="conson", "length.v"="length", "complexity.v"="complexity", 
                                 "backness.v"="backness"
                                 , "experiment.v"="experiment"
))


##Find files that need to be remeasured
str(sumdata)
dim(sumdata)
table(sumdata$experiment, sumdata$total)

remeasure <- droplevels(subset(sumdata, sumdata$total<=7)) 
dim(remeasure)
table(remeasure$experiment, remeasure$block)
table(remeasure$experiment, remeasure$block, remeasure$total)

#210 files with <=7 ratings.
#Of these, 136 are from Dump5, Block 3; they need only 1 more rater.

##Save files that are good to go
gooddata <- droplevels(subset(sumdata, sumdata$total>7))
str(gooddata)

#Are there any NAs in the data?
which(is.na(gooddata$total))

setwd("C:/Users/tara/Dropbox/R/BFS/Frontiers_Project")

write.table(gooddata, file = "BFS2_trials_summarized", 
            row.names = T,col.names=NA, 
            quote = F, sep ="\t")

