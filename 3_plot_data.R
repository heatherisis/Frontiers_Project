rm(list=ls())

library(plotrix)
library(dplyr)
library(fields)
library(car)
library(reshape2)
library(plyr)

#Read in data
data <- read.csv("clean_data.csv")
#Check that variables are of correct types
str(data)
data$subject_number <- as.factor(data$subject_number)
#Need to re-set ordered factors
data$session = ordered(data$session, levels = c("BL1","BL2","BL3","BL4","BL5","TX1","TX2","TX3","TX4","TX5","TX6","TX7","TX8","TX9","TX10",
                                                "MP1","MP2","MP3","TX11","TX12",
                                                "TX13","TX14","TX15","TX16","TX17","TX18","TX19","TX20","MN1","MN2","MN3"))   


#This code creates an option to plot only the primary words and exclude generalization words
#Note how these categories are defined: primary means probed in BLMN and PREPOST,
#"generalization" means probed in BLMN only. 
#All are untreated (i.e. not words used as targets during treatment)
#You can ignore it if you are including all words

    #Determine which words are BLMN only and which are shared BLMN/PREPOST
    length(levels(data$word))
    tab2 = as.data.frame(plyr::count(data,"word"))
  #  plot(tab2$freq)
    #items occurring over 300 times are shared BLMN/PREPOST
    #and the items occurring under 100 times are BLMN only.
    both <- droplevels(tab2[which(tab2$freq>300),])
    primary <- levels(both$word)
    only <- droplevels(tab2[which(tab2$freq<150),])
    generalization <- levels(only$word)
    
    #Option to reduce to only primary words
    primarywords <- droplevels(data[which(data$word%in%primary),])
    head(primarywords)
    levels(primarywords$word)
    primarywords$word_type <- "primary"
    generalizationwords <- droplevels(data[which(data$word%in%generalization),])
    head(generalizationwords)
    levels(generalizationwords$word)
    generalizationwords$word_type <- "generalization"
    #Only uncomment this if you want to exclude generalization words
    #data <- primarywords

#Specs for plotting
par(mfrow=c(3,1))
par(mar = c(2.5, 2.5, 2.5, 2.5))
par(xpd=FALSE)

#Loop through participants
Children <- levels(data$subject)

#*creates empty vector to add in missing data*
missing <- c()

#*creates empty vector to add in missing data*
session_count <- c()

for (i in seq_along(Children)){
  
  child <- droplevels(data[data$subject==Children[i],])
  #child <- data[data$subject=="Hannah",] 
  numerical=c(1:length(levels(child$session)))
  minus <- length(numerical) - 2
  labels = levels(child$session)
  label = unique(child$subject)
  
   #*adds missing data for each child to a data frame*
  missing <- rbind(missing, droplevels(child[which(is.na(child$mean)),]))

  #*adds missing data for each child to a data frame*
  session_count <- rbind(session_count, 
      ddply(child,.(subject, session, prepost), summarise, count=length(session)))
    
  BL <- droplevels(subset(child, sessiontype=="BL"))
  nbl <- length(levels(BL$session))
  
  bl1_sum <- tapply(BL$correct, BL$session, sum, na.rm=TRUE)
  bl1_total <- tapply(BL$total, BL$session, sum, na.rm=TRUE)
  bl1_perc <- bl1_sum*100/bl1_total
  bl1_m <- mean(bl1_perc)
  
  #Plot Treated Targets
  ###NB name "BF" here is a holdover from cohort 1--
  ###both BF AND TRAD sessions are included in these plots
  #BF Pre
  BF <- subset(child, child$condition=="BF"|child$condition=="PROBE")
  BF_pre <- subset(BF, BF$prepost=="Pre"|BF$prepost=="PROBE")
  BF_pre_mean <- tapply(BF_pre$mean, BF_pre$session, mean)
  BF_pre_correct <- tapply(BF_pre$correct, BF_pre$session, sum)
  BF_pre_total <- tapply(BF_pre$total, BF_pre$session, sum)
  BF_pre_percent <- BF_pre_correct*100/BF_pre_total
  
  #BF Post
  BF_post <- subset(BF, BF$prepost=="Post")
  BF_post_mean <- tapply(BF_post$mean, BF_post$session, mean)
  BF_post_correct <- tapply(BF_post$correct, BF_post$session, sum)
  BF_post_total <- tapply(BF_post$total, BF_post$session, sum)
  BF_post_percent <- BF_post_correct*100/BF_post_total
  
  plot(numerical, BF_pre_percent, 
       main = label,
       xaxt="n", xlab = "Session", ylab = "Mean rating", ylim = c(0, 100))
  axis(1, at=numerical, labels=labels, cex.axis = 1.25)
  #Plot baseline mean
  yline(bl1_m, lty=2)
  
  points(numerical+.2, BF_post_percent,  pch = 8, col = "red", lty=2) 
  
  #Set points for shading BL, MP, and MN regions
  endBL <- nbl + .5
  beginMP <- endBL + 10
  endMP <- beginMP + 3
  beginMN <- endMP + 10
  endMN <- beginMN + 3
  color <- rgb(190, 190, 190, alpha=80, maxColorValue=255)
  rect(xleft=0.0-1, xright=endBL, ybottom=-4, ytop=106, density=100, 
       col=color)
  rect(xleft=beginMP, xright=endMP, ybottom=-4, ytop=106, density=100, 
       col=color)
  rect(xleft=beginMN, xright=endMN, ybottom=-4, ytop=106, density=100, 
       col=color)
  
  #Plot TRAD
  #TRAD Pre
  TRAD_pre <- subset(child, child$condition=="Trad"&child$prepost=="Pre")
  TRAD_pre_mean <- tapply(TRAD_pre$mean, TRAD_pre$session, mean)
  TRAD_pre_correct <- tapply(TRAD_pre$correct, TRAD_pre$session, sum)
  TRAD_pre_total <- tapply(TRAD_pre$total, TRAD_pre$session, sum)
  TRAD_pre_percent <- TRAD_pre_correct*100/TRAD_pre_total
  #TRAD Post
  TRAD_post <- subset(child, child$condition=="Trad"&child$prepost=="Post")
  TRAD_post_mean <- tapply(TRAD_post$mean, TRAD_post$session, mean)
  TRAD_post_correct <- tapply(TRAD_post$correct, TRAD_post$session, sum)
  TRAD_post_total <- tapply(TRAD_post$total, TRAD_post$session, sum)
  TRAD_post_percent <- TRAD_post_correct*100/TRAD_post_total
  
  #Plot points
  points(numerical, TRAD_pre_percent,  pch = 12, col = "blue", lty=2) 
  points(numerical+.2, TRAD_post_percent,  pch = 6, col = "green", lty=2) 

  #*Place a legend based on mean rating of MN sessions*
  ifelse(mean(rev(BF_pre_percent)[1:3]) <= 50, 
         legend("topright",cex=.9, legend = c("Pre BF", "Post BF","Pre Trad", "Post Trad"), 
                pch = c(1,1,12,6), col=c("black","red", "blue","green"), bg = "white"), 
         legend("bottomright" ,cex=.9, legend = c("Pre BF", "Post BF","Pre Trad", "Post Trad"), 
                pch = c(1,1,12,6), col=c("black","red", "blue","green"), bg = "white"))
  
}

#*write missing ratings to a data frame*
#write.csv(missing, "missing.csv")
row.names(missing) <- NULL
missing[c("subject", "session", "word")]

#*write session counts to a data frame*
write.csv(session_count, "session_count.csv")

