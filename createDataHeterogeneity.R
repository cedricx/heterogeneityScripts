##############################################################################
################                                               ###############
################ GO1 Data Creation for Heterogenity Project    ###############
################           Angel Garcia de la Garza            ###############
################              angelgar@upenn.edu               ###############
################                 03/16/2016                    ###############
##############################################################################


library(psych)
library(ggplot2)
library(base)
library(reshape2)
library(gamm4)
library(stats)
library(knitr)
library(mgcv)
library(visreg)
library(plyr)



### Load data release and QA dataset 
data.go1release <- read.csv("/data/joy/BBL/studies/pnc/subjectData/n1601_go1_datarel_020716.csv")
data.qa <- read.csv("/data/joy/BBL/projects/pnc_t1Heterogen/subjectData/pnc1601_qaRatings_20160316.csv")

##Merge then into one big dataset using bblid
data.final <- merge(data.go1release, data.qa, by="bblid")

##Exclude everyone that has a healthExclusion 
data.final <- data.final[which(data.final$healthExclude == 0), ]

#Exclude everyone with an average rating of zero (Bad Data Quality)
data.final <- data.final[-which(data.final$averageRating == 0), ]

#Exclude everyone using SBIAMarsQA exclusion
data.final <- data.final[-which(data.final$mprageSbiaMarsQaExclude == 1), ]


##Final sample contains 1387 subjects

##Create dataset with structural data
data.mars <- data.final[, c(1, grep("mprage_mars_vol", names(data.final)))]


#Create a typically developing vs. Not TD group. 
data.final$group<-"notTd"
data.final$group[which(data.final$goassessSmryPsychOverallRtg<4 & data.final$goassessPstd!="4PS" & data.final$ltnExclude==0)]<-"Td"
data.final$group<-as.factor(data.final$group)


#Create new demographics 
data.demographics <- data.final[ , c(1, grep("ageAtGo1Scan", names(data.final), fixed=T)
                                      , grep("sex", names(data.final), fixed=T)
                                      , 4
                                      , grep("handedness", names(data.final), fixed=T)
                                      , grep("meduCnbGo1", names(data.final), fixed=T)
                                      , grep("mprageMassICV", names(data.final), fixed=T)
                                      , grep("group", names(data.final), fixed=T)
                                     )]

##Change names of the data to something clearer
names(data.demographics)[2] <- "age"
names(data.demographics)[6] <- "maternal_education"
names(data.demographics)[7] <- "mprageMassICV"


#Create age squared and age cubed
data.demographics$age <- data.demographics$age / 12
data.demographics$ageSquared <- data.demographics$age * data.demographics$age
data.demographics$ageCubed <- data.demographics$ageSquared * data.demographics$age


#Make race a binary variable
data.demographics$race[which(data.demographics$race == 1)] <- "white"
data.demographics$race[which(data.demographics$race == 2)] <- "nonwhite"
data.demographics$race[which(data.demographics$race == 3)] <- "nonwhite"
data.demographics$race[which(data.demographics$race == 4)] <- "nonwhite"
data.demographics$race[which(data.demographics$race == 5)] <- "nonwhite"
data.demographics$race[which(data.demographics$race == 6)] <- "nonwhite"

#Change sex to strings not numeric 
data.demographics$sex[which(data.demographics$sex == 1)] <- "male"
data.demographics$sex[which(data.demographics$sex == 2)] <- "female"

#Write out data. 
write.csv(data.demographics, "n1387_go1_heterogeneity_demographics.csv", row.names = F)
write.csv(data.mars, "n1387_go1_heterogeneity_structural.csv", row.names = F)
