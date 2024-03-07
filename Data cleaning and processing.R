############################################################
### Load required libraries and set working directory
############################################################
library(optmatch)
library(plyr)
library(tidyverse)
#tidyverse_update()
library(lubridate)
require(data.table)
require(highcharter)
library(table1)
library(psych)
library(knitr)
library(lattice)             
library(likert) 
library(MASS)
library(viridis)
library(gridExtra)
library(car)
library(mctest)
library(epiDisplay)
setwd("D:/WendyRcoding/influenza_survey")
#options(scipen = 999)##you will not see the scientific number eXXX


##################################################
# cleaning raw data
##################################################
#3rd round
new_fludata <- read.csv2(file="Data/data_20210509.csv", header=TRUE, 
                         sep=",",fileEncoding="UTF-8-BOM")
new_fludata$survey_date <- as.Date(new_fludata$survey_date,format="%m/%d/%Y")
new_fludata$round_no <- 3
new_fludata$vac_201920 <-ifelse(new_fludata$vaccinated_last_yr==TRUE,1,0)
new_fludata$vac_202021 <-ifelse(new_fludata$vaccinated_this_yr==TRUE,1,0)
new_fludata$age <- 2021-new_fludata$birthday
new_fludata$student<-ifelse(new_fludata$occupation==1,1,0)

#2nd round
online_fludata <- read.csv2(file="Data/google_survey.csv", header=TRUE, sep=",")
online_fludata$survey_date <- as.Date(online_fludata$survey_date,format="%m/%d/%Y")
online_fludata$round_no <- 2
online_fludata$vac_201920 <- ifelse(online_fludata$vaccinated_this_yr==TRUE,1,0)
online_fludata$vac_202021 <-NA
online_fludata$age <- 2020-online_fludata$birthday
online_fludata$student<-ifelse(online_fludata$occupation==1,1,0)

#1st round
paper_fludata <- read.csv2(file="Data/survey1.csv", header=TRUE, sep=",")
paper_fludata$survey_date <- as.Date(paper_fludata$survey_date,format="%d/%m/%Y")
#make all the people with vaccination date T
paper_fludata$vaccinated_this_yr<- ifelse(paper_fludata$vaccinated_date=="2/10/2019"|
                                            paper_fludata$vaccinated_date=="26/10/2019"|
                                            paper_fludata$vaccinated_date=="30/9/2019",
                                          TRUE,paper_fludata$vaccinated_this_yr)
paper_fludata$round_no <- 1
paper_fludata$vacc_timing<-ifelse(paper_fludata$vacc_timing=="5",
                                  "4",paper_fludata$vacc_timing)
paper_fludata$vac_201920 <-ifelse(paper_fludata$vaccinated_this_yr==TRUE,1,0)
paper_fludata$vac_202021 <-NA
paper_fludata$age <- 2019-paper_fludata$birthday
paper_fludata$student<-ifelse(paper_fludata$occupation==4|
                                paper_fludata$occupation==5,0,1)
###select the random sample in 1st round
paper_fludata_random <-paper_fludata[!paper_fludata$vaccinated_date=="2/10/2019"&
                                       !paper_fludata$vaccinated_date=="26/10/2019"&
                                       !paper_fludata$vaccinated_date=="30/9/2019",]


##merge the three databases
total_fludata <- rbind.fill(paper_fludata_random,online_fludata,new_fludata)

var_names <- c("info_source","pull_factor","flu_treatment","flu_preventive_measure","covid_vac_care")
var_names_lengths <- c(8,6,4,8,6)
for (i in (1:length(var_names))) {
  for (j in (1:var_names_lengths[i])) {
    col_name <- paste(var_names[i],".",as.character(j),sep="") #column name
    total_fludata[[col_name]] <- 0
    total_fludata[[col_name]][grepl(j,total_fludata[[var_names[i]]])] <- 1 #see if choose that option
  }
}

total_fludata$others_opinion_effect_positive <- ifelse(
  grepl(1,total_fludata$others_opinion_effect)==TRUE,1,0)
total_fludata$others_opinion_effect_negative<- ifelse(
  grepl(-1,total_fludata$others_opinion_effect)==TRUE,1,0)

total_fludata$ageGroup <- ifelse(total_fludata$age>35,1,0)
total_fludata$year <- format(total_fludata$survey_date, format="%Y")
total_fludata$vaccinated_tf <- ifelse(total_fludata$vaccinated_last_yr=="TRUE",1,
                                      ifelse(total_fludata$vaccinated_this_yr=="TRUE",1,0))

total_fludata$care_social_distancing <- 
  ifelse(total_fludata$flu_preventive_measure.1=="1",1,
         ifelse(total_fludata$flu_preventive_measure.3=="1",1,0))
#table(total_fludata$round_no)

##select student
student_data<- total_fludata[total_fludata$student=="1"&
                               !is.na(total_fludata$student),]

#write.csv(total_fludata,file = "Data/output/dataset_for_thesis.csv")