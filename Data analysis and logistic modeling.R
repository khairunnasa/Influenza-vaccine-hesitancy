library(optmatch)
library(plyr)
library(tidyverse)
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
setwd("DATA(D:)/Flu vaccine hesitancy/New folder/Data")

##################################################
# loading and cleaning raw data from survey
# 3 rounds total
##################################################
#3rd round
new_fludata <- read.csv2 (file = "data/data_20210509.csv", 
                          header = TRUE, sep = ",", fileEncoding = "UTF-8-BOM")
new_fludata$survey_date <- as.Date (new_fludata$survey_date, format = "%m/%d/%Y")
new_fludata$season <- "2020/21"
new_fludata$vac_201920 <- ifelse (new_fludata$vaccinated_last_yr == TRUE, 1, 0)
new_fludata$vac_202021 <- ifelse (new_fludata$vaccinated_this_yr == TRUE, 1, 0)
new_fludata$age <- 2021 - new_fludata$birthday
new_fludata$student <- ifelse (new_fludata$occupation == 1, 1, 0)

#2nd round
online_fludata <- read.csv2 (file = "data/google_survey.csv", 
                             header = TRUE, sep = ",")
online_fludata$survey_date <- as.Date (online_fludata$survey_date, 
                                       format = "%m/%d/%Y")
online_fludata$season <- "2019/20"
online_fludata$vac_201920 <- ifelse(online_fludata$vaccinated_this_yr == TRUE, 
                                    1, 0)
online_fludata$vac_202021 <- NA
online_fludata$age <- 2020 - online_fludata$birthday
online_fludata$student <- ifelse (online_fludata$occupation == 1, 1, 0)

#1st round
paper_fludata <- read.csv2 (file = "data/survey1_new.csv", header = TRUE, 
                            sep = ",")
paper_fludata$survey_date <- as.Date (paper_fludata$survey_date, 
                                      format = "%d/%m/%Y")
paper_fludata$vaccinated_date[is.na (paper_fludata$vaccinated_date)] <- NaN
#make all the people with vaccination date to be TRUE in this column
paper_fludata$vaccinated_this_yr <- ifelse(
  paper_fludata$vaccinated_date == "2/10/2019" |
    paper_fludata$vaccinated_date == "26/10/2019" |
    paper_fludata$vaccinated_date == "30/9/2019",
  TRUE, paper_fludata$vaccinated_this_yr)
paper_fludata$season <- "2019/20"
paper_fludata$vacc_timing <- ifelse (paper_fludata$vacc_timing == "5",
                                     "4", paper_fludata$vacc_timing) #5 is combine with 4 later
paper_fludata$vac_201920 <- ifelse (paper_fludata$vaccinated_this_yr == TRUE, 
                                    1, 0)
paper_fludata$vac_202021 <- NA
paper_fludata$age <- 2019 - paper_fludata$birthday_new
paper_fludata$student <- ifelse (paper_fludata$occupation == 4|
                                   paper_fludata$occupation == 5, 0, 1)

###select the random sample in 1st round (not in 2nd 26nd Oct and 30nd Sept)
paper_fludata_random <- 
  paper_fludata[paper_fludata$vaccinated_date != "2/10/2019" &
                  paper_fludata$vaccinated_date != "26/10/2019" &
                  paper_fludata$vaccinated_date != "30/9/2019", ]
###################################################
# merging and selecting the data for further analysis
##################################################
##merge the three databases
total_fludata <- rbind.fill (paper_fludata_random, online_fludata, new_fludata)

#change the formate of some multiple answers questions
var_names <- c("info_source", "pull_factor", "flu_treatment", 
               "flu_preventive_measure", "covid_vac_care")
var_names_lengths <- c(8, 6, 4, 8, 6)
for (i in (1:length(var_names))) {
  for (j in (1:var_names_lengths[i])) {
    col_name <- paste(var_names[i], ".", as.character(j), sep= "") #column name
    total_fludata[[col_name]] <- 0
    total_fludata[[col_name]][grepl(j, total_fludata[[var_names[i]]])] <- 1 #see if choose that option
  }
}

total_fludata$others_opinion_effect[total_fludata$others_opinion_effect == ""] <- NA 
total_fludata$others_opinion_effect_positive <- ifelse(
  grepl(1, total_fludata$others_opinion_effect) == TRUE, 1, 0)
total_fludata$others_opinion_effect_negative <- ifelse(
  grepl(-1, total_fludata$others_opinion_effect) == TRUE, 1, 0)

total_fludata$ageGroup <- ifelse(total_fludata$age > 35, 1, 0)
total_fludata$year <- format(total_fludata$survey_date, format = "%Y")
total_fludata$vaccinated_tf <- 
  ifelse(total_fludata$vaccinated_last_yr == "TRUE", 1,
         ifelse(total_fludata$vaccinated_this_yr == "TRUE", 1, 0))

total_fludata$care_social_distancing <- 
  ifelse(total_fludata$flu_preventive_measure.1 == "1", 1,
         ifelse(total_fludata$flu_preventive_measure.3 == "1", 1, 0))

table(total_fludata$season)

#choose people whose age <=35 & >=18 -> younger adults
young_ppl <- total_fludata[total_fludata$ageGroup == "0"&
                             !is.na (total_fludata$ageGroup), ]
mean(young_ppl$age,na.rm = TRUE)
median(young_ppl$age,na.rm = TRUE)
sd(young_ppl$age,na.rm = TRUE)
mean(young_ppl [young_ppl$season=="2019/20", ]$age, na.rm = TRUE)
median(young_ppl [young_ppl$season=="2019/20", ]$age, na.rm = TRUE)
sd(young_ppl [young_ppl$season=="2019/20", ]$age, na.rm = TRUE)
mean(young_ppl [young_ppl$season=="2020/21", ]$age, na.rm = TRUE)
median(young_ppl [young_ppl$season=="2020/21", ]$age, na.rm = TRUE)
sd(young_ppl [young_ppl$season=="2020/21", ]$age, na.rm = TRUE)
table(young_ppl$season)

total_fludata_plot <- young_ppl

total_fludata_plot$season <- as.character(total_fludata_plot$season)
total_fludata_plot$vacc_care <- as.character(total_fludata_plot$vacc_care)
total_fludata_plot$info_source <- as.character(total_fludata_plot$info_source)
total_fludata_plot$vacc_timing <- as.character(total_fludata_plot$vacc_timing)
total_fludata_plot$circle_vaccinated <- as.character(total_fludata_plot$circle_vaccinated)
total_fludata_plot$others_opinion_effect <- as.character(total_fludata_plot$others_opinion_effect)
total_fludata_plot$occupation  <- as.character(total_fludata_plot$occupation )
total_fludata_plot$religion <- as.character(total_fludata_plot$religion)
total_fludata_plot$personal_health <- as.character(total_fludata_plot$personal_health)
total_fludata_plot$gender <- as.character(total_fludata_plot$gender)
total_fludata_plot$marital_state <- as.character(total_fludata_plot$marital_state)
total_fludata_plot$flu_disturbance <-as.character(total_fludata_plot$flu_disturbance)
total_fludata_plot$personal_health_satisfaction <- as.character(total_fludata_plot$personal_health_satisfaction)
total_fludata_plot$average_monthly_income <- as.character(total_fludata_plot$average_monthly_income)
total_fludata_plot$own_flu_symptoms <- as.character(total_fludata_plot$own_flu_symptoms)
total_fludata_plot$circle_flu_symptoms  <- as.character(total_fludata_plot$circle_flu_symptoms)
total_fludata_plot$flu_gravity  <- as.character(total_fludata_plot$flu_gravity)
total_fludata_plot$flu_fatality_guess  <- as.character(total_fludata_plot$flu_fatality_guess)
total_fludata_plot$vacc_recommend  <- as.character(total_fludata_plot$vacc_recommend)
total_fludata_plot$flu_pandemic_influence  <- as.character(total_fludata_plot$flu_pandemic_influence)
total_fludata_plot$vaccinated_last_yr <- as.character(total_fludata_plot$vaccinated_last_yr)
total_fludata_plot$vacc_efficacy  <- as.character(total_fludata_plot$vacc_efficacy)
total_fludata_plot$vacc_sides_worry  <- as.character(total_fludata_plot$vacc_sides_worry)
total_fludata_plot$vacc_willingness_next_yr<- as.character(total_fludata_plot$vacc_willingness_next_yr)
total_fludata_plot$chinese_medicine  <- as.character(total_fludata_plot$chinese_medicine)
total_fludata_plot$exercise_regularly  <- as.character(total_fludata_plot$exercise_regularly)
total_fludata_plot$weekly_exercise  <- as.character(total_fludata_plot$weekly_exercise)
total_fludata_plot$vac_201920 <- as.character(total_fludata_plot$vac_201920)
total_fludata_plot$vaccinated_tf <- as.character(total_fludata_plot$vaccinated_tf)

#############################
###table the general information of the data
gen_data <- total_fludata_plot[,(names(total_fludata_plot) %in%
                                   c("gender","marital_state","average_monthly_income",
                                     "religion","vaccinated_tf","student"))]

gen_data$student <- ifelse(gen_data$student=="1","1)Student","2)Other occupation")
gen_data$gender <-  ifelse(gen_data$gender=="F","1)Female","2)Male")
gen_data$marital_state <-  ifelse(gen_data$marital_state=="1","1)Single/unmarried",
                                  ifelse(gen_data$marital_state=="2","2)Married/cohabiting","3)Others"))
gen_data$vaccinated_tf <- ifelse(gen_data$vaccinated_tf=="1","Vaccinated","Unvaccinated")

gen_data$average_monthly_income <-  
  ifelse(gen_data$average_monthly_income=="0","1)0-no income",
         ifelse(gen_data$average_monthly_income=="1","2)below $20,000",
                ifelse(gen_data$average_monthly_income=="2","3)$20,000-40,000",
                       ifelse(gen_data$average_monthly_income=="3","4)$40,000-60,000",
                              ifelse(gen_data$average_monthly_income=="4","5)$60,000-80,000",
                                     ifelse(gen_data$average_monthly_income=="5","6)$80,000-100,000","7)above$100,000"))))))
gen_data$religion <-  
  ifelse(gen_data$religion=="0","1)None",
         ifelse(gen_data$religion=="1","2)Buddhist",
                ifelse(gen_data$religion=="3","3)Christian",
                       ifelse(gen_data$religion=="4","4)Catholic",
                              ifelse(gen_data$religion=="5","5)Atheism",
                                     ifelse(gen_data$religion=="7","6)Believes in God but no particular religion",
                                            ifelse(gen_data$religion=="8","7)Kuan Tao","8)Others")))))))


table1::label(gen_data$gender)  <-  "Gender"
table1::label(gen_data$marital_state)  <-  "Marital Status"
table1::label(gen_data$average_monthly_income)  <-  "Family Average Monthly Income"
table1::label(gen_data$religion)  <-  "Religion"
table1::label(gen_data$vaccinated_tf)  <-  "FLU Vaccinated history(two year)"
table1::label(gen_data$student)  <-  "Occupation"
table1(~ gender +student +marital_state +average_monthly_income+ religion,
       data=gen_data)

gen_data_1 <- total_fludata_plot[,(names(total_fludata_plot) %in%
                                     c("gender","marital_state","average_monthly_income",
                                       "religion","vaccinated_tf","student","season"))]
gen_data_11<- gen_data_1[gen_data_1$season=="2019/20",]
gen_data_11$student <- ifelse(gen_data_11$student=="1","1)Student","2)Other occupation")
gen_data_11$gender <-  ifelse(gen_data_11$gender=="F","1)Female","2)Male")
gen_data_11$marital_state <-  ifelse(gen_data_11$marital_state=="1","1)Single/unmarried",
                                     ifelse(gen_data_11$marital_state=="2","2)Married/cohabiting","3)Others"))
gen_data_11$vaccinated_tf <- ifelse(gen_data_11$vaccinated_tf=="1","Vaccinated","Unvaccinated")

gen_data_11$average_monthly_income <-  
  ifelse(gen_data_11$average_monthly_income=="0","1)0-no income",
         ifelse(gen_data_11$average_monthly_income=="1","2)below $20,000",
                ifelse(gen_data_11$average_monthly_income=="2","3)$20,000-40,000",
                       ifelse(gen_data_11$average_monthly_income=="3","4)$40,000-60,000",
                              ifelse(gen_data_11$average_monthly_income=="4","5)$60,000-80,000",
                                     ifelse(gen_data_11$average_monthly_income=="5","6)$80,000-100,000","7)above$100,000"))))))
gen_data_11$religion <-  
  ifelse(gen_data_11$religion=="0","1)None",
         ifelse(gen_data_11$religion=="1","2)Buddhist",
                ifelse(gen_data_11$religion=="3","3)Christian",
                       ifelse(gen_data_11$religion=="4","4)Catholic",
                              ifelse(gen_data_11$religion=="5","5)Atheism",
                                     ifelse(gen_data_11$religion=="7","6)Believes in God but no particular religion",
                                            ifelse(gen_data_11$religion=="8","7)Kuan Tao","8)Others")))))))


table1::label(gen_data_11$gender)  <-  "Gender"
table1::label(gen_data_11$marital_state)  <-  "Marital Status"
table1::label(gen_data_11$average_monthly_income)  <-  "Family Average Monthly Income"
table1::label(gen_data_11$religion)  <-  "Religion"
table1::label(gen_data_11$vaccinated_tf)  <-  "FLU Vaccinated history(two year)"
table1::label(gen_data_11$student)  <-  "Occupation"

table1(~ gender +student +marital_state +average_monthly_income+ religion,
       data=gen_data_11)

gen_data_12<- gen_data_1[gen_data_1$season=="2020/21",]
gen_data_12$student <- ifelse(gen_data_12$student=="1","1)Student","2)Other occupation")
gen_data_12$gender <-  ifelse(gen_data_12$gender=="F","1)Female","2)Male")
gen_data_12$marital_state <-  ifelse(gen_data_12$marital_state=="1","1)Single/unmarried",
                                     ifelse(gen_data_12$marital_state=="2","2)Married/cohabiting","3)Others"))
gen_data_12$vaccinated_tf <- ifelse(gen_data_12$vaccinated_tf=="1","Vaccinated","Unvaccinated")

gen_data_12$average_monthly_income <-  
  ifelse(gen_data_12$average_monthly_income=="0","1)0-no income",
         ifelse(gen_data_12$average_monthly_income=="1","2)below $20,000",
                ifelse(gen_data_12$average_monthly_income=="2","3)$20,000-40,000",
                       ifelse(gen_data_12$average_monthly_income=="3","4)$40,000-60,000",
                              ifelse(gen_data_12$average_monthly_income=="4","5)$60,000-80,000",
                                     ifelse(gen_data_12$average_monthly_income=="5","6)$80,000-100,000","7)above$100,000"))))))
gen_data_12$religion <-  
  ifelse(gen_data_12$religion=="0","1)None",
         ifelse(gen_data_12$religion=="1","2)Buddhist",
                ifelse(gen_data_12$religion=="3","3)Christian",
                       ifelse(gen_data_12$religion=="4","4)Catholic",
                              ifelse(gen_data_12$religion=="5","5)Atheism",
                                     ifelse(gen_data_12$religion=="7","6)Believes in God but no particular religion",
                                            ifelse(gen_data_12$religion=="8","7)Kuan Tao","8)Others")))))))


table1::label(gen_data_12$gender)  <-  "Gender"
table1::label(gen_data_12$marital_state)  <-  "Marital Status"
table1::label(gen_data_12$average_monthly_income)  <-  "Family Average Monthly Income"
table1::label(gen_data_12$religion)  <-  "Religion"
table1::label(gen_data_12$vaccinated_tf)  <-  "FLU Vaccinated history(two year)"
table1::label(gen_data_12$student)  <-  "Occupation"

table1(~ gender +student +marital_state +average_monthly_income+ religion,
       data=gen_data_12)

# preparing for the data-set
test_data <- young_ppl

##vaccine hesitancy
test_data$vaccine_hesitancy <-  
  ifelse(test_data$vacc_acceptance>=6 & test_data$vacc_willingness_next_yr>=2,
         "2)No Hesitance","1)Hesitance")
test_data$vaccine_hesitancy[is.na(test_data$vaccine_hesitancy)] <- "1)Hesitance"

##vaccine willingness
test_data$vacc_willingness_next_yr[test_data$vacc_willingness_next_yr=="NaN"] <- NA
test_data$vacc_willingness_outcome <- ifelse(test_data$vacc_willingness_next_yr=="0"|
                                               test_data$vacc_willingness_next_yr=="1",
                                             "2)UnWilling","1)Willing")
#vaccine acceptance
test_data$vacc_acceptance <- ifelse(test_data$vacc_acceptance>5,
                                    "1)greater than 5","2)equal/less than 5")

#others
test_data$vacc_care <- ifelse(test_data$vacc_care>=2,"1)care more","2)care less")
test_data$vacc_timing <- ifelse(test_data$vacc_timing>1,"1)vaccinated before flu season","2)else")
test_data$circle_vaccinated<- ifelse(test_data$circle_vaccinated>=2,"1)half or more","2)less than half")
test_data$personal_health_satisfaction <- ifelse(
  test_data$personal_health_satisfaction>0,"1)satisfied","2)unsatisfied")
test_data$flu_disturbance <- ifelse(test_data$flu_disturbance>=2,"1)troble","2)no or little")
test_data$own_flu_symptoms <- ifelse(test_data$own_flu_symptoms>=3,"1)more than once a year","2)less")
test_data$own_recent_flu_symptoms <- ifelse(test_data$own_recent_flu_symptoms==TRUE,"1)TRUE","2)FALSE")
test_data$circle_flu_symptoms <- ifelse(test_data$circle_flu_symptoms>=3,"1)more than once a year","2)less")
test_data$flu_gravity <- ifelse(test_data$flu_gravity>=2,"1)know a bit or more","2)don't know or much")
test_data$flu_fatality_guess <- ifelse(test_data$flu_fatality_guess>=3,"1)more than 200","2)0-200")
test_data$flu_pandemic_influence <- ifelse(test_data$flu_pandemic_influence>=2,"1)improve/'a lot","2)else")
test_data$flu_hospitalized <- ifelse(test_data$flu_hospitalized==TRUE,"1)TRUE","2)FALSE")
test_data$vacc_recommend <- ifelse(test_data$vacc_recommend>0,"1)for","2)against")
test_data$vacc_efficacy <- ifelse(test_data$vacc_efficacy>0,"1)protect","2)worse or no")
test_data$vacc_sides_worry <- ifelse(test_data$vacc_sides_worry>=2,"1)quite worried more","2)slightly or no worried")
test_data$chinese_medicine <- ifelse(test_data$chinese_medicine>=1,"1)often","2)not ver often")
test_data$exercise_regularly   <- ifelse(test_data$exercise_regularly>=2,"1)often","2)never or not often")
test_data$weekly_exercise  <- ifelse(test_data$weekly_exercise>=3,"1)5h or more","2)0-5h")

test_data$share_opinion_on_vacc <- ifelse(test_data$share_opinion_on_vacc>5,"1)greater than 5","2)equal/less than 5")
test_data$affected_by_others_opinion <- ifelse(test_data$affected_by_others_opinion==TRUE,"1)TRUE","2)FALSE")
test_data$gender <- ifelse(test_data$gender=='M',"1)Male","2)Female")
test_data$ageGroup <- ifelse(test_data$ageGroup=="1","1)above 35yo","2)equal/below 35yo")
test_data$religion <- ifelse(test_data$religion<=0,"1)no religion","2)have religion")
test_data$marital_state <- ifelse(test_data$marital_state<=1,"1)single","2)others")
test_data$personal_health<- ifelse(test_data$personal_health>0,"1)satisfied","2)unsatisfied")
test_data$average_monthly_income<- ifelse(test_data$average_monthly_income>=4,"1)above$60000","2)$0-6000")
test_data$others_opinion_effect_positive<- ifelse(test_data$others_opinion_effect_positive==1,"1)TRUE","2)FALSE")
test_data$others_opinion_effect_negative<- ifelse(test_data$others_opinion_effect_negative==1,"1)TRUE","2)FALSE")
test_data$care_social_distancing<-ifelse(test_data$care_social_distancing==1,"1)TRUE","2)FALSE")
test_data$vaccinated_last_yr<-ifelse(test_data$vaccinated_last_yr==TRUE,"1)TRUE","2)FALSE")
test_data$student<-ifelse(test_data$student==1,"1)TRUE","2)FALSE")



##################################
#### hesitance
##################################
###overall hesitance
test_data_all<- test_data
remove_col <- c( "survey_date","ID" , "info_source",
                 "pull_factor","others_opinion_effect" ,"flu_treatment" ,
                 "flu_preventive_measure" ,"surname" ,"birthday" ,"occupation" ,
                 "department" ,"follow_up", "tel","email","age" ,
                 "area" ,"vaccinated_date","vac_accessibility","covid_flu",
                 "covid_vac_care","covid_vaccinated","covid_vac_willingness",
                 "infect_num_willingness","year","ageGroup",
                 "vaccinated_tf","vac_202021","vac_201920",
                 "vaccinated_outcome","vacc_willingness_outcome")

hes_all <- test_data_all[,(!names(test_data_all) %in% remove_col)]

hes_all <- data.frame(lapply(hes_all, function(x) as.factor(as.character(x))))

hes_all_test<-table1(~.|vaccine_hesitancy,data = hes_all)

hes_all_test

#################################################################################################
##########chi-square test

test_data <- young_ppl
##vaccine hesitancy
test_data$vaccine_hesitancy <-  
  ifelse(test_data$vacc_acceptance>=6 & test_data$vacc_willingness_next_yr>=2,
         "2)No Hesitance","1)Hesitance")
test_data$vaccine_hesitancy[is.na(test_data$vaccine_hesitancy)] <- "1)Hesitance"

##vaccine willingness
test_data$vacc_willingness_next_yr[test_data$vacc_willingness_next_yr=="NaN"] <- NA
test_data$vacc_willingness_outcome <- ifelse(test_data$vacc_willingness_next_yr=="0"|
                                               test_data$vacc_willingness_next_yr=="1",
                                             "2)UnWilling","1)Willing")
#vaccine acceptance
test_data$vacc_acceptance <- ifelse(test_data$vacc_acceptance>5,
                                    "1)greater than 5","2)equal/less than 5")

#others
test_data$vacc_care <- ifelse(test_data$vacc_care>=2,"1)care more","2)care less")
test_data$vacc_timing <- ifelse(test_data$vacc_timing>1,"1)vaccinated before flu season","2)else")
test_data$circle_vaccinated<- ifelse(test_data$circle_vaccinated>=2,"1)half or more","2)less than half")
test_data$personal_health_satisfaction <- ifelse(
  test_data$personal_health_satisfaction>0,"1)satisfied","2)unsatisfied")
test_data$flu_disturbance <- ifelse(test_data$flu_disturbance>=2,"1)troble","2)no or little")
test_data$own_flu_symptoms <- ifelse(test_data$own_flu_symptoms>=3,"1)more than once a year","2)less")
test_data$own_recent_flu_symptoms <- ifelse(test_data$own_recent_flu_symptoms==TRUE,"1)TRUE","2)FALSE")
test_data$circle_flu_symptoms <- ifelse(test_data$circle_flu_symptoms>=3,"1)more than once a year","2)less")
test_data$flu_gravity <- ifelse(test_data$flu_gravity>=2,"1)know a bit or more","2)don't know or much")
test_data$flu_fatality_guess <- ifelse(test_data$flu_fatality_guess>=3,"1)more than 200","2)0-200")
test_data$flu_pandemic_influence <- ifelse(test_data$flu_pandemic_influence>=2,"1)improve/'a lot","2)else")
test_data$flu_hospitalized <- ifelse(test_data$flu_hospitalized==TRUE,"1)TRUE","2)FALSE")
test_data$vacc_recommend <- ifelse(test_data$vacc_recommend>0,"1)for","2)against")
test_data$vacc_efficacy <- ifelse(test_data$vacc_efficacy>0,"1)protect","2)worse or no")
test_data$vacc_sides_worry <- ifelse(test_data$vacc_sides_worry>=2,"1)quite worried more","2)slightly or no worried")
test_data$chinese_medicine <- ifelse(test_data$chinese_medicine>=1,"1)often","2)not ver often")
test_data$exercise_regularly   <- ifelse(test_data$exercise_regularly>=2,"1)often","2)never or not often")
test_data$weekly_exercise  <- ifelse(test_data$weekly_exercise>=3,"1)5h or more","2)0-5h")

test_data$share_opinion_on_vacc <- ifelse(test_data$share_opinion_on_vacc>5,"1)greater than 5","2)equal/less than 5")
test_data$affected_by_others_opinion <- ifelse(test_data$affected_by_others_opinion==TRUE,"1)TRUE","2)FALSE")
test_data$gender <- ifelse(test_data$gender=='M',"1)Male","2)Female")
test_data$ageGroup <- ifelse(test_data$ageGroup=="1","1)above 35yo","2)equal/below 35yo")
test_data$religion <- ifelse(test_data$religion<=0,"1)no religion","2)have religion")
test_data$marital_state <- ifelse(test_data$marital_state<=1,"1)single","2)others")
test_data$personal_health<- ifelse(test_data$personal_health>0,"1)satisfied","2)unsatisfied")
test_data$average_monthly_income<- ifelse(test_data$average_monthly_income>=4,"1)above$60000","2)$0-6000")
test_data$others_opinion_effect_positive<- ifelse(test_data$others_opinion_effect_positive==1,"1)TRUE","2)FALSE")
test_data$others_opinion_effect_negative<- ifelse(test_data$others_opinion_effect_negative==1,"1)TRUE","2)FALSE")
test_data$care_social_distancing<-ifelse(test_data$care_social_distancing==1,"1)TRUE","2)FALSE")
test_data$vaccinated_last_yr<-ifelse(test_data$vaccinated_last_yr==TRUE,"1)TRUE","2)FALSE")
test_data$student<-ifelse(test_data$student==1,"1)TRUE","2)FALSE")

###flu vaccine awareness

chisq.test(test_data$vacc_care,test_data$vaccine_hesitancy, correct=FALSE)
chisq.test(test_data$vacc_timing,test_data$vaccine_hesitancy, correct=FALSE)
chisq.test(test_data$circle_vaccinated,test_data$vaccine_hesitancy, correct=FALSE)

###flu susceptibility

chisq.test(test_data$personal_health_satisfaction,test_data$vaccine_hesitancy, correct=FALSE)
chisq.test(test_data$flu_disturbance,test_data$vaccine_hesitancy, correct=FALSE)
chisq.test(test_data$own_flu_symptoms,test_data$vaccine_hesitancy, correct=FALSE)
chisq.test(test_data$own_recent_flu_symptoms,test_data$vaccine_hesitancy, correct=FALSE)
chisq.test(test_data$circle_flu_symptoms,test_data$vaccine_hesitancy, correct=FALSE)

###flu risk perception

chisq.test(test_data$flu_gravity,test_data$vaccine_hesitancy, correct=FALSE)
chisq.test(test_data$flu_fatality_guess,test_data$vaccine_hesitancy, correct=FALSE)
chisq.test(test_data$flu_pandemic_influence,test_data$vaccine_hesitancy, correct=FALSE)
chisq.test(test_data$flu_hospitalized,test_data$vaccine_hesitancy, correct=FALSE)


###flu vaccine efficacy

chisq.test(test_data$vacc_efficacy,test_data$vaccine_hesitancy, correct=FALSE)
chisq.test(test_data$vacc_sides_worry,test_data$vaccine_hesitancy, correct=FALSE)

###flu preventive treatment awareness

chisq.test(test_data$chinese_medicine,test_data$vaccine_hesitancy, correct=FALSE)
chisq.test(test_data$exercise_regularly,test_data$vaccine_hesitancy, correct=FALSE)
chisq.test(test_data$weekly_exercise,test_data$vaccine_hesitancy, correct=FALSE)

###socio-demographic

chisq.test(test_data$gender,test_data$vaccine_hesitancy, correct=FALSE)
chisq.test(test_data$average_monthly_income,test_data$vaccine_hesitancy, correct=FALSE)

###season
chisq.test(test_data$season,test_data$vaccine_hesitancy, correct=FALSE)


##################################################
# model prediction-
# using the logistic regression
##################################################

#preparing the data set for model building
merged_fludata <- young_ppl

#hesitance
merged_fludata$vaccine_hesitancy <- 1
merged_fludata$vaccine_hesitancy<- 
  ifelse(merged_fludata$vacc_acceptance>=6 & merged_fludata$vacc_willingness_next_yr>=2,
         0,merged_fludata$vaccine_hesitancy)
merged_fludata$vaccine_hesitancy[is.na(merged_fludata$vaccine_hesitancy)]<-1

#change the data in to the value prediction need
merged_fludata$vacc_care_ge2 <- ifelse(merged_fludata$vacc_care>=2,1,0)
merged_fludata$vacc_timing_g1 <- ifelse(merged_fludata$vacc_timing>1,1,0)
merged_fludata$circle_vaccinated_ge2 <- ifelse(merged_fludata$circle_vaccinated>=2,1,0)
merged_fludata$flu_disturbance_ge2 <- ifelse(merged_fludata$flu_disturbance>=2,1,0)
merged_fludata$own_flu_symptoms_ge3 <- ifelse(merged_fludata$own_flu_symptoms>=3,1,0)
merged_fludata$flu_gravity_ge2 <- ifelse(merged_fludata$flu_gravity>=2,1,0)
merged_fludata$flu_fatality_guess_ge3 <- ifelse(merged_fludata$flu_fatality_guess>=3,1,0)
merged_fludata$flu_pandemic_influence_ge2 <- ifelse(merged_fludata$flu_pandemic_influence>=2,1,0)
merged_fludata$flu_hospitalized_T <- merged_fludata$flu_hospitalized
merged_fludata$vacc_efficacy_g0 <- ifelse(merged_fludata$vacc_efficacy>0,1,0)
merged_fludata$vacc_sides_worry_ge2 <- ifelse(merged_fludata$vacc_sides_worry>=2,1,0)
merged_fludata$chinese_medicine_ge1 <- ifelse(merged_fludata$chinese_medicine>=1,1,0)
merged_fludata$exercise_regularly_ge2   <- ifelse(merged_fludata$exercise_regularly>=2,1,0)
merged_fludata$weekly_exercise_ge3  <- ifelse(merged_fludata$weekly_exercise>=3,1,0)

write.csv2(merged_fludata, file = "D:\\Flu vaccine hesitancy\\New folder\\merged_fludata.csv")

merged_fludata <- read.csv2 (file = "data/merged_fludata.csv", 
                             header = TRUE, sep = ",")

merged_fludata$vacc_care_ge2 <- factor(merged_fludata$vacc_care_ge2,levels=c("0","1"))
merged_fludata$vacc_timing_g1 <- factor(merged_fludata$vacc_timing_g1,levels=c("0","1"))
merged_fludata$circle_vaccinated_ge2 <- factor(merged_fludata$circle_vaccinated,levels=c("0","1"))
merged_fludata$flu_disturbance_ge2 <- factor(merged_fludata$flu_disturbance_ge2,levels=c("0","1"))
merged_fludata$own_flu_symptoms_ge3 <- factor(merged_fludata$own_flu_symptoms_ge3,levels=c("0","1"))
merged_fludata$flu_gravity_ge2 <- factor(merged_fludata$flu_gravity_ge2,levels=c("0","1"))
merged_fludata$flu_fatality_guess_ge3 <- factor(merged_fludata$flu_fatality_guess_ge3,levels=c("0","1"))
merged_fludata$flu_pandemic_influence_ge2 <- factor(merged_fludata$flu_pandemic_influence_ge2,levels=c("0","1"))
merged_fludata$flu_hospitalized_T <- factor(merged_fludata$flu_hospitalized_T, levels=c("FALSE","TRUE"))
merged_fludata$vacc_efficacy_g0 <- factor(merged_fludata$vacc_efficacy_g0,levels=c("0","1"))
merged_fludata$vacc_sides_worry_ge2 <- factor(merged_fludata$vacc_sides_worry_ge2,levels=c("0","1"))
merged_fludata$chinese_medicine_ge1 <- factor(merged_fludata$chinese_medicine_ge1,levels=c("0","1"))
merged_fludata$exercise_regularly_ge2   <- factor(merged_fludata$exercise_regularly_ge2,levels=c("0","1"))
merged_fludata$weekly_exercise_ge3  <- factor(merged_fludata$weekly_exercise_ge3,levels=c("0","1"))
merged_fludata$vaccine_hesitancy  <- factor(merged_fludata$vaccine_hesitancy,levels=c("0","1"))

##separate the data set to two seasons
season20192020 <- merged_fludata[merged_fludata$season == "2019/20", ]
season20202021 <- merged_fludata[merged_fludata$season == "2020/21", ]


####################
#overall
round_all <- merged_fludata
left_col <- c( "vaccine_hesitancy", "vacc_care_ge2", "vacc_timing_g1",
               "circle_vaccinated_ge2","flu_disturbance_ge2",
               "own_flu_symptoms_ge3","flu_gravity_ge2","flu_fatality_guess_ge3",
               "flu_pandemic_influence_ge2","flu_hospitalized_T",
               "vacc_efficacy_g0","vacc_sides_worry_ge2","chinese_medicine_ge1",
               "exercise_regularly_ge2","weekly_exercise_ge3","season")
data_all_hes <- round_all[,(names(round_all) %in% left_col)]
data_all_hes   <- na.omit(data_all_hes)

write.csv2(data_all_hes, file = "D:\\Flu vaccine hesitancy\\New folder\\data_all_hes.csv")

model_all_hes  <- glm(vaccine_hesitancy ~ ., family=binomial, 
                      data=data_all_hes )
model_all_aic_hes <- stepAIC(model_all_hes )
summary(model_all_aic_hes)

#plot(model_all_aic_hes)
hesi_total <- glm(vaccine_hesitancy ~ vacc_care_ge2 + circle_vaccinated_ge2 + 
                    flu_disturbance_ge2 + flu_gravity_ge2 + flu_fatality_guess_ge3 + 
                    vacc_efficacy_g0 + vacc_sides_worry_ge2 + weekly_exercise_ge3, 
                  family = binomial, data = data_all_hes)
summary(hesi_total)

###logistic regression model with indicator function
data_all_hes$indicator<-ifelse(data_all_hes$season=="2020/21",1,0)
hesi_total_ind <- glm(vaccine_hesitancy ~ vacc_care_ge2+vacc_care_ge2*indicator + circle_vaccinated_ge2 +circle_vaccinated_ge2*indicator+ 
                        flu_gravity_ge2+flu_gravity_ge2*indicator + vacc_efficacy_g0 +vacc_efficacy_g0*indicator+ 
                        vacc_sides_worry_ge2+vacc_sides_worry_ge2*indicator + flu_disturbance_ge2 +flu_disturbance_ge2*indicator
                      + flu_fatality_guess_ge3 + flu_fatality_guess_ge3*indicator+
                        weekly_exercise_ge3+weekly_exercise_ge3*indicator, 
                      family = binomial, data = data_all_hes)
summary(hesi_total_ind)
library(coefplot)
coefplot(hesi_total_ind)

#season 20192020
hesi_12 <- glm(vaccine_hesitancy ~ vacc_care_ge2 + circle_vaccinated_ge2 + 
                 flu_disturbance_ge2 + flu_gravity_ge2 + flu_fatality_guess_ge3 + 
                 vacc_efficacy_g0 + vacc_sides_worry_ge2 + weekly_exercise_ge3, 
               family = binomial, data = season20192020)
summary(hesi_12)
logistic.display(hesi_12)
coefplot(hesi_12)

#season 20202021
hesi_3 <- glm(vaccine_hesitancy ~ vacc_care_ge2 + circle_vaccinated_ge2 + 
                flu_disturbance_ge2 + flu_gravity_ge2 + flu_fatality_guess_ge3 + 
                vacc_efficacy_g0 + vacc_sides_worry_ge2 + weekly_exercise_ge3, family = binomial(link=logit),na.action=na.omit, data = season20202021)
summary(hesi_3)
logistic.display(hesi_3)
coefplot(hesi_3)

multiplot(hesi_12, hesi_3)
season_1=hesi_12
season_2=hesi_3
multiplot(season_1,season_2)

########co-efficient plot

library(modelsummary)
coef_1=modelplot(season_1,coef_omit = 'Interc')
coef_2=modelplot(season_2,coef_omit = 'Interc')

models <- list("season_2" = glm(vaccine_hesitancy ~ vacc_care_ge2 + circle_vaccinated_ge2 + 
                                  flu_disturbance_ge2 + flu_gravity_ge2 + flu_fatality_guess_ge3 + 
                                  vacc_efficacy_g0 + vacc_sides_worry_ge2 + weekly_exercise_ge3, family = binomial, data = season20202021),
               "season_1" = glm(vaccine_hesitancy ~ vacc_care_ge2 + circle_vaccinated_ge2 + 
                                  flu_disturbance_ge2 + flu_gravity_ge2 + flu_fatality_guess_ge3 + 
                                  vacc_efficacy_g0 + vacc_sides_worry_ge2 + weekly_exercise_ge3, 
                                family = binomial, data = season20192020))

modelsummary(models, statistic = 'conf.int')

p1=modelplot(models, conf_level = .95,coef_rename = c("vacc_care_ge2"="vacc_care","circle_vaccinated_ge2"="circle_vaccinated",
                                                      "flu_gravity_ge2"="flu_gravity","vacc_efficacy_g0"="vacc_efficacy",
                                                      "vacc_sides_worry_ge2"="vacc_sides_worry","weekly_exercise_ge3"="weekly_exercise",
                                                      "flu_disturbance_ge2"="flu_disturbance","flu_fatality_guess_ge3"="flu_fatality_guess"), coef_omit = 'Interc')+
  labs(x = 'Coefficient estimates with 95% confidence int.', 
       y = 'Vaiables',
       size = 18, fatten = .7,aspect_ratio=1 ) 

# displaying odds ratio using a log scale


p2=modelplot(models, conf_level = 0.95, coef_rename = c("vacc_care_ge2"="vacc_care","circle_vaccinated_ge2"="circle_vaccinated",
                                                        "flu_gravity_ge2"="flu_related_knowledge","vacc_efficacy_g0"="vacc_efficacy",
                                                        "vacc_sides_worry_ge2"="vacc_sides_worry","weekly_exercise_ge3"="weekly_exercise",
                                                        "flu_disturbance_ge2"="flu_disturbance","flu_fatality_guess_ge3"="flu_fatality_guess"),
             coef_omit = 'Interc', exponentiate = TRUE) +
  scale_x_log10()+geom_vline(xintercept=1,color="darkgrey")+
  xlab("Odds Ratios with 95% confidence int.")+ 
  labs(y = 'Vaiables') + theme(legend.position = "bottom") 
p2

library(patchwork)
combined <- p1 + p2 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

exp(cbind("Odds ratio" = coef(hesi_12), confint.default(hesi_12, level = 0.95)))
exp(cbind("Odds ratio" = coef(hesi_3), confint.default(hesi_3, level = 0.95)))