setwd("~/Documents/CityU Lab/Flu vaccine hesitancy/New folder")
###Before pandemic data
before_fludata <- read.csv2 (file = "Data/surveys_merged.csv", 
                                 +                              header = TRUE, sep = ",")
###After pandemic data
after_fludata <- read.csv2 (file = "Data/data_20210509.csv", 
                                  +                              header = TRUE, sep = ",")

library(ggplot2)
ggplot(before_fludata, aes(x=survey_date))+geom_histogram(binwidth=.5)
