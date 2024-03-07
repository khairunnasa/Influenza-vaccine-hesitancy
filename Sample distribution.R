setwd("~/Flu vaccine hesitancy")
###Before pandemic data
before_fludata <- read.csv2 (file = "surveys_merged.csv", 
                             header = TRUE, sep = ",")
###After pandemic data
after_fludata <- read.csv2 (file = "data_20210509.csv", 
                            header = TRUE, sep = ",")

install.packages("readxl")
install.packages("hrbrthemes")
install.packages("plotly")
library("readxl")

library(ggplot2)
library(tidyverse)
library(hrbrthemes)
p <- ggplot(Data_distribution, aes(x = survey_date)) + geom_histogram()

library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
dibujo <- ggplot(Data_distribution, aes(x = factor(survey_date), y = Sample_freq)) +
  geom_col(fill = "lightblue", color = "lightblue") +
  labs(x = "survey_date", y = "sample_frequency") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust = 1))
 
dibujo

 
dibujo2 <- ggplot(Data_distribution, aes(x = factor(survey_date), y = Sample_freq, 
                                         fill=factor(Round))) + 
                    geom_bar(stat="identity", position="dodge") +
  labs(x = "survey_date", y = "sample_frequency") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust = 1))

dibujo2
