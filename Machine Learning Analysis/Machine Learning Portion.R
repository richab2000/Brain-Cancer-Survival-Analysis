#Richa Bhattacharya
#Mentor David Cohn
#Machine learning portion 

code.directory= "Machine Learning Analysis"
setwd = code.directory

# Loading libraries needed
library(ggplot2)
library(dplyr)
library(tidyr)
library(survival)
library(survminer)
library(caret)
library(stats)
library(randomForest)
library(e1071)

# Read in data
data.extract = read.csv('dataextract.csv', stringsAsFactors = FALSE)
#source(paste(code.directory, 'summary_statistics_copy.R', sep = '/'))
#source(paste(code.directory, 'kaplan_meier_analysis_copy.R', sep = '/'))

#the actual value: summarize(filtered.data, mean = mean(Survival.Months))


filtered.data <- filter(data.extract, Survival.Months > 0 & Survival.Status == 1 & !is.na(Medical.Facility)
                        & !is.na(Study) & !is.na(Histology) & !is.na(Grade) & !is.na(Gender) & !is.na(Age.at.diagnosis))
#filtered.data <- mutate(filtered.data,g.or.l = ifelse(Survival.Months > mean(Survival.Months), ">m", "<m"))
#filtered.data$g.or.l = as.factor(filtered.data$g.or.l)
filtered.data <- select(filtered.data, Medical.Facility, Study, Histology, Grade,Gender, Survival.Months)
na.omit(filtered.data)

set.seed(1427)
Parted <- createDataPartition(filtered.data$Survival.Months, times = 1, p = .8, list = FALSE)
surv.train <- filtered.data[Parted,]
surv.test <- filtered.data[-Parted,]

#ignore: hi <- multinom(Survival.Months~., data = surv.train)
#ignore: predict (hi, surv.test)

#Categorical to Numeric
surv.train$Medical.Facility <- as.numeric(factor(surv.train$Medical.Facility))
surv.train$Study <- as.numeric(factor(surv.train$Study))
surv.train$Histology <- as.numeric(factor(surv.train$Histology))
surv.train$Grade <- as.numeric(factor(surv.train$Grade))
surv.train$Gender <- as.numeric(factor(surv.train$Gender))

#Model 
glm<- glm(Survival.Months~Medical.Facility+Study+Histology+Grade+Gender, family = poisson, data = surv.train)
summary(glm)
