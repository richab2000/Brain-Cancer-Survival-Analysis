#Richa Bhattacharya
#Mentor David Cohn
#Summary Statistics and Survival Analysis for Brain Cancer Survival Data

#Setting the working directory
code.directory = "BrainCancerSurvivalAnalysis"
setwd = code.directory

#Loading libraries needed
library(ggplot2)
library(dplyr)
library(survival)
library(survminer)

#TO DO: Alter code to read the CSV file 
#dataextract <- read.csv(file = "dataextract")
dataextractCopy <- originaldataextract

#Count of NA Values 
NAValues <- data.frame(NA.count = colSums(is.na(dataextractCopy)))

#Merge same Medical Facilities
dataextractCopy$Medical.Facility = trimws(dataextractCopy$Medical.Facility)

dataextractCopy$Medical.Facility[dataextractCopy$Medical.Facility == "St. Joseph's Hospital AZ" |
dataextractCopy$Medical.Facility == "St. Joseph's Hospital (AZ)"] = "St. Joseph's Hospital (AZ)"

dataextractCopy$Medical.Facility[dataextractCopy$Medical.Facility == "University of North Carolina" | 
dataextractCopy$Medical.Facility == "UNC"] = "University of North Carolina"

dataextractCopy$Medical.Facility[dataextractCopy$Medical.Facility == "MD Anderson" | 
dataextractCopy$Medical.Facility == "MD Anderson Cancer Center"] = "MD Anderson"

dataextractCopy$Medical.Facility[dataextractCopy$Medical.Facility == "Case Western" | 
dataextractCopy$Medical.Facility == "Case Western - St Joes"] = "Case Western"

dataextractCopy$Medical.Facility[dataextractCopy$Medical.Facility == "Fondazione-Besta" |
dataextractCopy$Medical.Facility == "Milan - Italy, Fondazione IRCCS Instituto Neuroligico C. Besta"] = "Fondazione-Besta"

dataextractCopy$Medical.Facility[dataextractCopy$Medical.Facility == "UCSF" |
dataextractCopy$Medical.Facility == "University of California San Francisco"] = "University of California San Francisco"


#Age Mean and Median by Grade 
age.grade.mean.summary = dataextractCopy %>%
  filter(!(is.na(Grade)) & !(is.na(Age.at.diagnosis))) %>%
  group_by(Grade) %>%
  summarise(Mean.Age = mean(Age.at.diagnosis))

age.grade.median.summary = dataextractCopy %>%
  filter(!(is.na(Grade)) & !(is.na(Age.at.diagnosis))) %>%
  group_by(Grade) %>%
  summarise(Median.Age = median(Age.at.diagnosis))

age.grade.summary = inner_join(age.grade.median.summary, age.grade.mean.summary,
                               by = "Grade")


#Age Mean and Median by Medical Facility 
age.mean.medical.facility = dataextractCopy %>%
  filter(!(is.na(Age.at.diagnosis)) & !(is.na(Medical.Facility))) %>%
  group_by(Medical.Facility) %>%
  summarise(Mean.Age = mean(Age.at.diagnosis))

age.median.medical.facility = dataextractCopy %>%
  filter(!(is.na(Age.at.diagnosis)) & !(is.na(Medical.Facility))) %>%
  group_by(Medical.Facility) %>%
  summarise(Median.Age = median(Age.at.diagnosis))

age.medical.facility.summary = inner_join(age.mean.medical.facility, age.median.medical.facility,
                                          by = "Medical.Facility")

#Age Distribution for Grade 2 Tumor
G2Age <- qplot(select(filter(select(dataextractCopy, Age.at.diagnosis, Grade),
Grade == "G2"), Age.at.diagnosis),geom = "histogram", 
binwidth = 0.5, main = "Histogram for G2", xlab = "Age",
fill = I("blue"), col = I("black"),alpha = I(.2), xlim = c(0,100))


#Age Distribution for Grade 3 Tumor 
G3Age <- qplot(select(filter(select(dataextractCopy, Age.at.diagnosis, Grade),
Grade == "G3"), Age.at.diagnosis),geom = "histogram", 
binwidth = 0.5, main = "Histogram for G3", xlab = "Age",
fill = I("blue"), col = I("black"),alpha = I(.2), xlim = c(0,100))


#Age Distribution for Grade 4 Tumor
G4Age <- qplot(select(filter(select(dataextractCopy, Age.at.diagnosis, Grade), 
Grade == "G4"), Age.at.diagnosis),geom = "histogram",
binwidth = 0.5, main = "Histogram for G4", xlab = "Age", 
fill = I("blue"), col = I("black"),alpha = I(.2), xlim = c(0,100))

#Total Count of each of the studies 
study.data <- summarize(group_by(dataextractCopy,Study), Count = n())

#Glioblastoma multiforme per Facility
study.gbm <- data.frame(summarize(group_by(filter(dataextractCopy,
Study == "Glioblastoma multiforme"), Medical.Facility), GBM = n()))

#Brain Lower Grade Glioma
study.lgg <- data.frame(summarize(group_by(filter(dataextractCopy,
Study == "Brain Lower Grade Glioma"), Medical.Facility), LGG= n()))


#Dataframes of # of each grade per Histology
grade2 <- data.frame(summarize(group_by(filter(dataextractCopy,
Grade == "G2"), Histology), G2= n()))
grade3 <-data.frame(summarize(group_by(filter(dataextractCopy,
Grade == "G3"), Histology), G3= n()))
grade4 <-data.frame(summarize(group_by(filter(dataextractCopy,
Grade == "G4"), Histology), G4= n()))

#combined graph of the above dataframes
# Could not find a convenient way to join all graphs since G4 is only glioblastoma and 
#     G3 and G2 are a combination of astrocytoma, oligodendroglioma, oligoastrocytoma
GradeData <- data.frame(
  Histology = c("astrocytoma", "oligodendroglioma", "oligoastrocytoma", "glioblastoma"),
  G2 = c(grade2[1,2], grade2[2,2], grade2[3,2],0),
  G3 = c(grade3[1,2], grade3[2,2], grade3[3,2],0),
  G4 = c(0,0,0, grade4[1,2]))

#Total Number of Females Per Facility
Female <- data.frame(summarize(group_by(filter(dataextractCopy,
Gender == "female"), Medical.Facility),Females_per_Facility= n()))


#Total Number of Males Per Facility
Male <- data.frame(summarize(group_by(filter(dataextractCopy,
Gender == "male"), Medical.Facility),Males_per_Facility= n()))


#Total Count of Patients per Facility 
Patients <- data.frame(summarize(group_by
(dataextractCopy, Medical.Facility), Count= n()))

#Percent Aneuploidy by Grade 
aneuploidy.grade.mean.summary = dataextractCopy %>%
  filter(!(is.na(Grade)) & !(is.na(Percent.Aneuploidy))) %>%
  group_by(Grade) %>%
  summarise(Mean.Percent = mean(Percent.Aneuploidy))

aneuploidy.grade.median.summary = dataextractCopy %>%
  filter(!(is.na(Grade)) & !(is.na(Percent.Aneuploidy))) %>%
  group_by(Grade) %>%
  summarise(Median.Percent = median(Percent.Aneuploidy))

aneuploidy.grade.summary = inner_join(aneuploidy.grade.mean.summary, aneuploidy.grade.median.summary,
                               by = "Grade")

#Percent Aneuploidy by Histology 
aneuploidy.grade.mean.summary = dataextractCopy %>%
  filter(!(is.na(Histology)) & !(is.na(Percent.Aneuploidy))) %>%
  group_by(Histology) %>%
  summarise(Mean.Percent = mean(Percent.Aneuploidy))

aneuploidy.grade.median.summary = dataextractCopy %>%
  filter(!(is.na(Histology)) & !(is.na(Percent.Aneuploidy))) %>%
  group_by(Histology) %>%
  summarise(Median.Percent = median(Percent.Aneuploidy))

aneuploidy.grade.summary = inner_join(aneuploidy.grade.mean.summary, aneuploidy.grade.median.summary,
by = "Histology")

#Percent Aneuploidy by Gender 
aneuploidy.gender.mean.summary = dataextractCopy %>%
  filter(!(is.na(Gender)) & !(is.na(Percent.Aneuploidy))) %>%
  group_by(Gender) %>%
  summarise(Mean.Percent = mean(Percent.Aneuploidy))

aneuploidy.gender.median.summary = dataextractCopy %>%
  filter(!(is.na(Gender)) & !(is.na(Percent.Aneuploidy))) %>%
  group_by(Gender) %>%
  summarise(Median.Percent = median(Percent.Aneuploidy))

aneuploidy.gender.summary = inner_join(aneuploidy.gender.mean.summary, aneuploidy.gender.median.summary,
by = "Gender")


#Modifying the Survival Data
#Excludes NA, negative / 0 months, caps at followup.threshold
followup.threshold = 36
survival.data = dataextractCopy %>%
filter(!is.na(Survival.Months) & Survival.Months > 0) %>%
mutate(Survival.Months.Threshold = ifelse(Survival.Months > followup.threshold,
    followup.threshold, Survival.Months)) %>%
mutate(Survival.Status.Threshold = ifelse(Survival.Months > followup.threshold &
    Survival.Status == 1, 0, Survival.Status))
#Creating a copy 
KM.dataframe = survival.data

#Kaplan Meier Curves for G2,G3,G4 Tumors 
KM.dataframe$SurvObject = with(KM.dataframe,
Surv(Survival.Months.Threshold, Survival.Status.Threshold))
kaplan.meier.fit = surv_fit(SurvObject ~ Grade, data = KM.dataframe, 
conf.type = "log-log")
KM.plot.grade = ggsurvplot(kaplan.meier.fit, conf.int = TRUE, title = "Kaplan Meier Analysis by Tumor Grade",
xlab = "Months", ylab = "Probability", risk.table = TRUE, break.x.by = 6)
KM.plot.grade$plot = KM.plot.grade$plot + theme(plot.title = element_text(hjust = 0.5)) 
print(KM.plot.grade)

#Kaplan Meier Curves for Histology
KM.dataframe = filter(survival.data, Histology != "glioblastoma")
KM.dataframe$SurvObject = with(KM.dataframe,
Surv(Survival.Months.Threshold, Survival.Status.Threshold))
kaplan.meier.fit = surv_fit(SurvObject ~ Histology, data = KM.dataframe,
conf.type = "log-log")
KM.plot.histology = ggsurvplot(kaplan.meier.fit, conf.int = TRUE, title = "Kaplan Meier Analysis by Histology",
xlab = "Months", ylab = "Probability", risk.table = TRUE, break.x.by = 6)
KM.plot.histology$plot = KM.plot.histology$plot + theme(plot.title = element_text(hjust = 0.5)) 
print(KM.plot.histology)

#Sorting Age Data into greater than / less than 50 (40 for G2)
age.data <- select(dataextractCopy, Grade, Age.at.diagnosis,Survival.Months,Survival.Status)
age.data <-filter(age.data, Survival.Months > 0 &!is.na(Grade) & !is.na(Survival.Months) & 
  !is.na(Age.at.diagnosis) & !is.na(Survival.Status))

#Kaplan Meier Curves for G2,G3,G4 Tumors 
G2 <- filter(age.data, Grade == "G2")
G2 <- mutate(G2,g.or.l = ifelse(Age.at.diagnosis > 40 & Grade == "G2", ">40", "<40"))
G2$g.or.l = as.factor(G2$g.or.l)
G3 <- filter(age.data, Grade == "G3")
G3 <- mutate(G3,g.or.l = ifelse(Age.at.diagnosis > 50 & Grade == "G3", ">40", "<40"))
G3$g.or.l = as.factor(G3$g.or.l)
G4 <- filter(age.data, Grade == "G4")
G4 <- mutate(G4,g.or.l = ifelse(Age.at.diagnosis > 50 & Grade == "G4", ">40", "<40"))
G4$g.or.l = as.factor(G4$g.or.l)

#Kaplan Meier Curves for G2 Age Distribution (greater/less than 40)
SurvObject = with(G2,Surv(G2$Survival.Months, G2$Survival.Status))
kaplan.meier.fit = surv_fit(SurvObject ~ G2$g.or.l, data = G2, 
conf.type = "log-log")
KM.plot.G2 = ggsurvplot(kaplan.meier.fit, conf.int = TRUE, title = "Kaplan Meier Curves for G2",
xlab = "Months", ylab = "Probability", risk.table = TRUE, break.x.by = 6)
KM.plot.G2$plot = KM.plot.G2$plot + theme(plot.title = element_text(hjust = 0.5)) 
KM.plot.G2

#Kaplan Meier Curves for G3 Age Distribution (greater/less than 50)
SurvObject = with(G3,Surv(G3$Survival.Months, G3$Survival.Status))
kaplan.meier.fit = surv_fit(SurvObject ~ G3$g.or.l, data = G3, 
conf.type = "log-log")
KM.plot.G3 = ggsurvplot(kaplan.meier.fit, conf.int = TRUE, title = "Kaplan Meier Curves for G3",
xlab = "Months", ylab = "Probability", risk.table = TRUE, break.x.by = 6)
KM.plot.G3$plot = KM.plot.G3$plot + theme(plot.title = element_text(hjust = 0.5)) 
KM.plot.G3

#Kaplan Meier Curves for G4 Age Distribution (greater/less than 50)
SurvObject = with(G4,Surv(G4$Survival.Months, G4$Survival.Status))
kaplan.meier.fit = surv_fit(SurvObject ~ G4$g.or.l, data = G4, 
conf.type = "log-log")
KM.plot.G4 = ggsurvplot(kaplan.meier.fit, conf.int = TRUE, title = "Kaplan Meier Curves for G4",
xlab = "Months", ylab = "Probability", risk.table = TRUE, break.x.by = 6)
KM.plot.G4$plot = KM.plot.G4$plot + theme(plot.title = element_text(hjust = 0.5)) 
KM.plot.G4

#Karnofsky Performance Score Count 
k.score <- summarize(group_by(dataextractCopy, Karnofsky.Performance.Score), Count = n())

#Mutation Count
qplot(filter(select(dataextractCopy, Mutation.Count), !is.na(Mutation.Count)),geom = "histogram",
binwidth = 0.5, main = "Mutation Count", xlab = "Mutation Count", 
fill = I("blue"), col = I("black"),alpha = I(.2), xlim = c(0,100))

#Karnofsky Score
qplot(filter(select(dataextractCopy, Karnofsky.Performance.Score), !is.na(Karnofsky.Performance.Score)),geom = "histogram",
binwidth = 0.5, main = "Karnofsky Score Histogram", xlab = "Karnofsky Score", 
fill = I("blue"), col = I("black"),alpha = I(.2), xlim = c(0,100))

#Grade v. Karnofsky Score 
#how do you have multiple if statements for three bins
karnofsky.data <- filter(select(dataextractCopy, Grade, Karnofsky.Performance.Score), 
       !is.na(Grade) &!is.na(Karnofsky.Performance.Score))
G2 <- filter(karnofsky.data, Grade == "G2")
G2 <- mutate(G2,g.or.l = ifelse(Karnofsky.Performance.Score > 40 & Grade == "G2", ">40", "<40"))
G2$g.or.l = as.factor(G2$g.or.l)
G3 <- filter(karnofsky.data, Grade == "G3")
G3 <- mutate(G3,g.or.l = ifelse(Karnofsky.Performance.Score > 50 & Grade == "G3", ">40", "<40"))
G3$g.or.l = as.factor(G3$g.or.l)
G4 <- filter(karnofsky.data, Grade == "G4")
G4 <- mutate(G4,g.or.l = ifelse(Karnofsky.Performance.Score > 50 & Grade == "G4", ">40", "<40"))
G4$g.or.l = as.factor(G4$g.or.l)
