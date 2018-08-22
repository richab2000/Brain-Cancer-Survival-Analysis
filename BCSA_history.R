colnames(GradeData) <- c(" ","G2","G3","G4")
GradeData <- matrix(c(55,100,61,0,114,74,53,0,0,0,0,590),ncol=4,byrow=TRUE)
rownames(GradeData) <- c(“astrocytoma”, “oligodendroglioma”, “oligoastrocytoma”, “glioblastoma”)
colnames(GradeData) <- c(“G2”, “G3”, “G4”)
GradeData <- as.table(GradeData)
GradeData <- matrix(c(55,100,61,0,114,74,53,0,0,0,0,590),ncol=3,byrow=TRUE)
rownames(GradeData) <- c(“astrocytoma”, “oligodendroglioma”, “oligoastrocytoma”, “glioblastoma”)
colnames(GradeData) <- c(“G2”, “G3”, “G4”)
GradeData <- as.table(GradeData)
GradeData <- matrix(c(55,100,61,0,114,74,53,0,0,0,0,590),ncol=3,byrow=TRUE)
rownames(GradeData) <- c("astrocytoma", "oligodendroglioma", "oligoastrocytoma", "glioblastoma")
colnames(GradeData) <- c(" ","G2", "G3", "G4")
GradeData <- as.table(GradeData)
GradeData <- matrix(c(55,100,61,0,114,74,53,0,0,0,0,590),ncol=3,byrow=TRUE)
rownames(GradeData) <- c("astrocytoma", "oligodendroglioma", "oligoastrocytoma", "glioblastoma")
colnames(GradeData) <- c(G2", "G3", "G4")
GradeData <- as.table(GradeData)
colnames(GradeData) <- c("G2", "G3", "G4")
GradeData <- as.table(GradeData)
GradeData <- matrix(c(55,100,61,0,114,74,53,0,0,0,0,590),ncol=3,byrow=TRUE)
rownames(GradeData) <- c("astrocytoma", "oligodendroglioma", "oligoastrocytoma", "glioblastoma")
colnames(GradeData) <- c("G2", "G3", "G4")
GradeData <- as.table(GradeData)
colnames(GradeData) <- c("G2", "G3", "G4")
> GradeData <- as.table(GradeData)
GradeData <- matrix(c(g2a,g2o,g2ao,g2g,g3a,g3o,g3ao,g3g,g4a,g4o,g4ao,g4g),ncol=3,byrow=TRUE)
rownames(GradeData) <- c("astrocytoma", "oligodendroglioma", "oligoastrocytoma", "glioblastoma")
colnames(GradeData) <- c("G2", "G3", "G4")
GradeData <- as.table(GradeData)
col_headings <- c('astrocytoma', 'o','ao', 'g')
names(G2) <- col_headings
colNames <- c('G2','G3','G4')
names(GradeData) <- colNames
GradeData <- data.frame(
g2 = c(g2a, g2o, g2ao, g2g)
g3 = c(g3a, g3o, g3ao, g3g)
g4 = c(g4a, g4o, g4ao, g4g)
)
GradeData <- data.frame(grade2,grade3,grade4)
View(GradeData)
row.names(GradeData) <- ("A", "O", "AO", "G")
GradeData <- dataframe(
Histology = c("Astrocytoma", "Oligodendroglioma", "Oligoastrocytoma", "Glioblastoma"),
grade2,
grade3,
grade4
)
GradeData <- data.frame(
Histology = c("Astrocytoma", "Oligodendroglioma", "Oligoastrocytoma", "Glioblastoma"),
grade2,
)
GradeData <- data.frame(
Histology = c("Astrocytoma", "Oligodendroglioma", "Oligoastrocytoma", "Glioblastoma"),
grade2,
grade3,
grade4
)
View(GradeData)
View(GradeData)
GradeData <- data.frame(
Histology = c("Astrocytoma", "Oligodendroglioma", Glioblastoma", "Oligoastrocytoma"),
grade2,
grade3,
grade4
)
View(GradeData)
GradeData <- data.frame(
Histology = c("Astrocytoma", "Oligodendroglioma","Glioblastoma","Oligoastrocytoma"),
grade2,
grade3,
grade4
)
View(GradeData)
medFac
View(medFac)
try <- filter(dataextract, !is.na(male))
%.%
%>%
male <- filter(dataextract, Gender == "male")
male <- filter(dataextract, Gender == "female")
View(dataextract)
View(dataextract)
View(dataextract)
filter(dataextract, Gender == "male")
warnings()
male <- filter(dataextract, !is.na(Gender) & Gender == "male")
warnings()
library(dplyr)
filter(dataextract, !is.na(Gender) & Gender == "male")
female <- filter(dataextract, !is.na(Gender) & Gender == "male")
male <- filter(dataextract, !is.na(Gender) & Gender == "female")
View(male)
summarize(group_by(male, Medical.Facility), Male = n())
Male <- summarize(group_by(filter(dataextract, !is.na(Gender) & Gender == "female"), Medical.Facility), Male = n())
View(Male)
rm(male)
Female <- summarize(group_by(filter(dataextract, !is.na(Gender) & Gender == "male"), Medical.Facility), Female = n())
View(Female)
rm(female)
View(Female)
View(g2)
View(Female)
left_join(Male, Female)
MedFac <- data.frame(
male
)
MedFac <- data.frame(Male)
mutate(MedFac, Female)
View(MedFac)
rm(MedFac)
medFac <- summarize(group_by(dataextract, Medical.Facility), Count == n())
medFac <- summarize(group_by(dataextract, Medical.Facility), count = n())
View(medFac)
StudyData <- summarize(group_by(dataextract,Study), count = n())
View(StudyData)
val = 0
select(dataextract, Study == "Brain Lower Grade Glioma")
library(dplyr)
filter(dataextract,Study == "Brain Lower Grade Glioma")
summarize(group_by(filter(dataextract, Study == "Brain Lower Grade Glioma"),Medical.Facility), Patients = n())
summarize(group_by(filter(dataextract, Study == "Glioblastoma multiforme"),Medical.Facility), Patients = n())
StudyLGG <- data.frame(
summarize(group_by(filter(dataextract, Study == "Glioblastoma multiforme"),Medical.Facility), Patients = n())
)
View(StudyLGG)
StudyLGG <- data.frame(
summarize(group_by(filter(dataextract, Study == "Glioblastoma multiforme"),Medical.Facility), LGG = n())
)
StudyGM <- data.frame(
summarize(group_by(filter(dataextract, Study == "Glioblastoma multiforme"),Medical.Facility), GM = n())
)
> StudyLGG <- data.frame(
+     summarize(group_by(filter(dataextract, Study == "Brain Lower Grade Glioma"),Medical.Facility), LGG = n())
)
> StudyLGG <- data.frame(
summarize(group_by(filter(dataextract, Study == "Brain Lower Grade Glioma"),Medical.Facility), LGG = n()))
View(StudyLGG)
rm(StudyLGG)
StudyLGG <- data.frame(
summarize(group_by(filter(dataextract, Study == "Brain Lower Grade Glioma"),Medical.Facility), LGG = n())
)
StudyHG <- summarize(group_by(filter(dataextract, Study == "Glioblastoma multiforme"),Medical.Facility), HG = n())
)
StudyHG <- data.frame(
summarize(group_by(filter(dataextract,Study == "Glioblastoma multiforme"), Medical.Facility), HG = n()
)
View(StudyHG)
View(StudyLGG)
View(StudyHG)
View(StudyHG)
StudyGM <- StudyHG
rm(StudyHG)
select(Age)
select(dataextract, Age)
select(dataextract, Age.at.diagnosis)
plot(select(dataextract,Age.at.diagnosis),1:100,main = "Age")
plot(select(dataextract,Age.at.diagnosis))
plot(select(dataextract,Age.at.diagnosis), type = 'o', pch =10, cex = .2)
plot(select(dataextract,Age.at.diagnosis), main="title", sub="subtitle",
xlab="X-axis label", ylab="y-axix label",
xlim=c(0, 100), ylim=c(1, ymax))
dotchart(dataextract$Age.at.diagnosis),labels=row.names(dataextract),cex=.7,
main="Age Range",
xlab="Age")
mean = mean(dataextract$Age.at.diagnosis)
mean
filter(dataextract$Age.at.diagnosis, !is.na(Age.at.diagnosis))
dataextract$Age.at.diagnosis
sum = 0
count = 0
for(val in 1:nrow(dataextract$Age.at.diagnosis))
{
if (!is.na(Age.at.diagnosis[val]))
{
sum = sum + Age.at.diagnosis[val]
count = count + 1
}
}
ageAnalysis = dataextract$Age.at.diagnosis
ageAnalysis <- select(dataextract,Age.at.diagnosis)
hist(ageAnalysis,
right = FALSE)
library(ggplot2)
qplot(ageAnalysis, geom = "histogram")
age <- select(dataextract, Grade, Age.at.diagnosis)
View(age)
G2Age <- filter(select(dataextract, Grade, Age.at.diagnosis), Grade == "G2")
View(G2Age)
rm(age)
View(ageAnalysis)
rm(ageAnalysis)
View(g2)
View(StudyGM)
qplot(G2Age, geom = "histogram")
qplot(G2Age,
geom="histogram",
binwidth = 0.5,
main = "Histogram for Age",
xlab = "Age",
fill=I("blue"),
col=I("red"),
alpha=I(.2),
xlim=c(20,50))
View(G2Age)
qplot(select(dataextract,Age.at.diagnosis),
geom="histogram",
binwidth = 0.5,
main = "Histogram for Age",
xlab = "Age",
fill=I("blue"),
col=I("red"),
alpha=I(.2),
xlim=c(20,50))
qplot(select(dataextract, Age.at.diagnosis),
geom="histogram",
binwidth = 0.5,
main = "Age Distribution for G2",
xlab = "Age",
fill=I("blue"),
col=I("black"),
alpha=I(.2),
xlim=c(0,100))
select(dataextract,filter(dataextract,Grade == "G2"))
select(dataextract$Age.at.diagnosis, filter(dataextract, Grade == "G2"))
dataextract$Age.at.diagnosis
dataextract$Age.at.diagnosis$Grade
select(dataextract, Age.at.diagnosis, Grade)
filter(select(dataextract, Age.at.diagnosis, Grade), Grade == "G2")
qplot(select(filter(select(dataextract, Age.at.diagnosis, Grade), Grade == "G2"), Age.at.diagnosis),
geom="histogram",
binwidth = 0.5,
main = "Histogram for G2",
xlab = "Age",
fill=I("blue"),
col=I("black"),
alpha=I(.2),
xlim=c(0,100))
G2Age <- qplot(select(filter(select(dataextract, Age.at.diagnosis, Grade), Grade == "G2"), Age.at.diagnosis),geom = "histogram", binwidth = 0.5, main = "Histogram for G2", xlab = "Age", fill = I("blue"), col = I("black"),alpha = I(.2), xlim = c(0,100))
G2Age
G3Age <- qplot(select(filter(select(dataextract, Age.at.diagnosis, Grade), Grade == "G3"), Age.at.diagnosis),geom = "histogram", binwidth = 0.5, main = "Histogram for G2", xlab = "Age", fill = I("blue"), col = I("black"),alpha = I(.2), xlim = c(0,100))
G3Age
G3Age <- qplot(select(filter(select(dataextract, Age.at.diagnosis, Grade), Grade == "G3"), Age.at.diagnosis),geom = "histogram", binwidth = 0.5, main = "Histogram for G3", xlab = "Age", fill = I("blue"), col = I("black"),alpha = I(.2), xlim = c(0,100))
G3Age
G4Age <- qplot(select(filter(select(dataextract, Age.at.diagnosis, Grade), Grade == "G4"), Age.at.diagnosis),geom = "histogram", binwidth = 0.5, main = "Histogram for G4", xlab = "Age", fill = I("blue"), col = I("black"),alpha = I(.2), xlim = c(0,100))
G4Age
rm(g3na)
rm(g2na)
rm(g4na)
rm(g4)
View(StudyLGG)
View(StudyGM)
View(StudyData)
View(medFac)
View(Male)
View(histGrade)
View(histFac)
View(histGrade)
View(histFac)
View(GradeData)
rm(histFac)
GradeDate
rm(GradeDate)
GradeData
rm(mean)
rm(sum)
sum = 0
count = 0
for (int val in 1:nrow(select(filter(select(dataextract, Age.at.diagnosis, Grade), Grade == "G4"), Age.at.diagnosis))
{
sum = sum + Age[val]
count = count + 1
}
for (val in 1:nrow(select(filter(select(dataextract, Age.at.diagnosis, Grade), Grade == "G4"), Age.at.diagnosis))
{
sum = sum + Age.at.diagnosis[val]
count = count + 1
}
(select(filter(select(dataextract, Age.at.diagnosis, Grade), Grade == "G4"), Age.at.diagnosis))
mean = mean(Age.at.diagnosis)
mean(dataextract, Age.at.diagnosis)
mean(dataextract$Age.at.diagnosis)
View(dataextract)
val = 0
sum = 0
count =0
for (val in 1:nrow(dataextract))
{
if (!is.na(dataextract[val,5] & dataextract[val,4] == "G2")
{
sum = sum + dataextract[val,5]
count = count + 1
}
}
View(sum)
rm(sum)
val = as.integer(0)
val = 0
sum = 0
count = 0
for (val in 1:nrow(dataextract))
{
if (!is.na(dataextract[val,5]) & dataextract[val,4] == "G2")
{
sum = sum + dataextract[val,5]
count = count + 1
}
}
sum
for (val in 1:nrow(dataextract))
{
if (!is.na(dataextract[val,5]) & dataextract[val,4] == "G2")
{
sum = sum + as.integer(dataextract[val,5])
count = count + 1
}
}
sum
17464/2
count = 0
val = 0
sum = 0
summ
sum
val
for (val in 1:nrow(dataextract))
{
if (!is.na(dataextract[val,5]) & dataextract[val,4] == "G2")
{
sum = sum + as.integer(dataextract[val,5])
count = count + 1
}
}
g2mean = sum / count
g2mean
val = 0
count = 0
sum = 0
for (val in 1:nrow(dataextract))
{
if (!is.na(dataextract[val,5]) & dataextract[val,4] == "G3")
{
sum = sum + as.integer(dataextract[val,5])
count = count + 1
}
}
g3mean = sum / count
g3mean
val = 0
count = 0
sum = 0
for (val in 1:nrow(dataextract))
{
if (!is.na(dataextract[val,5]) & dataextract[val,4] == "G4")
{
sum = sum + as.integer(dataextract[val,5])
count = count + 1
}
}
g4mean = sum / count
g4mean
AgevGrade <- data.frame(
Grade = c("G2", "G3", "G4"),
Mean Age = c(g2mean, g3mean, g4mean)
)
AgevGrade <- data.frame(
Grade = c("G2", "G3", "G4"),
Mean_Age = c(g2mean, g3mean, g4mean)
)
AgevGrade
View(AgevGrade)
col_headings
rm(col_headings)
sum = 0
val = 0
count = 0
colNames
View(StudyLGG)
View(StudyGM)
View(StudyData)
View(medFac)
View(histGrade)
View(GradeData)
View(Male)
View(AgevGrade)
View(g2)
rm(g2)
View(GradeData)
View(g3)
rm(g3)
rm(colNames)
View(GradeData)
View(StudyLGG)
View(AgevGrade)
View(AgevGrade)
select(dataextract, Grade, Age.at.diagnosis)
filter(select(dataextract, Grade, Age.at.diagnosis), Grade == "G2")
filter(filter(select(dataextract, Grade, Age.at.diagnosis), Grade == "G2"),!is.na(Age.at.diagnosis))
median( filter(filter(select(dataextract, Grade, Age.at.diagnosis), Grade == "G2"),!is.na(Age.at.diagnosis)))
select(filter(filter(select(dataextract, Grade, Age.at.diagnosis), Grade == "G2"),!is.na(Age.at.diagnosis)),Age.at.diagnosis)
median(select(filter(filter(select(dataextract, Grade, Age.at.diagnosis), Grade == "G2"),!is.na(Age.at.diagnosis)),Age.at.diagnosis))
hi =  c(1,2,3)
mean(hi)
median(hi)
median(select(filter(filter(select(dataextract, Grade, Age.at.diagnosis), Grade == "G2"),!is.na(Age.at.diagnosis)),Age.at.diagnosis))
G2Age
val = 0
count = 0
G2Age
G3Age
G4Age
G3Age
g3
G2Age
G3Age
G2Age
G4Age
View(AgevGrade)
View(dataextract)
View(Female)
View(GradeData)
View(histGrade)
View(histGrade)
rm(histGrade)
View(medFac)
View(AgevGrade)
View(Male)
View(GradeData)
View(Male)
View(medFac)
View(StudyData)
View(StudyGM)
View(StudyLGG)
View(AgevGrade)
load("~/BrainCancerSurvivalAnalysis/.RData")
load("~/BrainCancerSurvivalAnalysis/.RData")
select(dataextract, Age.at.diagnosis)
select(dataextract,Medical.Facility,Age.at.diagnosis)
by_fac <- group_by(select(dataextract,Medical.Facility,Age.at.diagnosis),Medical.Facility)
by_fac
summarize(by_fac, mean = mean(Age.at.diagnosis,na.rm = TRUE)
View(by_fac)
View(by_fac)
summarize(by_fac, mean = mean(Age.at.diagnosis,na.rm = TRUE)
summarize(by_fac, mean = mean(Age.at.diagnosis,na.rm = TRUE)
by_fac <- group_by(select(dataextract,Medical.Facility,Age.at.diagnosis),Medical.Facility)
View(by_fac)
rm.na(by_fac)
na.rm(by_fac)
%>%
select(dataextract,Medical.Facility,Age.at.diagnosis)
filter(select(dataextract,Medical.Facility,Age.at.diagnosis), !is.na(Age.at.diagnosis))
summarize(group_by(filter(select(dataextract,Medical.Facility,Age.at.diagnosis), !is.na(Age.at.diagnosis)),Medical.Facility), mean = mean(Age.at.diagnosis))
rm(by_fac)
FacilityvAge <- summarize(group_by(filter(select(dataextract,Medical.Facility,Age.at.diagnosis), !is.na(Age.at.diagnosis)),Medical.Facility), Average Age = mean(Age.at.diagnosis))
summarize(group_by(filter(select(dataextract,Medical.Facility,Age.at.diagnosis), !is.na(Age.at.diagnosis)),Medical.Facility), mean = mean(Age.at.diagnosis))
FacilityvAge <- summarize(group_by(filter(select(dataextract,Medical.Facility,Age.at.diagnosis), !is.na(Age.at.diagnosis)),Medical.Facility), mean = mean(Age.at.diagnosis))
FacilityvAge <- summarize(group_by(filter(select(dataextract,Medical.Facility,Age.at.diagnosis), !is.na(Age.at.diagnosis)),Medical.Facility), Average_Age = mean(Age.at.diagnosis))
View(FacilityvAge)
View(g2)
rm(g3)
rm(g2)
View(histFac)
rm(histFac)
View(AgevGrade)
summarize(group_by(filter(select(dataextract,Medical.Facility,Age.at.diagnosis), !is.na(Age.at.diagnosis)),Medical.Facility), Median_Age = median(Age.at.diagnosis))
select(summarize(group_by(filter(select(dataextract,Medical.Facility,Age.at.diagnosis), !is.na(Age.at.diagnosis)),Medical.Facility), Median_Age = median(Age.at.diagnosis)), Median_Age)
mutate(FacilityvAge,select(summarize(group_by(filter(select(dataextract,Medical.Facility,Age.at.diagnosis), !is.na(Age.at.diagnosis)),Medical.Facility), Median_Age = median(Age.at.diagnosis)), Median_Age) )
median <- select(summarize(group_by(filter(select(dataextract,Medical.Facility,Age.at.diagnosis), !is.na(Age.at.diagnosis)),Medical.Facility), Median_Age = median(Age.at.diagnosis)), Median_Age)
View(median)
mutate(FacilityvAge, median)
FacilityvAge[, "Median_Age"] <- select(summarize(group_by(filter(select(dataextract,Medical.Facility,Age.at.diagnosis), !is.na(Age.at.diagnosis)),Medical.Facility), Median_Age = median(Age.at.diagnosis)
View(FacilityvAge)
FacilityvAge[, "Median_Age"] <- select(summarize(group_by(filter(select(dataextract,Medical.Facility,Age.at.diagnosis), !is.na(Age.at.diagnosis)),Medical.Facility), Median_Age = median(Age.at.diagnosis)
select(summarize(group_by(filter(select(dataextract,Medical.Facility,Age.at.diagnosis), !is.na(Age.at.diagnosis)),Medical.Facility), Median_Age = median(Age.at.diagnosis)
select(summarize(group_by(filter(select(dataextract,Medical.Facility,Age.at.diagnosis), !is.na(Age.at.diagnosis)),Medical.Facility), Median_Age = median(Age.at.diagnosis)
dsf
select(summarize(group_by(filter(select(dataextract,Medical.Facility,Age.at.diagnosis), !is.na(Age.at.diagnosis)),Medical.Facility), Median_Age = median(Age.at.diagnosis)
summarize(group_by(filter(select(dataextract,Medical.Facility,Age.at.diagnosis), !is.na(Age.at.diagnosis)),Medical.Facility), Median_Age = median(Age.at.diagnosis)
median <- select(summarize(group_by(filter(select(dataextract,Medical.Facility,Age.at.diagnosis), !is.na(Age.at.diagnosis)),Medical.Facility), Median_Age = median(Age.at.diagnosis)), Median_Age)
median
summarize(group_by(filter(select(dataextract,Medical.Facility,Age.at.diagnosis), !is.na(Age.at.diagnosis)),Medical.Facility), Median_Age = median(Age.at.diagnosis))
join(FacilityvAge, median)
inner_join(FacilityvAge, summarize(group_by(filter(select(dataextract,Medical.Facility,Age.at.diagnosis), !is.na(Age.at.diagnosis)),Medical.Facility), Median_Age = median(Age.at.diagnosis)))
FacilityvAge <- inner_join(FacilityvAge, summarize(group_by(filter(select(dataextract,Medical.Facility,Age.at.diagnosis), !is.na(Age.at.diagnosis)),Medical.Facility), Median_Age = median(Age.at.diagnosis)))
View(FacilityvAge)
rm(median)
View(Female)
innerjoin(Female, Male)
inner_join(Female, Male)
View(StudyGM)
View(StudyLGG)
View(StudyData)
View(medFac)
View(Male)
View(histGrade)
View(GradeData)
View(FacilityvAge)
View(dataextract)
View(AgevGrade)
summarize(group_by(filter(select(dataextract,Grade,Age.at.diagnosis), !is.na(Age.at.diagnosis & Grade)),Grade), Median_Age = median(Age.at.diagnosis))
View(dataextract)
summarize(group_by(filter(select(dataextract,Grade,Age.at.diagnosis), !is.na(Age.at.diagnosis) & !is.na(Grade)),Grade), mean = mean(Age.at.diagnosis))
summarize(group_by(filter(select(dataextract,Grade,Age.at.diagnosis), !is.na(Age.at.diagnosis) & !is.na(Grade)),Grade), Average_Age = mean(Age.at.diagnosis))
summarize(group_by(filter(select(dataextract,Grade,Age.at.diagnosis), !is.na(Age.at.diagnosis) & !is.na(Grade)),Grade), Median_age = median(Age.at.diagnosis))
inner_join(summarize(group_by(filter(select(dataextract,Grade,Age.at.diagnosis), !is.na(Age.at.diagnosis) & !is.na(Grade)),Grade), Average_Age = mean(Age.at.diagnosis)),summarize(group_by(filter(select(dataextract,Grade,Age.at.diagnosis), !is.na(Age.at.diagnosis) & !is.na(Grade)),Grade), Median_age = median(Age.at.diagnosis)))
View(GradeData)
View(StudyData)
View(AgevGrade)
AgevGrade <- inner_join(summarize(group_by(filter(select(dataextract,Grade,Age.at.diagnosis), !is.na(Age.at.diagnosis) & !is.na(Grade)),Grade), Average_Age = mean(Age.at.diagnosis)),summarize(group_by(filter(select(dataextract,Grade,Age.at.diagnosis), !is.na(Age.at.diagnosis) & !is.na(Grade)),Grade), Median_age = median(Age.at.diagnosis)))
View(AgevGrade)
AgevGrade <- inner_join(summarize(group_by(filter(select(dataextract,Grade,Age.at.diagnosis), !is.na(Age.at.diagnosis) & !is.na(Grade)),Grade), Average_Age = mean(Age.at.diagnosis)),summarize(group_by(filter(select(dataextract,Grade,Age.at.diagnosis), !is.na(Age.at.diagnosis) & !is.na(Grade)),Grade), Median_Age = median(Age.at.diagnosis)))
View(AgevGrade)
