
#we import the libraries that we need
library(psych)
library(sandwich)
library(party)
library(gtools)
library(Hmisc)
library(RColorBrewer)
library(rpart)
library(tree)
library(cluster)
library(fpc)
library(RWeka)
library(partykit)
library(FSelector)
library(psych)
library(ggplot2)
library(corrplot)
library(rJava)
library(xlsxjars)
library(xlsx)
library(corrgram)
library(plotly)
library(plyr)
library(randomForest)
library(mboost)
library(ggcorrplot)
library(polycor)
library(ggpubr)
library(rpart.plot)

library(rattle)
library(rpart.plot)
library(caret)
library(RColorBrewer)
library(randomForest)
library(e1071)


datax <- read.csv("Hackathon_Data_2018.csv")

####DATA EXPLORATION####
dim(datax)
names(datax)
str(datax)
head(datax)
summary(datax)
describe(datax)

#--------------------------------#
# After looking at the data I found out that "Standard Hours", "Over 18" And "Employee No." columns are not required for the analysis.
#---------------------------------#

#we remove the variables that are not providing us any information
datax <- subset(datax, select = - c(Over18, StandardHours, EmployeeCount,EmployeeNumber))

for_factoring <- c("BusinessTravel","Gender","JobRole","Maritialstatus","OverTime","Department","EducationField")
datax[for_factoring] <- lapply(datax[for_factoring],as.numeric)

# For the people STILL AN EMPLOYEE ; NO means YES $$ YES means NO With Column Mistake
datax$CurrentEmployee
table(datax$CurrentEmployee)

#Changing No to Yes and YES to NO : Rectifying the error in the dataset
datax$CurrentEmployee <- ifelse(datax$CurrentEmployee=="Yes","No","Yes")
table(datax$CurrentEmployee)
datax$CurrentEmployee

# For people who left ( Not an Employee)
datax_No <- datax[datax$CurrentEmployee == "No",]
describe(datax_No)

# For people who are still an employee ( Is An Employee)
datax_Yes <- datax[datax$CurrentEmployee == "Yes",]
datax_Yes
summary(datax_Yes)
describe(datax_Yes)

#------------------- Analysing the Data--------------------#

# total number of people left from each department
dept_No <- (datax_No$Department)
left <- table(dept_No)
total <- table(datax$Department)
total
percent_left_perdept <- (left/total)*100
barplot(left,las=2)
barplot(percent_left_perdept,las=2)
table(dept_No)
# Turns out Human Resouces and Sales has the highest percent of people leaving.

#Is there a correlation between the distance they live from place of work?
summary(datax_No$DistanceFromHome)
summary(datax_Yes$DistanceFromHome)
#No much difference - Mean is almost same , this maynot be the reason.

#How did the employees rate their enviromnental satisfaction?
table(datax_No$EnvironmentSatisfaction)
table(datax_Yes$EnvironmentSatisfaction)
summary(datax_No$EnvironmentSatisfaction)
summary(datax_Yes$EnvironmentSatisfaction)
#No much difference here as well, environment is not an issue: mean is around 2.5 for both


#How did the employees rate their Job Satisfaction
table(datax_No$JobSatisfaction)
table(datax_Yes$JobSatisfaction)
summary(datax_No$JobSatisfaction)
summary(datax_Yes$JobSatisfaction)
#No much difference here as well, mean is around 2.5 here as well.


#Change the Non number variables into numberd:- Marital [ 1 = Single, 2 = Married]
#datax$MaritalStatus <- ifelse(datax$MaritalStatus=="Single",1,2)


#cor(current_binary,datax$YearsSinceLastPromotion)
for_correlation <- sapply(datax,is.numeric)
imp_variable <- cor(datax[,for_correlation][1:35,])
ggcorrplot(imp_variable)

#------------------------

datax
table(datax$CurrentEmployee)

#Features of people who have left
summary(datax_No$YearsSinceLastPromotion)

#Features of people who are still an employee
summary(datax_Yes$YearsSinceLastPromotion)

summary(datax_No$JobRole)
summary(datax_Yes$JobRole)

describe(datax_No$YearsAtCompany)
describe(datax_Yes$YearsAtCompany)


#Visualising Age 
hist(datax_No$Age,xlab="Age",ylab="Number of People",col = "green",main="Age oe people who left")
hist(datax_Yes$Age,xlab="Age",ylab="Number of People",col = "green",main="Age oe people who are working")

#Visualising Employee Education Levels and their field
ggplot(datax,aes(Education,fill=CurrentEmployee))+geom_bar()+labs(colour = "Cylinders")
#Percentage
(table(datax_No$Education)/table(datax$Education))*100



#Field
ggplot(datax,aes(EducationField,fill=CurrentEmployee))+geom_bar()+labs(colour = "Cylinders")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
(table(datax_No$EducationField)/table(datax$EducationField))*100
#Percentage
(table(datax_No$EducationField)/table(datax$EducationField))*100


#Employee Job Level
histogram(datax_No$JobLevel,xlab="Job Level",main="Job level of people who left", col = "pink")
table(datax_No[datax_No$JobLevel==1,]$EducationField)

#Education Field Percentage of these people
(table(datax_No[datax_No$JobLevel==1,]$EducationField)/table(datax[datax$JobLevel==1,]$EducationField))*100

#Distance From Work
histogram(datax_No$DistanceFromHome,xlab="Distance",main="Distance from work",col="grey")

#Department
ggplot(datax,aes(Department,fill=CurrentEmployee))+geom_bar()+labs(colour = "Cylinders")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Percentage
(table(datax_No$Department)/table(datax$Department))*100

#Job Involvement
ggplot(datax,aes(JobInvolvement,fill=CurrentEmployee))+geom_bar()+labs(colour = "Cylinders")
#Percentage
(table(datax_No$JobInvolvement)/table(datax$JobInvolvement))*100

#----------------------------------------------#
#Salary
ggplot(datax,aes(MonthlyIncome,fill=CurrentEmployee))+geom_density(alpha=0.7)
#Age
ggplot(datax,aes(Age,fill=CurrentEmployee))+geom_density(alpha = 0.6)
ggplot(datax,aes(x=Department,y=MonthlyIncome,fill=CurrentEmployee))+geom_boxplot()+theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(datax,aes(x=EducationField,y=MonthlyIncome,fill=CurrentEmployee))+geom_boxplot()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
datax$EducationField

#----------------------------------------------#

#Marriage
ggplot(datax,aes(MaritalStatus,fill=CurrentEmployee))+geom_bar(position="dodge")
#Percentage
(table(datax_No$MaritalStatus)/table(datax$MaritalStatus))*100

#Gender
ggplot(datax,aes(Gender,fill=CurrentEmployee))+geom_bar(position="dodge")
#Percentage
(table(datax_No$Gender)/table(datax$Gender))*100

#----------------------------------------------#

#Number of companies worked previously 
ggplot(datax,aes(NumCompaniesWorked,fill=CurrentEmployee))+geom_bar(position = "dodge")+geom_text(stat='count', aes(label=..count..), vjust=0)
#percentage
sort(table(datax_No$NumCompaniesWorked)/table(datax$NumCompaniesWorked))*100

#OverTime Stats
ggplot(datax,aes(OverTime,fill=CurrentEmployee))+geom_bar(position="dodge")+geom_text(stat='count', aes(label=..count..), vjust=0)


#percentage
(table(datax_No$OverTime)/table(datax$OverTime))*100

#---------------------------------------------------#
#JobRole visualisation
ggplot(datax,aes(JobRole,fill=CurrentEmployee))+geom_bar(position="dodge")+theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Percentage
(table(datax_No$JobRole)/table(datax$JobRole))*100
#---------------------------------------------------#
#Deepdiving into sales representatives data
datax_salesrep <- datax[datax$JobRole =="Sales Representative",]
datax_salesrep_no <- datax_No[datax_No$JobRole =="Sales Representative",]
datax_salesrep_yes <- datax_Yes[datax_Yes$JobRole =="Sales Representative",]


ggplot(datax_salesrep,aes(OverTime,fill=CurrentEmployee))+geom_bar(position="dodge")+geom_text(stat='count', aes(label=..count..), vjust=0)

ggplot(datax_salesrep,aes(MonthlyIncome,fill=CurrentEmployee))+geom_density(alpha=0.7)

#Proportion of Females and males in sales rep
ggplot(datax_salesrep,aes(Gender,fill=CurrentEmployee))+geom_bar(position="dodge")+geom_text(stat='count', aes(label=..count..), vjust=0)

table(datax_salesrep_no$Gender)/table(datax_salesrep$Gender)*100

table(datax_salesrep_no$Gender)

datax_salesrep_no_females <- datax_salesrep_no[datax_salesrep_no$Gender=="Female",]
table(datax_salesrep_no_females$MaritalStatus) #13 single females. 
table(datax_salesrep_no_females$BusinessTravel) #Maximum are made to travel frequently 

table(datax_salesrep_no_females$JobLevel)# They all belong to job level 1
table(datax_salesrep_no_females$JobLevel)

#----------------------------------------------#
#----------------------------------------------#

# Decision Trees 

# Partition the data in to a training set (=70%) and a test set (=30%)
split_data = function(datax, train, test) {
  dataPartition = sample(2, nrow(datax), replace = TRUE, prob = c(train, test))
  trainData <<- datax[dataPartition==1, ]
  testData <<- datax[dataPartition==2, ]
}
split_data(datax, 0.7,0.3)

dim(trainData)
dim(testData)



# Plot Unpruned Tree
fit1 <- rpart(CurrentEmployee ~., method = "class",  data = trainData)
rpart.plot(fit1,main="Unpruned Decision Tree - Train Dataset",cex=0.4)
table(testData$CurrentEmployee)
# Make predictions on Test data using UNPRUNED tree
prediction = predict(fit1, testData, type = 'class')
summary(prediction)
# Develop a confusion matrix using the caret library
unpruned_accuracy = confusionMatrix(as.factor(testData$CurrentEmployee), prediction)
unpruned_accuracy #Accuracy = 84.5

#CP Table
printcp(fit1)

#PRUNING THE TREE 
fit_prune <- prune(fit1,cp=0.018519)
rpart.plot(fit_prune,main="Pruned decision tree")
prediction_prune = predict(fit_prune, testData, type = 'class')
accuracy_prune = confusionMatrix(as.factor(testData$CurrentEmployee), prediction_prune)
accuracy_prune
# New accuracy = 85.02


#----------------------------------------------#
#----------------------------------------------#
#Checking if distribution of data can impact the output

sample_no <- datax_No
sample_yes <- datax_Yes[1:237,]
combined_data <- rbind(sample_no,sample_yes)
dim(combined_data)
combined_data
#Shuffle the data
combined_data <- combined_data[sample(1:nrow(combined_data)), ]
combined_data #Shuffled
dim(combined_data)
train_comb <- combined_data[1:331,]
test_comb <- combined_data[331:474,]
dim(train_comb)
dim(test_comb)
#Using this data to train and test 
fit_combined <- rpart(CurrentEmployee ~., method = "class",  data =train_comb)
rpart.plot(fit_combined,main="Unpruned Decision Tree - Full Dataset")
table(test_comb$CurrentEmployee)
prediction = predict(fit_combined, test_comb, type = 'class')
summary(prediction)
unpruned_accuracy = confusionMatrix(as.factor(test_comb$CurrentEmployee), prediction)
unpruned_accuracy #Accuracy = 70.1
#Prunning this combined data
printcp(fit_combined)
prune_combined <- prune(fit_combined,cp=0.049080)
rpart.plot(prune_combined,main="Pruned decision tree")
prediction_prune = predict(prune_combined, test_comb, type = 'class')
accuracy_prune = confusionMatrix(as.factor(test_comb$CurrentEmployee), prediction_prune)
accuracy_prune
printcp(prune_combined)#Accuracy = 67 not a good model. 



#Trying using Ctree
datax$EducationField
ct <- ctree(as.factor(CurrentEmployee) ~OverTime+Gender+MaritalStatus+JobInvolvement+JobLevel+Department+NumCompaniesWorked+MonthlyIncome+Age+EducationField, data=trainData)

plot(ct)
prediction_ctree = predict(ct, testData)
summary(prediction_ctree)
accuracy_ct = confusionMatrix(as.factor(testData$CurrentEmployee), prediction_ctree)
accuracy_ct

#Ctree Pruning
ct2 <- ctree(as.factor(CurrentEmployee) ~OverTime+JobLevel+MonthlyIncome+EducationField, data=trainData)

plot(ct2)
prediction_ctree2 = predict(ct2, testData)
summary(prediction_ctree2)
accuracy_ct2 = confusionMatrix(as.factor(testData$CurrentEmployee), prediction_ctree2)
accuracy_ct2


#Department+OverTime+JobRole+JobLevel+Monthly
ct3 <- ctree(as.factor(CurrentEmployee) ~Department+OverTime+JobRole+JobLevel+MonthlyIncome, data=trainData)

plot(ct3)
prediction_ctree3 = predict(ct3, testData)
summary(prediction_ctree3)
accuracy_ct3 = confusionMatrix(as.factor(testData$CurrentEmployee), prediction_ctree3)
accuracy_ct3


#Randome forest comparision to ctree
fit_forest <- randomForest(as.factor(CurrentEmployee)~.,data=trainData,importance=TRUE,ntree=2000)
varImpPlot(fit_forest)
Prediction_forest <- predict(fit_forest, testData)
accuracy_forest = confusionMatrix(as.factor(testData$CurrentEmployee), Prediction_forest)
accuracy_forest

