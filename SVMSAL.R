      ###Packages required###

library(kernlab)
library(caret)
library(plyr)
library(ggplot2)
library(psych)
library(e1071)
   
    #####Reading the train CSV file####
 
salary_train <- read.csv(file.choose())
View(salary_train)
str(salary_train)
  
   ##converting the variable for analysis ## 
salary_train$Salary<-(as.factor(salary_train$Salary))

    #### Reading the test CSV file###

salary_test <- read.csv(file.choose())
str(salary_train)

  ## Converting the variable for analysis ##
salary_test$Salary<-(as.factor(salary_test$Salary))


    ###Visualization ###
   # Plot and ggplot #
ggplot(data=salary_train,aes(x=salary_train$Salary, y = salary_train$age, fill = salary_train$Salary)) +geom_boxplot() +ggtitle("Box Plot")

ggplot(data=salary_train,aes(x=salary_train$Salary, y = salary_train$capitalgain, fill = salary_train$Salary)) + geom_boxplot() + ggtitle("Box Plot")

ggplot(data=salary_train,aes(x=salary_train$Salary, y = salary_train$capitalloss, fill = salary_train$Salary)) +geom_boxplot() +ggtitle("Box Plot")

ggplot(data=salary_train,aes(x=salary_train$Salary, y = salary_train$hoursperweek, fill =salary_train$Salary)) +geom_boxplot() +ggtitle("Box Plot")


ggplot(data=salary_train,aes(x = salary_train$age, fill = salary_train$Salary))+geom_density(alpha = 0.9, color = 'Violet') 
ggtitle("Age - Density Plot")

ggplot(data=salary_train,aes(x = salary_train$workclass, fill =salary_train$Salary)) +geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Workclass Density Plot")

ggplot(data=salary_train,aes(x = salary_train$education,fill = salary_train$Salary)) +geom_density(alpha = 0.9, color = 'Violet')
ggtitle("education Density Plot")

ggplot(data=salary_train,aes(x = salary_train$educationno,fill= salary_train$Salary)) +  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("educationno Density Plot")

ggplot(data=salary_train,aes(x = salary_train$maritalstatus, fill = salary_train$Salary)) +  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("maritalstatus Density Plot")

ggplot(data=salary_train,aes(x = salary_train$occupation, fill =salary_train$Salary)) +geom_density(alpha = 0.9, color = 'Violet')
ggtitle("occupation Density Plot")

ggplot(data=salary_train,aes(x = salary_train$sex, fill =salary_train$Salary)) +geom_density(alpha = 0.9, color = 'Violet')
ggtitle("sex Density Plot")

ggplot(data=salary_train,aes(x = salary_train$relationship, fill = salary_train$Salary)) +geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Relationship Density Plot")

ggplot(data=salary_train,aes(x = salary_train$race, fill = salary_train$Salary)) + geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Race Density Plot")

ggplot(data=salary_train,aes(x = salary_train$capitalgain, fill =salary_train$Salary)) +geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Capitalgain Density Plot")

ggplot(data=salary_train,aes(x =salary_train$capitalloss, fill =salary_train$Salary)) +  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Capitalloss Density Plot")

ggplot(data=salary_train,aes(x =salary_train$hoursperweek, fill =salary_train$Salary)) +geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Hoursperweek Density Plot")

ggplot(data=salary_train,aes(x =salary_train$native, fill =salary_train$Salary)) +  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("native Density Plot")

  
   ###Building the model ###

model1<-ksvm(salary_train$Salary~., 
             data=salary_train, kernel = "vanilladot",C=1)

model1

Salary_prediction <- predict(model1,salary_test)

table(Salary_prediction,salary_test$Salary)

agreement <- Salary_prediction == salary_test$Salary

table(agreement)

prop.table(table(agreement))


     ## Different types of kernels ##
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix".


    ### kernel = rfdot ###
model_rfdot<-ksvm(salary_train$Salary~., 
                  data=salary_train,kernel = "rbfdot")

pred_rfdot<-predict(model_rfdot,newdata=salary_test)
mean(pred_rfdot==salary_test$Salary) ###85.41%###


    ### kernel = vanilladot###
model_vanilla<-ksvm(salary_train$Salary~., 
                    data= salary_train,kernel = "vanilladot")

pred_vanilla<-predict(model_vanilla,newdata=salary_test)
mean(pred_vanilla==salary_test$Salary) ### 84.62%##

   ###############$$$$$$$$$$$$$$##############

















