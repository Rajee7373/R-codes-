                    ###Packages required ###
              
library(party)
library(caret)
library(C50)
library(tree)
library(gmodels)
library(ggplot2)

FraudCheck <- read.csv("C:/Users/Admin34/Desktop/dec tree/Fraud_check (1).csv")
View(FraudCheck)
hist(FraudCheck$Taxable.Income)


    #####Visualisation###
library(dplyr)
FraudCheck %>% ggplot(aes(Taxable.Income))+geom_histogram(color="black",fill="red")
FraudCheck %>% ggplot(aes(City.Population))+geom_histogram(color="black",fill="blue")
FraudCheck %>% ggplot(aes(Work.Experience))+geom_histogram(color="black",fill="green")


   ### Converting the variables for analysis ###
FraudCheck$Undergrad<-as.numeric(as.factor(FraudCheck$Undergrad))
FraudCheck$Marital.Status<-as.numeric(as.factor(FraudCheck$Marital.Status))
FraudCheck$Urban<-as.numeric(as.factor(FraudCheck$Urban))

  ##converting taxable income to factor of good and risky##
Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")

# if Taxable Income is less than or equal to 30000 then Risky else Good##
FCtemp= data.frame(FraudCheck,Risky_Good)
FC = FCtemp[,c(1:7)]
View(FC)
str(FC)

table(FC$Risky_Good)  
# 476 good customers and 124 risky customers##

FC$Risky_Good<-as.factor(FC$Risky_Good)

    ### Splitting the data into train and test Data ###

FD <- sample(2, nrow(FC), replace = TRUE, prob = c(0.7,0.3))
train <- FC[FD==1,]
test  <- FC[FD==2,]


        ### Buidling the model ###

  ###Using Party Function ####

library(party)

opall_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban,data = FC)
summary(opall_tree)
plot(opall_tree)
# From the above tree , we can conclude
#data has 20 % of Risky and 80 % good .


    #### using the training Data ###

View(train)

op_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data =train)
summary(op_tree)
plot(op_tree)
 
       ####Prediction####
set.seed(123)
pred_tree <- as.data.frame(predict(op_tree,newdata=test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=test)
mean(pred_test_df==test$Risky_Good) # Accuracy = 82.31 %
CrossTable(test$Risky_Good,pred_test_df)


  ############$$$$$$$$$$$$############

