        ### Packages required###

library(caret)
library(C50)
library(tree)
library(gmodels)
library(party)

       ####Reading the CSV data ####

CompanyData <- read.csv("C:/Users/Admin34/Desktop/dec tree/Company_Data (1).csv")
hist(CompanyData$Sales)
summary(CompanyData$Sales)


   ## Visualization using ggplot##
#1.)Histogram
library(ggplot2)
library(dplyr)
CompanyData %>% ggplot(aes(Sales))+geom_histogram(color="black",fill="red")
CompanyData %>% ggplot(aes(Income))+geom_histogram(color="black",fill="blue")
CompanyData %>% ggplot(aes(Population))+geom_histogram(color="black",fill="green")
CompanyData %>% ggplot(aes(Age))+geom_histogram(color="black",fill="YELLOW")
CompanyData %>% ggplot(aes(Price))+geom_histogram(color="black",fill="GREY")

#2.)BOXPLOT
CompanyData%>% ggplot(aes(Sales))+geom_boxplot(color="black",fill="red")+coord_flip()
CompanyData %>% ggplot(aes(Income))+geom_boxplot(color="black",fill="blue")+coord_flip()
CompanyData %>% ggplot(aes(Population))+geom_boxplot(color="black",fill="green")+coord_flip()
CompanyData %>% ggplot(aes(Age))+geom_boxplot(color="black",fill="YELLOW")+coord_flip()
CompanyData %>% ggplot(aes(Price))+geom_boxplot(color="black",fill="GREY")+coord_flip()


  ## Converting the variable for analysis###
CompanyData$ShelveLoc <- (as.factor(CompanyData$ShelveLoc))
CompanyData$Urban <- (as.factor(CompanyData$Urban))
CompanyData$US <- (as.factor(CompanyData$US))

View(CompanyData)

##Converting the Sales variable in factors of high low and medium##
##for classification based on decision tree##

High = ifelse(CompanyData$Sales<10, "No", "Yes")
CDtemp=data.frame(CompanyData,High)
CD=CDtemp[,c(2:12)]
View(CD)

CD$High<-as.factor(CD$High)


   ###Splitting the Data the into training and testing data### 

CD_train <- CD[1:200,]
#View(CD_train)

CD_test <- CD[201:400,]
# View(CD_test)


    #Using Party Function ###
op_tree = ctree(High ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc
                + Age + Education + Urban + US, data = CD_train)

summary(op_tree)

plot(op_tree)


# From Above tree,  if the Location of the Shelv is good,
# then there is a probability of 60% chance that the customer will buy.
# With ShelveLoc having a Bad or Medium and Price <= 87, the probability 
##of High sales  could be 60%.
# If ShelveLoc is Bad or Medium, With Price >= 87 & 
#Advertising less than <= 7 then there is a zero percent chance of high sales.
# If ShelveLoc is Bad or Medium, With Price >= 87 
#& Advertising less than > 7 then there is a 20%  chance of high sales.
 

     ## Prediction##
pred_tree <- as.data.frame(predict(op_tree,newdata=CD_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=CD_test)
mean(pred_test_df==CD$High) # Accuracy = 68.75%
CrossTable(CD_test$High,pred_test_df)


   #### Using tree function ###
cd_tree_org <- tree(CompanyData$Sales ~ . ,data=CD)
summary(cd_tree_org)
plot(cd_tree_org)
text(cd_tree_org,pretty = 0)


  ## Using the training data with  tree function ###
cd_tree <- tree(High ~ .,data=CD_train)
summary(cd_tree)
plot(cd_tree)
text(cd_tree,pretty = 0)


   ### Evaluating the Model###

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(cd_tree,newdata=CD_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(cd_tree,newdata=CD_test)

pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]
pred_tree$final <- as.factor(pred_tree$final)
summary(pred_tree$final)
summary(CD_test$High)
mean(pred_tree$final==CD$High) # Accuracy = 77.25%
CrossTable(CD_test$High,pred_tree$final)

confusionMatrix(CD_test$High,pred_tree$final) 
          
   ###############$$$$$$$$$$$$$$$$$$$$################