library(randomForest)
library(MASS)
library(caret)

      ##### Reading the CSV file #####

FraudCheck <- read.csv(file.choose())
hist(FraudCheck$Taxable.Income)


hist(FraudCheck$Taxable.Income, main = "Sales of Companydata",xlim = c(0,100000),
     breaks=c(seq(40,60,80)), col = c("blue","red", "green","yellow"))


Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")

# if Taxable Income is less than or equal to 30000 then Risky else Good##
FCtemp= data.frame(FraudCheck,Risky_Good)
FC = FCtemp[,c(1:7)]

str(FC)

table(FC$Risky_Good)  
# 476 good customers and 124 risky customers##

FC$Risky_Good<-as.factor(FC$Risky_Good)

  ### Splitting the data into train and test Data ###

FD <- sample(2, nrow(FC), replace = TRUE, prob = c(0.7,0.3))
train <- FC[FD==1,]
test  <- FC[FD==2,]

set.seed(213)
 
  ### Buidling the model ###
Model1 <- randomForest(Risky_Good~., data=train)
Model1 
attributes(Model1)

      ### Prediction and Confusion Matrix - Training data ###
pred1 <- predict(Model1, train)
head(pred1)

head(train$Risky_Good)

confusionMatrix(pred1, train$Risky_Good)   
# 100 % accuracy on training data 


      ### Prediction with test data ### 
pred2 <- predict(Model1, test)
confusionMatrix(pred2, test$Risky_Good) 
# 100 % accuracy on test data 

    
     ### Error Rate in Random Forest Model##
plot(Model1)
# at 200 there is a constant line and it does not vary after 200 trees

### MOdel2###   
Model2 <- randomForest(Risky_Good~., data=train, ntree = 200, mtry = 2, importance = TRUE,
                    proximity = TRUE)
Model2

pred1 <- predict(Model2, train)
confusionMatrix(pred1, train$Risky_Good)  
# 100 % accuracy on training data #

    ##Test data prediction using the Tuned Model 2 Data ###
pred2 <- predict(Model2, test)
confusionMatrix(pred2, test$Risky_Good) # 100 % accuracy on test data 

plot(Model2)
legend("topright",colnames(Model2$err.rate),col=1:3,cex=0.8,fill=1:6)


   #### number of nodes of trees###
hist(treesize(Model2), main = "No of Nodes for the trees", col = "green")

# Majority of the trees has an average number of more than 80 nodes#


  ##### Variable Importance ###
varImpPlot(Model2)

# The graph shows that how the model performs without each variable.
# Mean Decrease in gini graph shows how much by average the gini decreases if one of those nodes were 
# removed. Taxable.Income is very important and Urban is not that important.

varImpPlot(Model2 ,sort = T, n.var = 5, main = "Top 5-Variable Importance")


  ### Quantitative values ###
importance(Model2)

varUsed(Model2)   # which predictor variables are actually used in the random forest.

partialPlot(Model2, train, Taxable.Income, "Good")

#It can be seen that  the taxable Income is 30000 or greater,
# then they are less risky ##
    
    ### Extracting single tree from the forest ###

tr1 <- getTree(Model2, 2, labelVar = TRUE)

   ###Multi Dimension scaling plot of proximity Matrix###
MDSplot(Model2, FC$Risky_Good)

        ############$$$$$$$$$$$##############

