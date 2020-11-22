library(randomForest)
library(MASS)
library(caret)
library(ggplot2)

      #### Reading the CSV file####
CompanyData <- read.csv(file.choose())
View(CompanyData)
library(ggplot2)

hist(CompanyData$Sales, main = "Sales of Companydata",xlim = c(0,20),
     breaks=c(seq(10,20,30)), col = c("blue","red", "green","violet"))


highsales = ifelse(CompanyData$Sales<9, "No","Yes")  # if greater than 8 then high sales else Low
CD = data.frame(CompanyData[2:11],highsales)
str(CD)
table(CD$highsales)
CD$highsales <- as.factor(CD$highsales)

  ####Splitting the data into training data and testing data set ###

ind <- sample(2, nrow(CD), replace = TRUE, prob = c(0.6,0.4))
train <- CD[ind==1,]
test  <- CD[ind==2,]

set.seed(213)
rf <- randomForest(highsales~., data=train)
rf 

### Out of bag estimate of error rate is 20.9 % in Random Forest Model.##

attributes(rf)

   #### Prediction and Confusion Matrix - Training data### 
pred1 <- predict(rf, train)
head(pred1)
head(train$highsales)

 confusionMatrix(pred1, train$highsales)   # 100 % accuracy on training data 

   ### Prediction with test data - Test Data 
set.seed(222)
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$highsales)
##78% accuracy on test data          
 
   ### Error Rate in Random Forest Model##
library(ggplot2)
plot(rf)

  ### Tuning Random Forest Model ##
tune <- tuneRF(train[,-11], train[,11], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)


rf1 <- randomForest(highsales~., data=train, ntree = 300, mtry = 3, importance = TRUE,
                    proximity = TRUE)
rf1
## Out of bag error rate is 17.3.08%###

pred1 <- predict(rf1, train)
confusionMatrix(pred1, train$highsales)  # 100 % accuracy on training data 


### Test data prediction using the Tuned rf1 model###
pred2 <- predict(rf1, test)
confusionMatrix(pred2, test$highsales)
# 76.69 % accuracy on test data 
      
# no of nodes of trees

hist(treesize(rf1), main = "No of Nodes for the trees", col = "green")


# Majority of the trees has an average number of 45 to 50 nodes. 

# Variable Importance :

varImpPlot(rf1)


# The graph shows that how the model performs without each variable.
# MeanDecrease gini graph shows how much by average the gini decreases if one of those nodes were 
# removed. Price is very important and Urban is not that important.

varImpPlot(rf1 ,sort = T, n.var = 5, main = "Top 5 -Variable Importance")

# Quantitative values 
importance(rf1)

varUsed(rf)   # which predictor variables are actually used in the random forest.

# Partial Dependence Plot 
partialPlot(rf1, train, Price, "Yes")


###Conclusion :if the price is 100 or greater, then Customers are not buying those computers.

  # Extracting single tree from the forest ##
getTree(rf, 1, labelVar = TRUE)

 #Multi Dimension scaling plot of proximity Matrix###
MDSplot(rf1, CD$highsales)

    #############$$$$$$$$$$$$$$#############