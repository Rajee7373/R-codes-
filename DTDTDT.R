library("tree")
library(C50)
library(party)
library(ggplot2)
library(dplyr)

     ## Reading the Data ##

data("iris")
View(iris)

    ##Visualization using ggplot## 

iris %>% ggplot(aes(Sepal.Length))+geom_histogram(color="black",fill="red")
iris %>% ggplot(aes(Sepal.Width))+geom_histogram(color="black",fill="blue")
iris %>% ggplot(aes(Petal.Length))+geom_histogram(color="black",fill="green")
iris %>% ggplot(aes(Petal.Width))+geom_histogram(color="black",fill="yellow")

# Splitting data into training and testing. As the species are in order 
# Splitting the data based on species 

iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])

     ### Building model on training data ##

irisc5.0_train <- C5.0(iris_train[,-5],iris_train$Species)
windows()
plot(irisc5.0_train) # Tree graph


    ### Training accuracy###
pred_train <- predict(irisc5.0_train,iris_train)
mean(iris_train$Species==pred_train) # 97.33% Accuracy

library(caret)
confusionMatrix(pred_train,iris_train$Species)
## Prediction##
predc5.0_test <- predict(irisc5.0_train,newdata=iris_test) # predicting on test data
mean(predc5.0_test==iris_test$Species) # 94.66% accuracy 
confusionMatrix(predc5.0_test,iris_test$Species)

library(gmodels)
 # Cross tables#
CrossTable(iris_test$Species,predc5.0_test)

    

    ######$$$$$$$$$$#####

## Using Party Function ##

op_tree<-ctree(iris$Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=iris)
summary(op_tree)
plot(op_tree)

# Predcition based on above model #

pred_tree <- as.data.frame(predict(op_tree,newdata=iris_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=iris_test)
mean(pred_test_df==iris_test$Species) # Accuracy = 96.75%
CrossTable(iris_test$Species,pred_test_df)


      #####$$$$$$$$$$$$$$$$$$$$$$$$$$########

   ## Splitting the data into 70 :30 , training and testing data respectively ##

iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50
iris_train <- rbind(iris_setosa[1:35,],iris_versicolor[1:35,],iris_virginica[1:35,])
iris_test <- rbind(iris_setosa[36:50,],iris_versicolor[36:50,],iris_virginica[36:50,])

op_tree<-ctree(iris$Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=iris)
summary(op_tree)
plot(op_tree)

set.seed(333)

pred_tree <- as.data.frame(predict(op_tree,newdata=iris_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=iris_test)
mean(pred_test_df==iris_test$Species) # Accuracy = 100%
CrossTable(iris_test$Species,pred_test_df)

      ############%%%%%%%%%%%%%%%$$$$$$$$$$$$#################


