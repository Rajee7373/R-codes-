 
####### Using Random Forest###
### Reading iris data####
library(randomForest)
data(iris)
View(iris)

# Splitting data into training and testing. As the species are in order (70:30)
# splitting the data based on species 
## Hypertuning parameter= Petal width##
## Since data is in order , subsets cerated for unbaised selection###

iris_setosa<-iris[iris$Species=="setosa",] 
iris_versicolor <- iris[iris$Species=="versicolor",] 
iris_virginica <- iris[iris$Species=="virginica",] 
iris_train <- rbind(iris_setosa[1:35,],iris_versicolor[1:35,],iris_virginica[1:35,])
iris_test <- rbind(iris_setosa[36:50,],iris_versicolor[36:50,],iris_virginica[36:50,])

# Building a random forest model on training data 
fit.forest <- randomForest(Species~.,data=iris_train, action=Petalwidth,importance=TRUE)
# Training accuracy 
mean(iris_train$Species==predict(fit.forest,iris_train)) # 100% accuracy 

# Prediction of train data
pred_train <- predict(fit.forest,iris_train)
library(caret)

# Confusion Matrix
confusionMatrix(iris_train$Species, pred_train)


# Predicting test data 
pred_test <- predict(fit.forest,newdata=iris_test)
mean(pred_test==iris_test$Species) # Accuracy 97.77%


# Confusion Matrix 

confusionMatrix(iris_test$Species, pred_test)

# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)


##############@@@@2222@@@@@@@@################


# Using Random Forest
library(randomForest)
data(iris)
View(iris)

# Splitting data into training and testing. As the species are in order(50:50) 
# splitting the data based on species 
# HYperparameter = SepalWidth
iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])

# Building a random forest model on training data 
fit.forest <- randomForest(Species~.,data=iris_train, action=SepalWidth,importance=TRUE)
# Training accuracy 
mean(iris_train$Species==predict(fit.forest,iris_train)) # 100% accuracy 

# Prediction of train data
pred_train <- predict(fit.forest,iris_train)
library(caret)


# Confusion Matrix
confusionMatrix(iris_train$Species, pred_train)


# Predicting test data 
pred_test <- predict(fit.forest,newdata=iris_test)
mean(pred_test==iris_test$Species) # Accuracy 94.6%%


# Confusion Matrix 

confusionMatrix(iris_test$Species, pred_test)

# Visualization 
plot(fit.forest,lwd=3)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:3)

###############@@@@@33333@@@@##########

library(randomForest)
data(iris)
View(iris)

# Splitting data into training and testing. As the species are in order (70:30)
# splitting the data based on species 
## Hypertuning parameter= Sepalwidth##
## Since data is in order , subsets cerated for unbaised selection###

iris_setosa<-iris[iris$Species=="setosa",] 
iris_versicolor <- iris[iris$Species=="versicolor",] 
iris_virginica <- iris[iris$Species=="virginica",] 
iris_train <- rbind(iris_setosa[1:35,],iris_versicolor[1:35,],iris_virginica[1:35,])
iris_test <- rbind(iris_setosa[36:50,],iris_versicolor[36:50,],iris_virginica[36:50,])

# Building a random forest model on training data 
fit.forest <- randomForest(Species~.,data=iris_train, action=SepalWidth,importance=TRUE)
# Training accuracy 
mean(iris_train$Species==predict(fit.forest,iris_train)) # 100% accuracy 

# Prediction of train data
pred_train <- predict(fit.forest,iris_train)
library(caret)

# Confusion Matrix
confusionMatrix(iris_train$Species, pred_train)


# Predicting test data 
pred_test <- predict(fit.forest,newdata=iris_test)
mean(pred_test==iris_test$Species) # Accuracy 100%


# Confusion Matrix 

confusionMatrix(iris_test$Species, pred_test)

# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)

## histogram###
iris %>% ggplot(aes(Petal.Length))+geom_histogram(color="black",fill="red")
iris %>% ggplot(aes(Sepal.Length))+geom_histogram(color="black",fill="blue")
iris %>% ggplot(aes(Petal.Width))+geom_histogram(color="black",fill="yellow")
iris %>% ggplot(aes(Sepal.Width))+geom_histogram(color="black",fill="purple")

##### If we split the data into 50:50 TrainTest we are getting an accuracy of 94.66% , 
but 70:30 TrainTest gives an accuracy of 100% in iris dataset ###






