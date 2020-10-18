library(neuralnet)  
library(nnet)
library(NeuralNetTools)

   # Reading the CSV file ###
concrete <- read.csv(file.choose())
str(concrete)
 
 ### Visualization #####
hist(concrete$cement, prob = T, breaks = 30)
lines(density(concrete$cement))

hist(concrete$slag, prob = T, breaks = 30)
lines(density(concrete$slag))

hist(concrete$ash, prob=T, breaks=30)
lines(density(concrete$ash))

hist(concrete$water,prob=T,breaks=30)
lines(density(concrete$water))

hist(concrete$superplastic,prob=T,breaks=30)
lines(density(concrete$superplastic))

hist(concrete$coarseagg,prob=T,breaks=30)
lines(density(concrete$coarseagg))

hist(concrete$fineagg,prob=T,breaks=30)
lines(density(concrete$fineagg))

hist(concrete$age,prob=T,breaks=30)
lines(density(concrete$age))

##### Histograms of these features confirm that the 
    # data has different scales and needs normalization#######

summary(concrete) 
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concrete,FUN=normalize))
summary(concrete_norm$strength) 
summary(concrete$strength) 

      #### Data Partitioning into training and test ###

set.seed(123)
con <- sample(2, nrow(concrete_norm), replace = TRUE, prob = c(0.7,0.3))
concrete_train <- concrete_norm[con==1,]
concrete_test  <- concrete_norm[con==2,]


  #### Creating a neural network model on training data###

concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train)
str(concrete_model)

plot(concrete_model, rep = "best")
summary(concrete_model)

par(mar = numeric(4), family = 'serif')
plotnet(concrete_model, alpha = 0.6)


   ##### Evaluating model performance####

set.seed(12323)
model_results <- compute(concrete_model,concrete_test[1:8])
predicted_strength <- model_results$net.result

   #### Predicted strength Vs Actual Strength of test data.###

cor(predicted_strength,concrete_test$strength)  ## 85.66%##
str_max <- max(concrete$strength)
str_min <- min(concrete$strength)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualStrength_pred <- unnormalize(predicted_strength,str_min,str_max)
head(ActualStrength_pred)

    #### Improving the model performance by changing the number of hidden layers###

set.seed(12345)
concrete_model2 <- neuralnet(strength~cement+slag+ash+water+superplastic+
                               coarseagg+fineagg+age,data= concrete_train,
                             hidden = 5)
plot(concrete_model2, rep = "best")
summary(concrete_model2)

model_results2<-compute(concrete_model2,concrete_test[1:8])
predicted_strength2<-model_results2$net.result
cor(predicted_strength2,concrete_test$strength)  ## 94.84%##
plot(predicted_strength,concrete_test$strength)


par(mar = numeric(4), family = 'serif')
plotnet(concrete_model2, alpha = 0.6)


    ##### Model3 with seven hidden layers ###

set.seed(12333)
concrete_model3 <- neuralnet(strength~cement+slag+ash+water+superplastic+
                               coarseagg+fineagg+age,data= concrete_train,
                             hidden = 7)
plot(concrete_model3, rep = "best")

summary(concrete_model3)

model_results3<-compute(concrete_model3,concrete_test[1:8])
predicted_strength3<-model_results3$net.result
cor(predicted_strength3,concrete_test$strength) ### 94.80%###
plot(predicted_strength,concrete_test$strength)

par(mar = numeric(4), family = 'serif')
plotnet(concrete_model3, alpha = 0.6)

       #### MOdel 4 with ten hidden layers ###

set.seed(12444)
concrete_model4 <- neuralnet(strength~cement+slag+ash+water+superplastic+
                               coarseagg+fineagg+age,data= concrete_train,
                             hidden = 10)
plot(concrete_model4, rep = "best")

summary(concrete_model4)

model_results4<-compute(concrete_model4,concrete_test[1:8])
predicted_strength4<-model_results4$net.result
cor(predicted_strength4,concrete_test$strength) ### 95.02%##
plot(predicted_strength,concrete_test$strength)


par(mar = numeric(4), family = 'serif')
plotnet(concrete_model4, alpha = 0.6)

#### It can be observed that with the increase in number of hidden layers, training steps increases but 
### the SSE(error) reduces ###
