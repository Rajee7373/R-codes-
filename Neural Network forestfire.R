
               #### Reading the CSV file ###

forest <- read.csv(file.choose())
View(forest)
head(forest)
   
   ## converting the string into numeric variables ###
forest$month <- as.numeric(as.factor(forest$month))
forest$day <- as.numeric(as.factor(forest$day))
forest$size_category<-as.numeric(as.factor(forest$size_category))

             ### Visualisations ###

par(mfrow=c(2,6),mar=c(3.90, 4.25, 2.5, 0.5))
for (variables in 1:(dim(forest)[2]-1)){
  thisvar = forest[,variables]
  d <- density(thisvar)
  plot(d, main = names(forest[variables]),xlab="")
  polygon(d, col="cyan", border="blue")
  title("Density plots for all 11 Model Variables", line = -19, outer = TRUE)}

# Density plot - Predictors
# Shows us rain is right skewed and FFMC is left skewed. 
# We can see normal looking distributions for temp, wind, RH, X, DMC and day.



 ###We can also visualize the density of the outcome variable of Area burned##

par(mfrow=c(1,2),mar=c(5, 4.25, 5.5, 2))
d <- density(forest$area)
plot(d,main="Area Burned Density (original)",xlab="Area Burned (Hec)", col='tomato', lwd=3)
d <- density(log(forest$area+1))
plot(d,main="Area Burned Density (log(x+1))",xlab="Area Burned (Hec)", col='violet', lwd=3)

###Heavy right-skew in the data set ##
###and hence transformed using a log transformation ##
##where 1 will first be added to the area (to account for the 0 values).##



  ### Splitting the data into training and testing dataset ###
set.seed(123)
tra <- sample(2,nrow(forest),replace = T,prob = c(0.7,0.3))
forest_train <- forest[tra==1,]
forest_test <- forest[tra==2,]

    #### Building the model ####
model.type <- neuralnet(size_category~.,data=forest_train)
predict <- compute(model.type,forest_test[,-31])
result <- predict$net.result
cor(result,forest_test$size_category)


     #### Improving model performance ----
   # A more complex neural network topology with 10 hidden neurons##
set.seed(111)
model1.type <- neuralnet(size_category~.,data=forest_train,hidden = 10)
predict <- compute(model1.type,forest_test[,-31])
result <- predict$net.result
cor(result,forest_test$size_category)## 72.95%## 

     ### Model 3 with seven hidden layers ###
set.seed(333)
model3.type <- neuralnet(size_category~.,data=forest_train,hidden = 7)
plot(model3.type)
predict <- compute(model3.type,forest_test[,-31])
result <- predict$net.result
cor(result,forest_test$size_category) ## 84.79%%##
summary(model3.type)


      ## ##Model 4 with 4 hidden layers####
set.seed(444)
model4.type <- neuralnet(size_category~.,data=forest_train,hidden = 4)
plot(model4.type)
predict <- compute(model4.type,forest_test[,-31])
result <- predict$net.result
cor(result,forest_test$size_category) ##44.77%##

           
      ###### The model with seven hidden layers has 84.79% accuracy ####
                           ####$$$$$$#####