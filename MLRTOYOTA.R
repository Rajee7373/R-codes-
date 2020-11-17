
      ##Reading the csv file##

Corolla <- read.csv("C:/Users/Admin34/Desktop/mlr/ToyotaCorolla.csv")
View(Corolla)

attach(Corolla)
Corolla_1 <- cbind(Price,Age_08_04,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight)
Corolla_1 <- as.data.frame(Corolla_1)
class(Corolla_1)

View(Corolla_1)
attach(Corolla_1)

summary(Corolla_1)

              ### EDA ###
  ##variance of some independent variable#

c(var(Corolla_1$Price),var(Corolla_1$Age_08_04),var(Corolla_1$KM),var(Corolla_1$KM),var(Corolla_1$HP),var(Corolla_1$cc),var(Corolla_1$Doors),var(Corolla_1$Gears))

   ##standard daviation of variables ##
c(sd(Corolla_1$Price),sd(Corolla_1$Age_08_04),sd(Corolla_1$KM),sd(Corolla_1$KM),sd(Corolla_1$HP),sd(Corolla_1$cc),sd(Corolla_1$Doors),sd(Corolla_1$Gears))

library(e1071) 
    ##skewness of variable##
c(skewness(Corolla_1$Price),skewness(Corolla_1$Age_08_04),skewness(Corolla_1$KM),skewness(Corolla_1$KM),skewness(Corolla_1$HP),skewness(Corolla_1$cc),skewness(Corolla_1$Doors),skewness(Corolla_1$Gears))

   ##ploting the variables##

library(ggplot2)
(p1 <-(ggplot(Corolla_1,aes(Price))+geom_histogram(color="black",fill="red")))
(p2 <- ggplot(Corolla_1,aes(Price))+geom_boxplot(color="black",fill="red")+coord_flip())
(h1 <- ggplot(Corolla_1,aes(Corolla_1$KM))+geom_histogram(color="black",fill="blue"))
(h2 <- ggplot(Corolla_1,aes(Corolla_1$KM))+geom_boxplot(fill="blue",color="black")+coord_flip())
(s1 <- ggplot(Corolla_1,aes(Corolla_1$HP))+geom_histogram(fill="green",color="black"))
(s2 <- ggplot(Corolla_1,aes(Corolla_1$HP))+geom_boxplot(fill="green",color="black")+coord_flip())
(a1 <- ggplot(Corolla_1,aes(Corolla_1$cc))+geom_histogram(fill="purple",color="black"))
(a2 <- ggplot(Corolla_1,aes(Corolla_1$cc))+geom_boxplot(fill="purple",color="black")+coord_flip())
(c1 <- ggplot(Corolla_1,aes(Corolla_1$Weight))+geom_histogram(fill="yellow",color="black"))
(c2 <- ggplot(Corolla_1,aes(Corolla_1$Weight))+geom_boxplot(fill="yellow",color="black")+coord_flip())
(d1 <- ggplot(Corolla_1,aes(Corolla_1$Quarterly_Tax))+geom_histogram(fill="pink",color="black"))
(d2 <- ggplot(Corolla_1,aes(Corolla_1$Quarterly_Tax))+geom_boxplot(fill="pink",color="black")+coord_flip())
library(gridExtra)
grid.arrange(p1,p2,h1,h2,s1,s2,a1,a2,c1,c2,d1,d2,nrow=6,ncol=2)

plot(Age_08_04, Price)
plot(KM, Price)
plot(HP, Price)
plot(cc, Price)
plot(Doors, Price)
plot(Gears, Price)
plot(Quarterly_Tax, Price)
plot(Weight, Price)


   #checking  for correlation##
windows()
pairs(Corolla_1)## finding the correlation 
cor(Corolla_1)###Correlation coefficient - Strength & Direction of correlation#


  # The Linear Model of interest##
corolla.price <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
summary(corolla.price)  ##Adjusted R-squared is 86.3%#

corolla.price2 <-lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight)
summary(corolla.price2) ##Adjusted R-squared:  86.3%# 


### Partial Correlation matrix - Pure correlation between the variables#

library(corpcor)
cor2pcor(cor(Corolla_1))
library(mvinfluence)
library(car)

# Deleting a single observation rather than entire variable to get rid of collinearity problem#
# Deletion Diagnostics for identifying influential variable

influence.measures(corolla.price)
influenceIndexPlot(corolla.price, id.n=3) # Index Plots of the influence measures
influencePlot(corolla.price, id.n=3) # A user friendly representation of the above


## Regression after deleting the 81st observation, which is influential observation

corolla.price1 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=Corolla_1[-81,])
summary(corolla.price1)## Adjusted R-squared:86.86 %#

vif(corolla.price1)  # VIF is > 10 => collinearity

plot(Age_08_04,KM, col="dodgerblue4",pch=20)

plot(HP,cc, col="dodgerblue4",pch=20)

layout(matrix(c(1,2,3,4),2,2))

plot(corolla.price1)

avPlots(corolla.price1, id.n=2, id.cex=0.7) # Added Variable Plots

# VIF & avPlots given us an indication to delete 'WT' variable#

Finalmodel <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
summary(Finalmodel) #Adjusted R-squared:86.3%# 


 ## Prediction based on the final model#

Price_Predict <- predict(corolla.price,interval="predict")
Pred_final <- predict(corolla.price)

Final <- cbind(Price,Pred_final,Age_08_04,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight)
View(Final)


    
   # Evaluating  model LINE assumptions##

plot(Finalmodel)
# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance

qqPlot(Finalmodel, id.n=5)
# QQ plots of studentized residuals, helps identify outliers#

library("MASS")
stepAIC(Finalmodel)


    ########$$$$$$$$$$$$$$##########

