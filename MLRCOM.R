
    ## Reading the CSV file##
CD <- read.csv(file.choose())
CD <- CD[,-1]
View(CD)
str(CD)
summary(CD)


  #Concerting the categorical variable##
CD$cd <- as.numeric(as.factor(CD$cd))
CD$multi <- as.numeric(as.factor(CD$multi))
CD$premium <- as.numeric(as.factor(CD$premium))

View(CD)
class(CD)

    ### EDA ###
attach(CD)
a <- c(CD$price,CD$speed,CD$hd,CD$ram,CD$screen,CD$cd,CD$multi,CD$multi,CD$premium,CD$ads,CD$trend)
var(CD$price)
var(hd)
var(speed)
var(ram)
var(screen)
var(trend)
sd(CD$price)
sd(hd)
sd(speed)
sd(ram)
sd(screen)
sd(trend)


    ## Visualisation###

library(ggplot2)
(p1 <-(ggplot(CD,aes(price))+geom_histogram(color="black",fill="red")))
(p2 <- ggplot(CD,aes(price))+geom_boxplot(color="black",fill="red")+coord_flip())
(h1 <- ggplot(CD,aes(speed))+geom_histogram(color="black",fill="blue"))
(h2 <- ggplot(CD,aes(speed))+geom_boxplot(fill="blue",color="black")+coord_flip())
(s1 <- ggplot(CD,aes(ram))+geom_histogram(fill="green",color="black"))
(s2 <- ggplot(CD,aes(ram))+geom_boxplot(fill="green",color="black")+coord_flip())
(a1 <- ggplot(CD,aes(hd))+geom_histogram(fill="purple",color="black"))
(a2 <- ggplot(CD,aes(hd))+geom_boxplot(fill="purple",color="black")+coord_flip())
(c1 <- ggplot(CD,aes(screen))+geom_histogram(fill="yellow",color="black"))
(c2 <- ggplot(CD,aes(screen))+geom_boxplot(fill="yellow",color="black")+coord_flip())
(d1 <- ggplot(CD,aes(ads))+geom_histogram(fill="pink",color="black"))
(d2 <- ggplot(CD,aes(ads))+geom_boxplot(fill="pink",color="black")+coord_flip())
library(gridExtra)
grid.arrange(p1,p2,h1,h2,s1,s2,a1,a2,c1,c2,d1,d2,nrow=6,ncol=2)

   #checking  for correlations#
pairs(CD)
cor(CD)

library(corpcor)
cor2pcor(cor(CD))

   #checking the random forest for the most important..#
   ##variable for the linear model##

library(tree)
imp <- tree(price~.,data=CD)
plot(imp)
text(imp)

library(randomForest)
imprf <- randomForest(price~.,data=CD)
varImpPlot(imprf)

     #creating a lm model##
#model selection by using the least AIC value##
model.null <- lm(price~1,data = CD,family = "binomial")
model.full <- lm(price~.,data = CD,family = "binomial")

   ## I - forward selection##
step(model.null,scope = list(upper=model.full),direction = "forward")

  ##II - backward elimination##
step(model.full,scope=list(lower=model.null,direction="backward"))

   ##III - bidirectional elimination##
model.price <- lm(price~ speed + hd + ram + screen,data = CD,family = "binomial")
step(model.price,scope = list(upper=model.full,lower=model.null),direction = "both")
#Step:  AIC=70336.65
#price ~ speed + hd + ram + screen + trend + premium + ads + multi + cd 
#These are the most significant variable with the least AIC

    ###Generating the Regression model###
model1 <- lm(price~ram+hd+trend+speed+screen+premium,data=CD)
summary(model1)###Adjusted R-squared is 76%#

model2 <- lm(price~ram+hd+trend+screen+speed+premium+ads+cd,data=CD)
summary(model2)###Adjusted R-squared is 77.23%# 

model3 <- lm(price~speed + hd + ram + screen + trend + premium + ads + multi + cd ,data=CD)
summary(model3) ###Adjusted R-squared is 77.52% #

   #model3 generate the most significant model
anova(model1,model2,model3)
predict(model3,interval="confidence")
confint(model3)
par(mfrow=c(2,2))
plot(model3)

library(mvinfluence)
influenceIndexPlot(model3)
model4 <- lm(price~speed + hd + ram + screen + trend + premium + ads + multi + cd ,data=CD[c(-1441-1701),])
summary(model4)#Adjusted R-squared is 77.52% 
predict(model4,interval="confidence")
confint(model4)
par(mfrow=c(2,2))
plot(model4)

qqPlot(model4, id.n=5)
# QQ plots of studentized residuals, helps identify outliers#

library("MASS")
stepAIC(model4)

   ###########$$$$$$$$$$############

