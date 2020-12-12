
Startups <- read.csv("C:/Users/Admin34/Desktop/mlr/50_Startups.csv")
View(Startups)
class(Startups)

library(plyr)

Startups$State <- as.numeric(as.factor(Startups$State))

Startups <- as.data.frame(Startups)

  ##### EDA###

var(Startups$R.D.Spend)
sd(Startups$R.D.Spend)
var(Startups$Administration)
var(Startups$Marketing.Spend)
sd(Startups$Marketing.Spend)

library(e1071)
skewness(Startups$R.D.Spend)
skewness(Startups$Administration)
skewness(Startups$Marketing.Spend)
skewness(Startups$Profit)
skewness(Startups$State)

  ### Visualisation ##
library(ggplot2)
r1 <- ggplot(Startups,aes(Startups$R.D.Spend))+geom_histogram(color="black",fill="red")
r2 <- ggplot(Startups,aes(Startups$R.D.Spend))+geom_boxplot(color="black",fill="red")+coord_flip()
a1 <- ggplot(Startups,aes(Startups$Administration))+geom_histogram(fill="blue",color="black")
a2 <- ggplot(Startups,aes(Startups$Administration))+geom_boxplot(fill="blue",color="black")+coord_flip()
p1 <- ggplot(Startups,aes(Startups$Profit))+geom_histogram(fill="green",color="black")
p2 <- ggplot(Startups,aes(Startups$Profit))+geom_boxplot(fill="green",color="black")+coord_flip()
s1 <- ggplot(Startups,aes(Startups$State))+geom_histogram(fill="purple",color="black")
s2 <- ggplot(Startups,aes(Startups$State))+geom_boxplot(fill="purple",color="black")+coord_flip()

library(gridExtra)
grid.arrange(r1,r2,a1,a2,p1,p2,s1,s2,nrow=4,ncol=2)

#checking for correlation- Scatter Diagram
pairs(Startups)

cor(Startups$R.D.Spend,Startups$Marketing.Spend)
cor(Startups$R.D.Spend,Startups$Profit)
#It shows that Startups$R.D.Spend is a good variable for regression analysis.
cor(Startups$Marketing.Spend,Startups$Profit)
cor(Startups)

# Correlation coefficient - Strength & Direction of correlation#
## Partial Correlation matrix - Pure correlation between the variables#
library(corpcor)
cor2pcor(cor(Startups))
#checking the variable importance in generating the regression model

library(tree)
mod <- tree(Startups$Profit~.,data=Startups)
plot(mod)
text(mod)

library(randomForest)
modrf <- randomForest(Startups$Profit~.,data=Startups)
varImpPlot(modrf)


  # The Linear Model of interest#
Model.Startups <- lm(Profit~R.D.Spend+Administration+Marketing.Spend+State)
summary(Model.Startups)
#Adjusted R-squared:94.64% 

Model.Startups1 <- lm(Profit~R.D.Spend+log(Administration))
summary(Model.Startups1)
## Adjusted R-squared:  0.9451


library(mvinfluence)
library(car)

# Deleting  a single observation rather than entire variable to get rid of collinearity problem.
# Deletion Diagnostics for identifying influential variable

influence.measures(Model.Startups)
influenceIndexPlot(Model.Startups,id.n=3) # Index Plots of the influence measures
influencePlot(Model.Startups,id.n=3) # A user friendly representation of the above

## Regression after deleting the 49th and 50th observation, which is influential observation

Startups$State<-as.numeric(as.factor(Startups$State))

   # Logarithimic Transformation ##
Model.Startups_Log<-lm(Profit~R.D.Spend+log(Administration)+Marketing.Spend+log(State),data=Startups[-c(49,50),])
summary(Model.Startups_Log)  # Adjusted R2 Value is 95.9%#

confint(Model.Startups_Log,level=0.95)

predict(Model.Startups_Log,interval="predict")


Model.Startups_Fin1<-lm(Profit~R.D.Spend+Administration+Marketing.Spend+State,data=Startups[-c(49,50),])
summary(Model.Startups_Fin1)  # Adjusted R2 Value is 95.67%#

     
    # Exponential Transformation :
Model.Startups_exp<-lm(log(Profit)~R.D.Spend+Administration+Marketing.Spend+State,data=Startups[-c(49,50),])
summary(Model.Startups_exp)  #Adjusted R2 Value is 91.79%#

Model.Startups_exp1<-lm(log(Profit)~R.D.Spend+Marketing.Spend,data=Startups[-c(49,50),])
summary(Model.Startups_exp1) ## Adjusted R2 value is 91.88%#


   #Quad model#
Model.Startups_Quad <- lm(Profit~R.D.Spend+I(R.D.Spend^2)+Administration+I(Administration^2)
                          +Marketing.Spend+I(Marketing.Spend^2)+State+I(State^2),data=Startups[-c(49,50),])
summary(Model.Startups_Quad)  #Adjusted R2 value is 95.67%#

confint(Model.Startups_Quad,level=0.95)

predict(Model.Startups_Quad,interval="predict")

Model.Startups_Quad1 <- lm(Profit~R.D.Spend+I(R.D.Spend^2)+Marketing.Spend+I(Marketing.Spend^2)
                           ,data=Startups[-c(49,50),])

summary(Model.Startups_Quad1)  #Adjusted R2 value is 95.67%#
 

   ###Poly Model##
Model.Startups_Poly <- lm(Profit~R.D.Spend+I(R.D.Spend^2)+I(R.D.Spend^3)+
                            Administration+I(Administration^2)+I(Administration^3)+
                            Marketing.Spend+I(Marketing.Spend^2)+I(Marketing.Spend^3)+
                            State+I(State^2)+I(State^3),data=Startups[-c(49,50),])
summary(Model.Startups_Poly) #Adjusted R2 Value is 95.69%#


Model.Startups_Poly1 <- lm(Profit~R.D.Spend+I(R.D.Spend^2)+I(R.D.Spend^3)+
                             Marketing.Spend+I(Marketing.Spend^2)+I(Marketing.Spend^3)
                           ,data=Startups[-c(49,50),])
summary(Model.Startups_Poly1) #Adjusted R2 Value is 96.01%#


vif(Model.Startups_Log)  # VIF is > 10 => collinearity
            
avPlots(Model.Startups_Log, id.n=2, id.cex=0.7) 

       ##### Final Model####
FinalModel<-lm(Profit~R.D.Spend+log(Administration)+Marketing.Spend+
                 log(State),data=Startups[-c(49,50),])

summary(FinalModel) #Adjusted R2 Value = 95.91%# 

# Prediction based on Final model ##
Profit_Predict <- predict(FinalModel,interval="predict")

Final <- cbind(Startups$R.D.Spend,Startups$Administration,Startups$Marketing.Spend,
               Startups$State,Startups$Profit,Profit_Predict)
View(Final)


  ## Evaluating  model LINE assumptions##
plot(FinalModel)# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance

qqPlot(FinalModel, id.n=5) 

library("MASS")
stepAIC(FinalModel) 

       ##Lower the AIC value better is the model ##

