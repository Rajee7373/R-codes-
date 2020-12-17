
     ####Reading the CSV file ###

card <- read.csv("C:/Users/Admin34/Desktop/lr/creditcard (1).csv")
View(card)
str(card)
summary(card)
attach(card)


  #### Converting the variable for analysis##
card$owner<-as.factor(card$owner)
card$selfemp<-as.factor(card$selfemp)
card$card<-as.factor(card$card)
card<-card[-1]
str(card)

  ###Building a model ##
 
model <- glm(card~card+reports+age+income+share+expenditure+owner+selfemp
             +dependents+months+majorcards+active,data = card,family = "binomial")

summary(model)#AIC=13360

 ##model selection by using the least AIC value##

model.null <- glm(card~1,data = card,family = "binomial")
model.full <- glm(card~.,data = card,family = "binomial")


  ## I  Forward selection##
step(model.null,scope = list(upper=model.full),direction = "forward")


  ## II  Backward elimination##
step(model.full,scope=list(lower=model.null,direction="backward"))


   ## III  Bidirectional elimination##
model.card <- glm(card~active+age+dependents+expenditure,data = card,family = "binomial")
step(model.card,scope = list(upper=model.full,lower=model.null),direction = "both")
# Bidirectional model has least AIC value.Hence these are
##best variable for prediction .AIC value of 128.5.


model.final <- glm(formula = card ~ active + age + dependents + expenditure + 
                     reports + majorcards, family = "binomial", data = card)
summary(model.final)

   #### Confusion Matrix Table###
prd <- predict(model.final,type=c("response"),data=test,family="binomial")
prd
length(prd)
confusion <- table(prd>0.5,card$card)
confusion
plot(prd)

    ####Model accuracy####
accuracy <- sum(diag(confusion))/sum(confusion)
accuracy ##98.40%#

   #### ROC Curve ###
library(ROCR)
rocrpred<-prediction(prd,card$card)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

        ### More area under the ROC Curve ,better is the 
         ##logistic regression model obtained.###
