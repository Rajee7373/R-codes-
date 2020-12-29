
     ####Reading the CSV file##
bank <- read.csv("C:/Users/Admin34/Desktop/lr/bank-full (1).csv")
View(bank)

library(Amelia)
missmap(bank)
str(bank)
df <- as.data.frame(bank)

   #pre prepocessing the data##
   #convert data given in one column to 17 columns##
df1 <- as.character(df[,])
bank1 <- read.csv("C:/Users/Admin34/Desktop/lr/bank-full (1).csv",sep =";")
bank1

bank2 <- matrix(unlist(bank1),nrow=45211,byrow = T)
View(bank2)
colnames(bank2) <- c("age","job","marital","education","default","balance","housing","loan","contact","day","month","duration","campaign","pdays","previous","poutcome","y")
View(bank2)

bank3 <- as.data.frame(bank2)
str(bank3)
summary(bank3)
attach(bank3)
View(bank3)

    ## Converting the varaibles for analysis ####

columns<-c(2:8,11,16:17)
bank3[,columns]<-lapply(bank3[,columns] , factor)
str(bank3)

    #### Building the model ##

model <- glm(y~.,data = bank3)
summary(model)

   #### Confusion Matrix Table###
prd <- predict(model,type=c("response"),family="binomial")
View(prd)

confusion <- table(prd >0.5,bank3$y)
confusion

   ###model accuracy###
accuracy <- sum(diag(confusion))/sum(confusion)
accuracy  ##88.30%##

   #### ROC Curve ###
library(ROCR)
rocrpred<-prediction(prd,y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

     ### This model gives an accuracy of 88.30%##
     # More area under the ROC Curve better##
    ##is the logistic regression model obtained##
   

