     ###Packages required ##

library(kernlab)
library(caret)
library(plyr)
library(ggplot2)
library(e1071)

   ####Reading the CSV file##

FF <- read.csv("C:/Users/Admin34/Desktop/svm/forestfires (1).csv")
View(FF)
summary(FF)
str(FF)
    
     
       ##Visualisation####

plot <- function(a){
  r <- ggplot(FF,aes(a))+geom_histogram(color="black",fill="green")
  return(r)
}
plot1 <- function(a){
  c <- ggplot(FF,aes(a))+geom_boxplot(fill="red",color="black")+coord_flip()
  return(c)
}
(f1 <- plot(a=FF$FFMC))
(f2 <- plot1(a=FF$FFMC))
(d1 <- plot(a=FF$DMC))
(d2 <- plot1(a=FF$DMC))
(dc1 <- plot(a=FF$DC))
(dc2 <- plot1(a=FF$DC))
(i1 <- plot(a=FF$ISI))
(i2 <- plot1(a=FF$ISI))
(t1 <- plot(a=FF$temp))
(t2 <- plot1(a=FF$temp))
(r1 <- plot(a=FF$RH))
(r2 <- plot1(a=FF$RH))
(w1 <- plot(a=FF$wind))
(w2 <- plot1(a=FF$wind))

library(gridExtra)
grid.arrange(f1,f2,d1,d2,dc1,dc2,i1,i2,t1,t2,r1,r2,w1,w2,nrow=7,ncol=2)


normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
FF$temp = normalize(FF$temp)
FF$RH   = normalize(FF$RH)
FF$wind = normalize(FF$wind)
FF$rain = normalize(FF$rain)


    ### Converting the variables for analysis ##

FF$size_category<-as.factor(FF$size_category)
FF$month<-as.factor(FF$month)
FF$day<-as.factor(FF$day)


   ### Data Partition ###
set.seed(123)
ind <- sample(2, nrow(FF), replace = TRUE, prob = c(0.7,0.3))
FF_train <- FF[ind==1,]
FF_test  <- FF[ind==2,]

# kvsm() function uses gaussian RBF kernel 

# Building model 

model1<-ksvm(size_category~temp+rain+wind+RH, 
             data= FF_train,kernel = "vanilladot")

model1
Area_pred <- predict(model1, FF_test)
table(Area_pred,FF_test$size_category)
agreement <- Area_pred == FF_test$size_category
table(agreement)

prop.table(table(agreement))


       ### kernel = rfdot ###
model_rfdot<-ksvm(size_category~temp+rain+wind+RH, 
                  data= FF_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=FF_test)
mean(pred_rfdot==FF_test$size_category)  # 68.41%#


   ###Kernel=Vanilladot##
model_vanilla<-ksvm(size_category~temp+rain+wind+RH, 
                    data= FF_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=FF_test)
mean(pred_vanilla==FF_test$size_category) # 67.80%#

   ###kernel = besseldot###
model_besseldot<-ksvm(size_category~temp+rain+wind+RH, 
                      data= FF_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=FF_test)
mean(pred_bessel==FF_test$size_category) # 67.80%#


   ####kernel= polydot##
model_poly<-ksvm(size_category~temp+rain+wind+RH, 
                 data= FF_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = FF_test)
mean(pred_poly==FF_test$size_category) # 67.80%#

    
    ### Kernel=anovadot##
model_poly<-ksvm(size_category~temp+rain+wind+RH, 
                 data= FF_train,kernel = "anovadot")
pred_poly<-predict(model_poly,newdata = FF_test)
mean(pred_poly==FF_test$size_category) # 67.80%#


     #### Kernel=laplacedot##
model_poly<-ksvm(size_category~temp+rain+wind+RH, 
                 data= FF_train,kernel = "laplacedot")
pred_poly<-predict(model_poly,newdata = FF_test)
mean(pred_poly==FF_test$size_category) # 68.49%#

      ######$$$$$#####

