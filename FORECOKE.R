##install.packages("forecast")
library(forecast)
##install.packages("fpp")
library(fpp)
##install.packages("smooth")
library(smooth)
##install.packages("tseries")
library(tseries)
library(readxl)

####  Reading the CSV file####

Coke_sales_rdata <-read.csv("C:/Users/Admin34/Downloads/CocaCola_Sales_Rawdata .csv")
View(Coke_sales_rdata)

    ##converting data into time series object###

tssales<-ts(Coke_sales_rdata$Sales,frequency=4,start=c(42))
View(tssales)

  ##dividing the data into training and testing set##
train<-tssales[1:38]
test<-tssales[39:42] ##considering only 4 qs since the data itself is q seasonal data


    ###converting time series object.size###
train<-ts(train,frequency=4)
test<-ts(test,frequency=4)

###plotting time series data
plot(tssales)
## visualization shows that it has level,trend,seasonality=>additive seasonality

###HoltWinters function###
#optimum values
##with alpha=0.2 which is default value
#assuming time series data has only one level parameter

hw_a<-HoltWinters(train,alpha=0.2,beta=F,gamma=F)### ALPHA=simple,beta for Holt,gamma for winter. 
hw_a                                                   
hwa_pred<-data.frame(predict(hw_a,n.ahead=4))

##  forecast values does not show  any train data##
plot(forecast(hw_a,h=4))
hwa_mape<-MAPE(hwa_pred$fit,test)*100
hwa_mape### mean avg 

##16.3, with alpha=0.2,beta=0.15(thumb rule)
##assuming data has trend an level parameter
hw_ab <- HoltWinters(train,alpha=0.2,beta=0.15,gamma=F)##double expo method##
hw_ab
hwab_pred<-data.frame(predict(hw_ab,n.ahead=4))

## values still missing some characters of train data
plot(forecast(hw_ab,h=4)) ## level,trend but no seasonality
hwab_mape<-MAPE(hwab_pred$fit,test)*100
hwab_mape   ### Here errors terms are reduced compared to previous
##8.74

### alpha=0.2,beta=0.15,gamma=0.05
##assuming data has level,trend ans seasonality
hw_abc<-HoltWinters(train,alpha=0.2,beta=0.15,gamma=0.05)### ALPHA=simple,beta for Holt,gamma for winter. 

hw_abc                                                 
hwabc_pred<-data.frame(predict(hw_abc,n.ahead=4))

### the forecasted values are similar to historical value ##
plot(forecast(hw_abc,h=4))### darker grey indicates prediction range,lighter grey is confidence range#
hwabc_mape<-MAPE(hwabc_pred$fit,test)*100
hwabc_mape### mean avg 
##3.58

   ##without optimum values##
hw_na<-HoltWinters(train,beta=F,gamma=F)
hw_na
hwna_pred<-data.frame(predict(hw_na,n.ahead=4))
hwna_pred
plot(forecast(hw_na,h=4))
hwna_mape<-MAPE(hwna_pred$fit,test)*100
hwna_mape
#9.09 ## better value than with alpha 


### without aplha and  beta values##
hw_nab<-HoltWinters(train,gamma=F)
hw_nab
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=4))
hwnab_pred
plot(forecast(hw_nab,h=4))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100
hwnab_mape
###8.62##

### without aplha, beta and gamma values##
hw_nabg<-HoltWinters(train)
hw_nabg
hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead=4))
hwnabg_pred
plot(forecast(hw_nabg,h=4))
hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100
hwnabg_mape
#####2.39###mean absolute percentage error##


## Based on the MAPE value, Holts winter exponential technique which assumes the time series
# Data level, trend, seasonality characters  with Default alpha, beta and gamma values.

new_model <- hw(tssales,alpha = NULL,beta = NULL,gamma = NULL)

plot(forecast(new_model,h=4))

# Forecasted values for the next 4 quarters
forecast_new <- data.frame(predict(new_model,h=4))


##### USING ses,holt,hw functions ##########
# Optimum values
# with alpha = 0.2
# Simple Exponential smoothing 

ses_a<-ses(train,alpha = 0.2) # 
ses_a
sesa_pred<-data.frame(predict(ses_a,h=4))
plot(forecast(ses_a,n.ahead=4))
sesa_mape<-MAPE(sesa_pred$Point.Forecast,test)*100

# with alpha = 0.2, beta = 0.1

holt_ab<-holt(train,alpha = 0.2,beta = 0.1)
holt_ab
holtab_pred<-data.frame(predict(holt_ab,h=4))
plot(forecast(holt_ab,h=4))
holtab_mape<-MAPE(holtab_pred$Point.Forecast,test)*100

# with alpha = 0.2, beta = 0.1, gamma = 0.1 

hw_abg_new<-hw(train,alpha = 0.2,beta = 0.1,gamma = 0.1)
hw_abg_new
hwabg_pred_new<-data.frame(predict(hw_abg_new,h = 4))
plot(forecast(hw_abg_new,h=4))
hwabg_mape_new<-MAPE(hwabg_pred_new$Point.Forecast,test)*100

# With out optimum values 

# simple exponential method

ses_na<-ses(train,alpha=NULL)
ses_na
sesna_pred<-data.frame(predict(ses_na,h = 4))
sesna_pred
plot(forecast(ses_na,h=4))
sesna_mape<-MAPE(sesna_pred$Point.Forecast,test)*100

# Holts winter method 

holt_nab<-holt(train,alpha = NULL,beta = NULL)
holt_nab
holtnab_pred<-data.frame(predict(holt_nab,h=4))
holtnab_pred
plot(forecast(holt_nab,h=4))
holtnab_mape<-MAPE(holtnab_pred$Point.Forecast,test)*100

# Holts winter Exponential method

hw_nabg_new<-hw(train,alpha=NULL,beta=NULL,gamma = NULL)
hw_nabg_new
hwnabg_pred_new<-data.frame(predict(hw_nabg_new,h=4))
hwnabg_pred_new
plot(forecast(hw_nabg_new,h=4))
hwnabg_mape_new<-MAPE(hwnabg_pred_new$Point.Forecast,test)*100

df_mapes_new<-data.frame(c("sesa_mape","holtab_mape","hwabg_mape_new","sesna_mape","holtnab_mape","hwnabg_mape_new"),c(sesa_mape,holtab_mape,hwabg_mape_new,sesna_mape,holtnab_mape,hwnabg_mape_new))
colnames(df_mapes_new)<-c("MAPE","VALUE")
View(df_mapes_new)

# Based on the MAPE value , HoltsWinter's exponential technique which assumes the time series
# Data level, trend, seasonality  with default aplha,beta and gamma values.

new_model <- hw(tssales,alpha = NULL,beta = NULL,gamma = NULL)

plot(forecast(new_model,h=4))

     #### Forecasted values for the next 4 quarters
forecast_new <- data.frame(predict(new_model,h=4))

        ################$$$$$$$$$$$$##############


