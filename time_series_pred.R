##### Forecast Model

###1. Time series 

### Read Data##
library(tseries)
library(forecast)

df = read.csv("E:/Users/mayur.v/Documents/Robadv/Data/Bitcoin.csv", sep = ",")
tail(df)
####Select Date & Price #####
df1 = data.frame(df$date,df$Price.USD.)
head(df1)

#### Plot Price vs Date

pt = plot(df1$df.Price.USD.,type = "l")

####3 Stationarity Check 

adf.test(diff(log(df1$df.Price.USD.)), alternative="stationary", k=0)

###if p<0.05 then its a stationary We
#see can check the p value of a series to find stationary enough to do any kind of time series modelling

#### Check for acf and pacf ###

acf(df1$df.Price.USD.)

acf(diff(log(df1$df.Price.USD.)))

pacf(diff(log(df1$df.Price.USD.)))

##### from looking at acf and pacf plots we got 
#### p = 0, q = 1, d = 0

###### fit the model #####
fit <- arima(log(df1$df.Price.USD.), c(0, 1, 0),seasonal = list(order = c(0, 1, 0), period = 12))
pred <- predict(fit, n.ahead = 10)


###### Auto fit ARima ######
ar = auto.arima(df1$df.Price.USD., seasonal=FALSE)


##### best Model ########(Series: df1$df.Price.USD. 
### ARIMA(2,2,0))
tsdisplay(residuals(fit), lag.max=45, main='(2,2,0) Model Residuals')

###
##There is a clear pattern present in ACF/PACF and model residuals plots
## repeating at lag 12. This suggests that our model may be better off
###with a different specification, such as p = 12 or q = 12. 
####


##### fit 2

fit2 = arima(df1$df.Price.USD., order=c(1,1,10))
fit2 = arima(df1$df.Price.USD., order=c(2,2,0))
fit2
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')
fcast <- forecast(fit2, h=5)
plot(fcast)

test  = data.frame(4174.73,4163.07,4338.71,4403.74,4409.32)

######### Test Results #

Act = c(4174.73,4163.07,4338.71,4403.74,4409.32)
Pred = c(4179.754, 4270.708, 4289.970, 4276.258, 4319.784)

a = Act-Pred


Avg1 = sum(4045.409, 4215.313)/2
Avg2 = sum(4084.724, 4319.936)/2
Avg3 = sum(4062.354, 4350.216)/2


#####Calculate the error term for forecast

e1 = 4045.409-4009.851 
e2 = 4084.724-4035.497
e3 = 4062.354-4002.108

e1 = (4215.313 - 4179.754)+(4045.409 - 4009.851)
e2 = (4319.936 - 4270.708)+(4084.724 - 4035.497)
e3 = (4350.216 - 4176.162)+(4062.354 - 4002.108)
