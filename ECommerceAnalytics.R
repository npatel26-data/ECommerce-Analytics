#******************************************************************************************************
install.packages('forecast')
install.packages('zoo')
install.packages('ggplot2')
install.packages('dplyr')
library(forecast)
library(zoo)
library(ggplot2)
library(dplyr)

# Set working directory for locating files.
setwd("~/Downloads")

# Create data frame
Sales.data <- read.csv("tsadjustedsales_copy_ Final.csv")
head(Sales.data)
tail(Sales.data) # This excel file has data from 2000 Q1 to 2020 Q1

# Selecting required columns 
Sales_final.data <- Sales.data

# See the first 6 records of the file.
head(Sales_final.data)
tail(Sales_final.data)
#----------------------------------------------------------------------------------------------------
#*******************************DATA EXPLORATION OF ECOMMERCE SALES*************************************
#---------------------------------------------------------------------------------------------------
# Create Time Series DataSet For Ecommerce Sales 
head(Sales_final.data$EcommerceSales_Total)
Sales.ts <- ts(Sales_final.data$EcommerceSales_Total, 
                start = c(1999, 4), end = c(2020, 1), freq = 4)

# Use Acf() Function To Identify Ecommerce Time Series Components
autocor_Sales <- Acf(Sales.ts, lag.max = 12, main = "Autocorrelation for Ecommerce Sales Data") 
Lag <- round(autocor_Sales$lag, 0)
ACF <- round(autocor_Sales$acf, 3)
data.frame(Lag, ACF)

head(Sales.ts)
# Use plot() Function To plot Quaterly Ecommerce Time Series Data
plot(Sales.ts, 
     xlab = "TimeLine", ylab = "Revenue", 
     ylim = c(4000, 170000), main = "Ecommerce Sales", col = "dark blue", lwd = 3)
lines(ma(Sales.ts,9),col="red",lwd=3)

# Use stl() Function to plot Times Series Components of the Original Data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
Sales.stl <- stl(Sales.ts, s.window = "periodic")
autoplot(Sales.stl, main = "Ecommerce Time Series Component")
#---------------------------------------------------------------------------------------------------------
#*************************************DATA PARTITIONING FOR ECOMMERCE***************************************************
#---------------------------------------------------------------------------------------------------------
# creating data partition for Ecommerce with the validation partition of 12 Quarters and training 
# partition of 70 Quarters.

nValid.ad <- 12  #last 4 years of data being alloted to Validation data (2017 Q2 to 2020 Q1)
nValid.ad
nTrain.ad <- length(Sales.ts) - nValid.ad
nTrain.ad
train.ts.ad <- window(Sales.ts, start = c(1999, 4), end = c(1999, 3+nTrain.ad))
train.ts.ad
valid.ts.ad <- window(Sales.ts, start = c(1999, nTrain.ad + 4), 
                      end = c(1999, nTrain.ad +3+ nValid.ad))
valid.ts.ad

#-----------------------------------------------------------------------------------------------------------------------
#*********************************Execution of Forecasting Models*******************************************************
#-----------------------------------------------------------------------------------------------------------------------
#_____________________MODEL1 - 2 level Model (Regression + MA Trailing for Residuals)______________________________________________________
#***************************************************************************************************************************
# Regression Model with Quadratic Trend and Seasonality
reg.trend.seas <- tslm(train.ts.ad ~ trend + I(trend^2) + season)
summary(reg.trend.seas)
# Creating regression forecast for the 12 Quarters of the validation period.
reg.trend.seas.pred <- forecast(reg.trend.seas, h = 12, level = 0)
reg.trend.seas.pred
# Identifying and displaying residuals for time series based on the regression
reg.trend.seas.res <- reg.trend.seas$residuals 
reg.trend.seas.res
# Applying trailing MA with 4 Quarters in the window to residuals.
ma.trailing.res_4 <- rollmean(reg.trend.seas.res, k = 4 , align = "right") 
ma.trailing.res_4
# Creating forecast for residuals for the 12 Quarters of the validation period
ma.trailing.res_4.pred <- forecast(ma.trailing.res_4, h = 12, level = 0) 
ma.trailing.res_4.pred
# combining regression forecast and trailing MA forecast for residuals.
ts.forecast.4 <- reg.trend.seas.pred$mean + ma.trailing.res_4.pred$mean 
ts.forecast.4
# Creating a table with regression forecast, trailing MA for residuals and total forecast for 
# 12 Quarters into the future
total.reg.ma.pred <- data.frame(reg.trend.seas.pred$mean, ma.trailing.res_4.pred$mean, 
                                ts.forecast.4)
total.reg.ma.pred 

# Plot training data and regression model.
plot(train.ts.ad, 
     xlab = "Time", ylab = "Sales (in millions of Euros)", ylim = c(4000, 180000), bty = "l",
     xaxt = "n", xlim = c(2000, 2022), lwd =2,
     main = "Training series and Regression with Trend and Seasonality") 
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))
lines(reg.trend.seas$fitted, col = "brown", lwd = 2)
lines(reg.trend.seas.pred$mean, col = "brown", lty = 5, lwd = 2)
lines(valid.ts.ad)
valid.ts.ad
legend(2002,140000, legend = c("Training series", "Regression",
                             "Regression Forecast for 12 Periods into Validation"), 
       col = c("black", "brown" , "brown"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")
lines(c(2020 - 2.75, 2020 - 2.75), c(4000, 180000))
lines(c(2020.25, 2020.25), c(4000, 180000))
text(2007, 180000, "Training")
text(2018.50, 180000, "Validation")
text(2021, 180000, "Future")
arrows(2016, 170000, 2000, 170000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.25, 170000, 2020.25, 170000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.50, 170000, 2021.50, 170000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

reg.trend.seas.res
# Plot regression residuals data and trailing MA based on residuals.
plot(reg.trend.seas.res, 
     xlab = "Time", ylab = "Sales (in millions of Euros)", bty = "l",
     xaxt = "n", xlim = c(2000, 2022),ylim=c(-5000,9000),lwd =2,
     main = "Regression Residuals and Trailing MA for Residuals, k =4") 
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))
lines(ma.trailing.res_4, col = "blue", lwd = 2, lty = 1)
lines(ma.trailing.res_4.pred$mean, col = "blue", lwd = 2, lty = 5)
legend(2002,9000, legend = c("Regresssion Residuals", "Trailing MA for Residuals, k=4",
                            "Trailing MA Forecast for 12 Periods into validation"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Use accuracy() function to identify common accuracy measures for regression model and two level forecast.
round(accuracy(reg.trend.seas.pred, valid.ts.ad), 3) 
round(accuracy(ts.forecast.4, valid.ts.ad), 3)
#-----------------------------------------------------------------------------------------------------------------------
#____________________________MODEL 2 - Holt Winter's Model ______________________________________________________
#***************************************************************************************************************************
hw.ZZZ.train.ad <- ets(train.ts.ad, model = "ZZZ")  #Model Received is AAN
hw.ZZZ.train.ad 

hw.ZZZ.train.pred.ad <- forecast(hw.ZZZ.train.ad, h = nValid.ad, level = 0)
hw.ZZZ.train.pred.ad

# Plot hw predictions for training data, optimal smoothing parameters.
plot(hw.ZZZ.train.pred.ad, 
     xlab = "Time", ylab = "Ecomeerce Revenue Sales", ylim = c(4000, 180000), bty = "l",
     xaxt = "n", xlim = c(1999, 2022), 
     main = "Holt-Winter's Model with Automated Selection of Model Options", flty = 2) 
axis(1, at = seq(1999, 2022, 1), labels = format(seq(1999, 2022, 1)))
lines(hw.ZZZ.train.pred.ad$fitted, col = "blue", lwd = 3)
lines(valid.ts.ad)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2017.25, 2017.25), c(4000, 180000))
lines(c(2020.25, 2020.25), c(4000, 180000))
text(2007, 180000, "Training")
text(2018.50, 180000, "Validation")
text(2021, 180000, "Future")
arrows(2016, 170000, 2000, 170000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.25 , 170000, 2020.25, 170000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 170000, 2021.75, 170000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
legend(2000,170000, legend = c("Ecommerce Revenue time series", "Holt-Winter's Model for Training Data",
                             "Holt-Winter's Model for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

round(accuracy(hw.ZZZ.train.pred.ad, valid.ts.ad), 3)
#-----------------------------------------------------------------------------------------------------------------------
#____________________________MODEL 3 - Regression ______________________________________________________
#***************************************************************************************************************************
#Model 1: Regression model with linear trend
train.ad.lin <- tslm(train.ts.ad ~ trend)
summary(train.ad.lin)

#Forecasting for Validation period:
train.ad.lin.pred <- forecast(train.ad.lin, h = nValid.ad, level = 0)
train.ad.lin.pred

# Plot ts data, linear trend and predictions for validation period.
plot(train.ad.lin.pred, 
     xlab = "Time", ylab = "Sales (in millions of Euros)", bty = "l",ylim=c(4000, 180000),
     xlim = c(2000, 2022), main = "Linear Trend for Training and Validation data", flty = 2) 
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))
lines(train.ad.lin.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts.ad, col = "black", lty = 1)
legend(2000,172000, legend = c("Ecommerce Revenue time series", "Linear Regression for Training Data",
                             "Linear forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020 - 2.75, 2020 - 2.75), c(4000, 180000))
lines(c(2020.25, 2020.25), c(4000, 180000))
text(2007, 180000, "Training")
text(2018.50, 180000, "Validation")
text(2021, 180000, "Future")
arrows(2016, 172000, 2000, 172000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.50, 172000, 2019.50, 172000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 172000, 2021.75, 172000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#---------------------------------------------------------------------------------------------------
#Model 2: Regression Model with Exponential Trend:
train.ad.expo <-tslm(train.ts.ad ~ trend, lambda = 0)
summary(train.ad.expo)

#Forecasting for Validation period:
train.ad.expo.pred <- forecast(train.ad.expo, h = nValid.ad, level = 0)
train.ad.expo.pred

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.ad.expo.pred, 
     xlab = "Time", ylab = "Sales (in millions of dollar)", bty = "l",ylim=c(4000, 180000),
     xlim = c(2000, 2022), main = "Exponential Trend for Training and Validation Data", flty = 2) 
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))
lines(train.ad.expo.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts.ad, col = "black", lty = 1)
lines(train.ts.ad,col="black",lty=1)
legend(2000,172000, legend = c("Ecommerce Revenue time series", "Exponential Trend for Training Data",
                             "Exponential forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020 - 2.75, 2020 - 2.75), c(4000, 180000))
lines(c(2020.25, 2020.25), c(4000, 180000))
text(2007, 180000, "Training")
text(2018.50, 180000, "Validation")
text(2021, 180000, "Future")
arrows(2016, 172000, 2000, 172000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.50, 172000, 2019.50, 172000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 172000, 2021.75, 172000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#---------------------------------------------------------------------------------------------------
#Model 3: Regression mode with quadratic trend
train.ad.quad <- tslm(train.ts.ad~ trend + I(trend^2))
summary(train.ad.quad)

#Forecasting for Validation period:
train.ad.quad.pred <- forecast(train.ad.quad, h = nValid.ad, level = 0)
train.ad.quad.pred

# Plot ts data, quadratic trend and predictions for validation period.
plot(train.ad.quad.pred, 
     xlab = "Time", ylab = "Sales (in millions of dollers)", bty = "l",ylim=c(4000, 180000),
     xlim = c(2000, 2022), main = "Quadratic Trend for Training and Validation Data", flty = 2) 
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))
lines(train.ad.quad.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts.ad, col = "black", lty = 1)
legend(2000,172000, legend = c("Ecommerce Revenue time series", "Quadratic Trend for Training Data",
                             "Quadratic Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020 - 2.75, 2020 - 2.75), c(4000, 180000))
lines(c(2020.25, 2020.25), c(4000, 180000))
text(2007, 180000, "Training")
text(2018.50, 180000, "Validation")
text(2021, 180000, "Future")
arrows(2016, 172000, 2000, 172000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.50, 172000, 2019.50, 172000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 172000, 2021.75, 172000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#---------------------------------------------------------------------------------------------------

#Model 4:Regression model with seasonality
train.ad.season <- tslm(train.ts.ad ~ season) 
summary(train.ad.season)

#Forecasting for Validation period:
train.ad.season.pred <- forecast(train.ad.season, h = nValid.ad, level = 0)
train.ad.season.pred

# Plot ts data, linear trend and predictions for validation period.
plot(train.ad.season.pred, 
     xlab = "Time", ylab = "Sales (in millions of dollers)", bty = "l",ylim=c(4000, 180000),
     xlim = c(2000, 2022), main = "Model with Seasonality for Training and Validation Data", flty = 2) 
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))
lines(train.ad.season.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts.ad, col = "black", lty = 1)
legend(2000,172000, legend = c("Ecommerce Revenue time series", "Seasonality Model Training Data",
                             "Seasonality forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020 - 2.75, 2020 - 2.75), c(4000, 180000))
lines(c(2020.25, 2020.25), c(4000, 180000))
text(2007, 180000, "Training")
text(2018.50, 180000, "Validation")
text(2021, 180000, "Future")
arrows(2016 , 172000, 2000, 172000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.50, 172000, 2019.50, 172000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 172000, 2021.75, 172000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#---------------------------------------------------------------------------------------------------
#Model 5: Regression model with quadratic trend and seasonality.
train.ad.trend.season <- tslm(train.ts.ad ~ trend + I(trend^2) + season)
summary(train.ad.trend.season)

train.ad.trend.season.pred <- forecast(train.ad.trend.season, h = nValid.ad, level = 0)
train.ad.trend.season.pred

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.ad.trend.season.pred, 
     xlab = "Time", ylab = "Sales (in millions of Euros)", bty = "l",ylim=c(4000, 180000),
     xlim = c(2000, 2022), main = "Model with Quadratic Trend and Quarterly Seasonality", flty = 2) 
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))
lines(train.ad.trend.season.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts.ad, col = "black", lty = 1)
legend(2000,172000, legend = c("Ecommerce Revenue time series", "Trend and Seasonality Model Training Data",
                             "Trend and Seasonality for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020 - 2.75, 2020 - 2.75), c(4000, 180000))
lines(c(2020.25, 2020.25), c(4000, 180000))
text(2007, 180000, "Training")
text(2018.50, 180000, "Validation")
text(2021, 180000, "Future")
arrows(2016, 172000, 2000, 172000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.50, 172000, 2019.50, 172000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 172000, 2021.75, 172000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

train.ad.trend.season.pred$residuals
# Plot residuals of predictions with trend and seasonality.
plot(train.ad.trend.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", bty = "l",
     xlim = c(2000, 2022), main = "Residuals for Trend and Seasonality Model", 
     col = "brown", lwd = 2,ylim=c(-5000,30000)) 
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))
lines(valid.ts.ad - train.ad.trend.season.pred$mean, col = "brown", lty = 1, lwd=2)
lines(c(2020 - 2.75, 2020 - 2.75), c(-5000, 25000))
lines(c(2020.25, 2020.25), c(-5000, 25000))
text(2007, 27000, "Training")
text(2018.50, 27000, "Validation")
text(2021, 27000, "Future")
arrows(2016, 25000, 2000, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.50 , 25000, 2019.50, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 25000, 2021.75, 25000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#-------------------------------------------------------------------------------------------------------
#Comparing Accuracy for Above 5 MLR models For Ecommerce Sales Validation Data

#Accuracy for Model 1: Regression model with linear trend
round(accuracy(train.ad.lin.pred, valid.ts.ad), 3)  

#Accuracy for Model2:  Regression Model with Exponential Trend:
round(accuracy(train.ad.expo.pred, valid.ts.ad), 3) 

#Accuracy for Model 3:Regression mode with quadratic trend
round(accuracy(train.ad.quad.pred, valid.ts.ad), 3)  

#Accuracy for Model 4:Regression model with seasonality
round(accuracy(train.ad.season.pred, valid.ts.ad), 3)  

#Accuracy for Model 5: Regression model with quadratic trend and seasonality.  
round(accuracy(train.ad.trend.season.pred, valid.ts.ad),3) 

#-----------------------------------------------------------------------------------------------------------------------
#____________________________MODEL 4 - Autocorrelation and Autoregressive model for Ecommerce ______________________________________________________
#***************************************************************************************************************************
## FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY.
train.trend.season <- tslm(train.ts.ad ~ trend + I(trend^2) + season)
summary(train.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonal model in validation set.  
train.trend.season.pred <- forecast(train.trend.season, h = nValid.ad, level = 0)
train.trend.season.pred

# plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.trend.season.pred, 
     xlab = "Time", ylab = "ADIDAS Sales (in millions of Euros) ", ylim = c(4000, 180000), bty = "l",
     xaxt = "n", xlim = c(2000, 2022), 
     main = "Regression with Quadratic Trend and Seasonality", lwd = 2, flty = 5) 
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))
lines(train.trend.season.pred$fitted, col = "red", lwd = 2)
lines(valid.ts.ad, col = "black", lwd = 2, lty = 1)
legend(2000,172000, legend = c("Ecommerce Time Series", "Regression for Training Data",
                             "Forecast for Validation Data"), 
       col = c("black", "red" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020 - 2.75, 2020 - 2.75), c(4000, 180000))
lines(c(2020.25, 2020.25), c(4000, 180000))
text(2007, 180000, "Training")
text(2018.5, 180000, "Validation")
text(2021, 180000, "Future")
arrows(2016, 172000, 2000, 172000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.50, 172000, 2019.50, 172000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.25, 172000, 2021.75, 172000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

train.trend.season.pred$residuals
# Plot residuals of the predictions with trend and seasonality.
plot(train.trend.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", ylim = c(-6000, 25000), bty = "l",
     xaxt = "n", xlim = c(2000, 2022), 
     main = "Regresssion Residuals for Training and Validation Data", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))
lines(valid.ts.ad - train.trend.season.pred$mean, col = "brown", lwd = 2, lty = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020 - 2.75, 2020 - 2.75), c(-6000, 25000))
lines(c(2020.25, 2020.25), c(-6000, 25000))
text(2008, 25000, "Training")
text(2018.50, 25000, "Validation")
arrows(2016, 22000, 2000, 22000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.50, 22000, 2019.50, 22000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to identify autocorrelation for the model residuals 
Acf(train.trend.season.pred$residuals, lag.max = 8, 
    main = "Autocorrelation for Ecommerce Training Residuals")
Acf(valid.ts.ad - train.trend.season.pred$mean, lag.max = 8, 
    main = "Autocorrelation for Ecommerce Validation Residuals")

## USE Arima() FUNCTION TO CREATE AR(1) MODEL FOR TRAINING RESIDUALS.
res.ar1 <- Arima(train.trend.season$residuals, order = c(1,0,0))
summary(res.ar1)

# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid.ad, level = 0)
res.ar1.pred

# Develop a data frame to demonstrate the training AR model results 
# vs. original training series, training regression model, 
# and its residuals.  
train.df <- data.frame(train.ts.ad, train.trend.season$fitted, 
                       train.trend.season$residuals, res.ar1$fitted, res.ar1$residuals)
names(train.df) <- c("Ecommerce Revenue", "Regression", "Residuals",
                     "AR.Model", "AR.Model.Residuals")
train.df

train.trend.season.pred$residuals
# Plot residuals of the predictions for training data before AR(1).
plot(train.trend.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", ylim = c(-6000, 15000), bty = "l",
     xaxt = "n", xlim = c(2000, 2020), 
     main = "Regresssion Residuals for Training Data before AR(1)", 
     col = "brown", lwd = 3) 
axis(1, at = seq(2000, 2020, 1), labels = format(seq(2000, 2020, 1)))

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020 - 2.75, 2020 - 2.75), c(-6000, 15000))
text(2010, 15000, "Training")
arrows(2016, 12000, 2000, 12000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

res.ar1$residuals
# Plot residuals of the residuals for training data after AR(1).
plot(res.ar1$residuals, 
     xlab = "Time", ylab = "Residuals", ylim = c(-5000, 2500), bty = "l",
     xaxt = "n", xlim = c(2000, 2020), 
     main = "Residuals of Residuals for Training Data after AR(1)", 
     col = "brown", lwd = 3) 
axis(1, at = seq(2000, 2020, 1), labels = format(seq(2000, 2020, 1)))

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020 - 2.75, 2020 - 2.75), c(-5000, 2500))
text(2010, 2500, "Training")
arrows(2016, 2000, 2000, 2000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to identify autocorrelation for the training 
# residual of residuals and plot autocorrrelation for different lags 
Acf(res.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Ecommerce Training Residuals of Residuals")

# Create two-level modeling results, regression + AR(1) for validation period.
valid.two.level.pred <- train.trend.season.pred$mean + res.ar1.pred$mean
train.trend.season.pred$mean 
res.ar1.pred$mean
valid.two.level.pred
# Plot two-level modeling results, regression + AR(1) for validation period.
plot(valid.two.level.pred, 
     xlab = "Time", ylab = "Sales (in milions of dollers)", ylim = c(4000, 180000), bty = "l",
     xaxt = "n", xlim = c(2000, 2022), 
     main = "Two level(Quadratic trend and Seasonality + AR(1)) Model", lwd = 2, lty = 5, col = "blue") 
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))
lines(train.trend.season.pred$fitted , col = "blue", lwd = 2)
lines(valid.ts.ad, col = "black", lwd = 2, lty = 1)
lines(train.ts.ad, col = "black", lwd = 2, lty = 1)
legend(2000,170000, legend = c("Ecommerce Time Series", "Two level Model for Training Period",
                             "Two level Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020 - 2.75, 2020 - 2.75), c(4000, 180000))
lines(c(2020.25, 2020.25), c(4000, 180000))
text(2007, 172000, "Training")
text(2018.50, 172000, "Validation")
text(2021, 172000, "Future")
arrows(2016, 170000, 2000, 170000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.5 , 170000, 2019.5, 170000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.5, 170000, 2021.5, 170000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
# Create data table with historical validation data, regression forecast
# for validation period, AR(1) for validation, and two level model results.
valid.ts.ad
train.trend.season.pred$mean
res.ar1.pred$mean
valid.two.level.pred
valid.df <- data.frame(valid.ts.ad, train.trend.season.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Ecommerce Revenue", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df

# Use accuracy() function to identify common accuracy measures for validation period forecast:
# (1) two-level model (quadratic trend and seasonal model + AR(1) model for residuals)
round(accuracy(valid.two.level.pred, valid.ts.ad), 3) 
#-----------------------------------------------------------------------------------------------------------------------
#____________________________MODEL 5 - ARIMA model for Ecommerce ______________________________________________________
#***************************************************************************************************************************
## FIT AUTO ARIMA MODEL.

# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima.ad <- auto.arima(train.ts.ad)
summary(train.auto.arima.ad)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.ad.pred <- forecast(train.auto.arima.ad, h = nValid.ad, level = 0)
train.auto.arima.ad.pred


# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.auto.arima.ad.pred, 
     xlab = "Time", ylab = "Ecommerce Sales (in milions)", ylim = c(4000, 185000), bty = "l",
     xaxt = "n", xlim = c(2000, 2022), 
     main = "Auto ARIMA Model", lwd = 2, flty = 5) 
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))
lines(train.auto.arima.ad.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts.ad, col = "black", lwd = 2, lty = 1)
legend(2000,172000, legend = c("Ecommerce Time Series", "Auto ARIMA Model for Training Period",
                             "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020 - 2.75, 2020 - 2.75), c(4000, 185000))
lines(c(2020, 2020), c(4000, 185000))
text(2007, 185000, "Training")
text(2018.50, 185000, "Validation")
text(2021, 185000, "Future")
arrows(2016, 180000, 2000, 180000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.25, 180000, 2020.25, 180000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.50, 180000, 2021.50, 180000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Accuracy on the validation dataset
round(accuracy(train.auto.arima.ad.pred, valid.ts.ad), 3)

#-----------------------------------------------------------------------------------------------------------------------
#____________________________MODEL 6 - Facebook's Prophet model for Ecommerce ______________________________________________________
#***************************************************************************************************************************
#installing fb prophet packages
install.packages('prophet')
library(prophet)
install.packages('Rcpp')
install.packages('rlang')

#reading dataset file in data frame
df = read.csv("tsadjustedsales_copy_ Final_Facebook.csv",header = TRUE)
head(df)

#selecting date and e-commerce sales columns for prediction
final_df = df[,c('ds','y')]
head(final_df)

#applying fb prophet model
m=prophet(final_df)
future= make_future_dataframe(m,periods =6,freq = 'quarter')
head(future)
tail(future)

#Generating predictions with fb prophet
forcast = predict(m,future)
tail(forcast[c('ds','yhat','yhat_lower','yhat_upper')])
plot(m,forcast)
prophet_plot_components(m,forcast)
##################################################
df = read.csv("tsadjustedsales_Final_Facebook_2020Q4.csv",header = TRUE)
head(df)
tail(df)
final_df = df[,c('ds','y')]
head(final_df)
tail(final_df)
m=prophet(final_df)
future= make_future_dataframe(m,periods =9,freq = 'quarter')
head(future)
tail(future)
forcast = predict(m,future)
tail(forcast[c('ds','yhat','yhat_lower','yhat_upper')])
plot(m,forcast)
prophet_plot_components(m,forcast)
###################################################
# Given actual data with Covid numbers (2020Q4) 
df = read.csv("tsadjustedsales_copy_ Final_Facebook.csv",header = TRUE)
head(df)
tail(df)
final_df = df[,c('ds','y')]
head(final_df)
tail(final_df)
m=prophet(final_df)
df.cv=cross_validation(m,horizon= 365,units = 'days')
df.p=performance_metrics(df.cv)
head(df.p)
tail(df.p)
plot(m,forcast)
prophet_plot_components(m,forcast)
################### Cheking accuracy on validation dataset ##############
library(Metrics)
install.packages('Metrics')
actual=c(112644,115419,121019,124936,128616,130625,134291,139713,146394,153274,156581,160414)
predicted=c(108306,111179,113523,116241,119160,122012,124710,127480,130414,133251,136679,139954)
result=rmse(actual,predicted)
print(result)
result=mape(actual,predicted)
print(result)
#_________________________________________________________________________________________________________________________________
#******************COMPARING ALL ABOVE MODELS ON VALIDATION DATA*************************************
# Accuracy measure for two level MA trailing model
round(accuracy(ts.forecast.4, valid.ts.ad), 3) 
# Accuracy measure for Holt's Winter model
round(accuracy(hw.ZZZ.train.pred.ad, valid.ts.ad), 3) 
#Accuracy for Model 1:  Regression Model with Linear Trend
round(accuracy(train.ad.lin.pred, valid.ts.ad), 3)
#Accuracy for Model 2:  Regression Model with Exponential Trend
round(accuracy(train.ad.expo.pred, valid.ts.ad), 3) 
#Accuracy for Model 3:Regression mode with quadratic Trend
round(accuracy(train.ad.quad.pred, valid.ts.ad), 3)  
#Accuracy for Model 4:Regression model with seasonality
round(accuracy(train.ad.season.pred, valid.ts.ad), 3) 
#Accuracy for Model 5: Regression model with quadratic Trend and seasonality
round(accuracy(train.ad.trend.season.pred, valid.ts.ad),3) 
# Accuracy measure for two level AutoRegressive model
round(accuracy(valid.two.level.pred, valid.ts.ad), 3) 
# Accuracy measure for ARIMA model
round(accuracy(train.auto.arima.ad.pred, valid.ts.ad), 3) 
#_________________________________________________________________________________________________________________________
#***********Running above THREE BEST MODEL *****************************************
#--------------------------------------------------------------------------------------------------------------------------
# Model 1: ARIMA model for Ecommerce
Ecom.ARIMA <- auto.arima(Sales.ts)
summary(Ecom.ARIMA)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
Ecom.ARIMA.pred <- forecast(Ecom.ARIMA, h = 7, level = 0)
Ecom.ARIMA.pred

# Accuracy measure for entire data set
round(accuracy(Ecom.ARIMA.pred$fitted, Sales.ts),3) # RMSE = , MAPE = 

#====================================================================================

# Model 2: Holt's Winter Model for Ecommerce prediction
hw.ZZZ.Ecom <- ets(Sales.ts, model = "ZZZ")  #Model Received is AAN
hw.ZZZ.Ecom 

hw.ZZZ.Ecom.pred <- forecast(hw.ZZZ.Ecom, h = 8, level = 0)
hw.ZZZ.Ecom.pred
# Accuracy measure for entire data set
round(accuracy(hw.ZZZ.Ecom.pred$fitted, Sales.ts),3)

#######################Code of new prediction models ##############################
Sales_new.data <- read.csv("tsadjustedsales_copy_ Final_prediction.csv")
head(Sales_new.data)
tail(Sales_new.data) # This excel file has data from 1999 Q4 to 2020 Q4

# Selecting required columns 
Sales_new.datafinal.data <- Sales_new.data
Sales_new.ts <- ts(Sales_new.datafinal.data$EcommerceSales_Total, 
               start = c(1999, 4), end = c(2020, 4), freq = 4)
#################
# Model 1: ARIMA model for Ecommerce
Ecom.ARIMA_new <- auto.arima(Sales_new.ts)
summary(Ecom.ARIMA_new)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
Ecom.ARIMA_new.pred <- forecast(Ecom.ARIMA_new, h = 7, level = 0)
Ecom.ARIMA_new.pred

# Accuracy measure for entire data set
round(accuracy(Ecom.ARIMA_new.pred$fitted, Sales.ts),3) 
##################
# Model 2: Holt's Winter Model for Ecommerce prediction of 2021Q1- 2021Q4
hw.ZZZ_new.Ecom <- ets(Sales_new.ts, model = "ZZZ")  #Model Received is MAN
hw.ZZZ_new.Ecom 

hw.ZZZ.Ecom_new.pred <- forecast(hw.ZZZ_new.Ecom, h = 7, level = 0)
hw.ZZZ.Ecom_new.pred
# Accuracy measure for entire data set
round(accuracy(hw.ZZZ.Ecom_new.pred$fitted, Sales_new.ts),3)

#------------------------------------------------------------------------------------------