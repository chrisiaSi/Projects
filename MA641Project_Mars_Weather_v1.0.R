## Clear all variables in R
rm(list=ls())
## Clear all graphs
dev.off()

# Install required packages
install.packages('fpp2')
install.packages('TSstudio')
install.packages('Jmisc')

#Import required libraries
library(fpp2) #forecasting package
library(TSstudio)
library(tidyr)
library(CADFtest)
library(fBasics)
library(TSA)
library(forecast)
library(Jmisc)

# Retrieve the dataset
data <- read.csv("C:/Users/Chrisia/OneDrive - stevens.edu/Documents/Stevens 2nd Semester/MA 641 Time Series Analysis 1/Project/Seasonal_MarsWeather/R code Mars Weather/mars-weather.csv",header=T) 

#View the data and attributes
str(data)
summary(data)
data <- data[order(data$sol),]
View(data)

# calculate average temperature using mean of high and low temperatures
avg_temp = list((data$min_temp+data$max_temp)/2)

#Add the average temperature column to the original dataset
new_data <- cbind(data, avg_temp = avg_temp)
colnames(new_data)[11] <- "Average_Temp"

View(new_data)

#Create a new dataset with only the columns required for time series analysis: terrestrial_date and Average_Temp 
new_data1 = new_data[c("terrestrial_date","Average_Temp","month","sol")]
colnames(new_data1)[3] <- "Mars_Month"

#Filling missing NA time series data with previous observations
new_data1 <- na.locf(new_data1) 
View(new_data1)
# Data Preprocessing: Add a Martian year column which is missing in the data
# New years start from sol 1019 and 1688
# Set Year 0 = Sol 10 to 350, Year 1 = Sol 351 to 1018; Year 2 = Sol 1019 to 1687; Year 3 = Sol 1688 to 1977
new_data1$Mars_Year[new_data1$sol<351] <- 0
new_data1$Mars_Year[new_data1$sol>=351  & new_data1$sol<1019] <- 1
new_data1$Mars_Year[new_data1$sol>=1019 & new_data1$sol<1688] <- 2
new_data1$Mars_Year[new_data1$sol>=1688] <- 3

#Use only the numeric month values
new_data1$Mars_Month <- as.numeric(gsub("\\D", "", new_data1$Mars_Month))

View(new_data1)

#Save the data as time series entry
ts_temp <- ts(new_data1$Average_Temp, start = c(1,6), frequency=668) #657 days in a year in mars

#Performing Preliminary Analysis

#Time series plot
autoplot(ts_temp) + ggtitle("Mars weather time series data") + ylab("Temperature (degree C)") + xlab("Martian Days (sol)")

#  Aggregate 'Average_Temp' on months and year and get mean
aggregate_monthly_temp<-aggregate( Average_Temp ~ Mars_Month + Mars_Year , new_data1 , mean )
View(aggregate_monthly_temp)

ts_aggregate_monthly_temp <- ts(aggregate_monthly_temp$Average_Temp, start = c(0,6), frequency=12)

autoplot(ts_aggregate_monthly_temp) + ggtitle("Mars weather Montly Aggregate time series data") + ylab("Mean Monthly Temperature (degree C)") + xlab("Martian Month-Year")

ggseasonplot(ts_aggregate_monthly_temp) + ggtitle("Seasonal Plot: Mars weather time series data") + ylab("Temperature")
#Observation: Clear seasonality observed for Mar temperature values

#Converting ts into list of numbers
ts_aggregate_monthly_temp_num <- coredata(ts_aggregate_monthly_temp)
ts_temp_num <- coredata(ts_temp)

decompose_ts <- decompose(ts_aggregate_monthly_temp)
plot(decompose_ts$figure, type='b', las=2, xlab='Month')
plot(decompose_ts)

#Dickey Fuller Tests
#If the p-value is less than or equal to the specified significance level, the null hypothesis is rejected; otherwise, the null hypothesis is not rejected.

#Augmented Dickey-Fuller Test 
CADFtest(ts_aggregate_monthly_temp) #p-value is 1.673e-05<0.05, null hypothesis is rejected, consider alternative hypothesis
adf.test(ts_aggregate_monthly_temp) #p-value is 0.04909, 
#Result shows that it is stationary but we do further testing

#Kwiatkowski-Phillips-Schmidt-Shin ( KPSS Test)
kpss.test(ts_aggregate_monthly_temp, null= "Level") #level stationary, pvalue is 0.1
kpss.test(ts_aggregate_monthly_temp, null = "Trend") #trend stationary, pvalue is 0.1


#Autocorrelation, Partial Autocorrelation and Extended Autocorrelation Function

acf(ts_aggregate_monthly_temp_num, lag.max = 36) #3, clear seasonality pattern seen
pacf(ts_aggregate_monthly_temp_num, lag.max = 36) #1
eacf(ts_aggregate_monthly_temp, ar.max = 7, ma.max = 9)
basicStats(ts_aggregate_monthly_temp) #High variance of 54.353980


#Taking FIRST DIFFERENCE -------------------------------------------

autoplot(diff(ts_aggregate_monthly_temp), type='l') #No visible upward trend observed, seasonality still visible

basicStats(diff(ts_aggregate_monthly_temp)) #Variance 17.2

#Dickey Fuller Tests

#Augmented Dickey-Fuller Test 
CADFtest(diff(ts_aggregate_monthly_temp), type = "trend") #p-value is 0.02597<0.05, null hypothesis is rejected, consider alternative hypothesis
adf.test(diff(ts_aggregate_monthly_temp)) #p-value is 0.1964>0.05, we cannot reject null hypothesis, thus data is NOT stationary

#Kwiatkowski-Phillips-Schmidt-Shin ( KPSS Test)
kpss.test(diff(ts_aggregate_monthly_temp), null= "Level") #level stationary, pvalue is 0.1
kpss.test(diff(ts_aggregate_monthly_temp), null = "Trend") #trend stationary, pvalue is 0.1

#Autocorrelation, Partial Autocorrelation and Extended Autocorrelation Function
acf(diff(ts_aggregate_monthly_temp)) # 1 and 3s
pacf(diff(ts_aggregate_monthly_temp)) #2 and 1s
eacf(diff(ts_aggregate_monthly_temp), ar.max = 7, ma.max = 9) #0,1
#MA2 2 
#Observation: The general upward trend has now disappeared but the strong seasonality is still seen.

#Nonseasonal=MA(3) Seasonal=MA(5), MA(6)
#NS = MA(2) MA(3), s=MA(3) MA(6)

#FIRST DIFFRENCE and SEASONAL DIFFERENCE ------------------------

autoplot(diff(diff(ts_aggregate_monthly_temp), lag=12), type='l')
basicStats(diff(diff(ts_aggregate_monthly_temp), lag=12)) #Variance reduced to 1.7

#Dickey Fuller Tests

#Augmented Dickey-Fuller Test 
CADFtest(diff(diff(ts_aggregate_monthly_temp), lag=12), type = "trend") #p-value is 0.01<0.05, null hypothesis is rejected, consider alternative hypothesis
#adf.test(ts_aggregate_monthly_temp) #p-value is 0.08649>0.05 thus cannot reject null hypothesis, thus data is not stationary
#adf.test(diff(diff(ts_aggregate_monthly_temp), lag=12)) #p-value is 0.2722>0.05, we cannot reject null hypothesis, thus data is NOT stationary

#Kwiatkowski-Phillips-Schmidt-Shin ( KPSS Test)
kpss.test(diff(diff(ts_aggregate_monthly_temp), lag=12), null= "Level") #level stationary, pvalue is 0.1
kpss.test(diff(diff(ts_aggregate_monthly_temp), lag=12), null = "Trend") #trend stationary, pvalue is 0.1

#Autocorrelation, Partial Autocorrelation and Extended Autocorrelation Function
acf(as.vector(diff(diff(ts_aggregate_monthly_temp), lag=12))) #2 , lag.max=36,ci.type='ma'
pacf(diff(diff(ts_aggregate_monthly_temp), lag=12))
eacf(diff(diff(ts_aggregate_monthly_temp_num), lag=12), ar.max = 5, ma.max = 5)

####--------------------

#Splitting test and train data
train <- head(ts_aggregate_monthly_temp, round(length(ts_aggregate_monthly_temp) * 0.6))
h <- length(ts_aggregate_monthly_temp) - length(train)
test <- tail(ts_aggregate_monthly_temp, h)

#Forecasting methods
#SARIMA(0,1,0)(0,1,0)[12] model
fit_mle00 <- Arima((train), order = c(0,1,0), seasonal = c(0,1,0), method='ML')
fit_mle00 #AIC=32.32

#SARIMA(2,1,1)(1,1,3)[12] model
fit_mle21 <- Arima((train), order = c(2,1,1), seasonal = c(1,1,3), method='ML')
fit_mle21 #AIC=41.24

#SARIMA(3,1,1)(1,1,3)[12] model
fit_mle31 <- Arima((train), order = c(3,1,1), seasonal = c(1,1,3), method='ML')
fit_mle31 #AIC=41.63

#SARIMA(0,1,0)(0,1,0)[12] model
fit_css00 <- Arima((train), order = c(0,1,0), seasonal = c(0,1,0), method='CSS')
fit_css00 #Log-Likelihood=-15.16


best_fit_mle = fit_mle00

#Plotting the forecasted model
forecast_daily <- forecast(best_fit_mle, h=24)
autoplot(forecast_daily)+ autolayer(test) + theme(legend.position = 'top')

#plot(forecast_daily,n1=c(0,6),n.ahead=24,xlab='Year',type='b',ylab='Mars Temperature')

#Residual Analysis - Plot, acf, qqplot, hist, shapiro wilk, Ljung-Box, forecast
#Plot  residuals
autoplot(rstandard(fit_mle))

#ACF and PACF of residuals
acf(rstandard(fit_mle))
pacf(rstandard(fit_mle))

#Histogram of residuals
win.graph(width=3, height=3,pointsize=8)
hist(window(rstandard(fit_mle),start=c(0,6)),xlab='Standardized Residuals')

# qqplot of residuals
win.graph(width=2.5,height=2.5,pointsize=8)
qqnorm(window(rstandard(fit_mle),start=c(0,6)))
qqline(window(rstandard(fit_mle),start=c(0,6)))

# Ljung-Box test for correlation
Box.test(rstandard(fit_mle), lag=20, type = 'Ljung-Box') #p-value 0.95>0.05, thus the residuals are uncorrelated

#Shapiro-Wilk test for normality
shapiro.test(rstandard(fit_mle)) #p-value 0.0005<0.05, residuals are normal


# Forecasting the model using tbats for cross checking
fit <- tbats(train)
fit #ARMA(0,0) errors, Box-Cox transformation of 1, 2 Fourier pairs with period 12 months
seasonal <- !is.null(fit$seasonal)
paste("The data is seasonal = ",seasonal)
forecast_daily <- forecast(fit, h=24)
autoplot(forecast_daily)+ autolayer(test) + theme(legend.position = 'top')

