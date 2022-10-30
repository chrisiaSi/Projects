## Clear all variables in R
rm(list=ls())
## Clear all graphs
dev.off()

install.packages('quantmod')
install.packages('aTSA')

library(quantmod)
library(xts)
library(TSA)
library(CADFtest)
library(forecast)
library(stats)
library(FinTS)
library(rugarch)
library(fBasics)
library(PerformanceAnalytics)

startDate = as.Date("2012-01-01") #Specify period of time we are interested in
endDate = as.Date("2022-01-01")

getSymbols("^IXIC", from = startDate, to = endDate, warnings = FALSE, auto.assign = TRUE)

head(IXIC)
str(IXIC)
nasdaq.AdjClose <- IXIC$IXIC.Adjusted
week <- format(as.Date(index(IXIC)), "%U")
year <- format(as.Date(index(IXIC)), "%Y")

any(is.na(nasdaq.AdjClose))

#Aggregating data on a Weekly basis for each year
aggregate_weekly<-aggregate( IXIC$IXIC.Adjusted ~ week + year , IXIC$IXIC.Adjusted , mean )
View(aggregate_weekly)
nasdaq.AdjClose <- aggregate_weekly$IXIC.Adjusted

ts_nasdaq.AdjClose <- ts(nasdaq.AdjClose, start = c(2012,1), frequency=52)

#Plotting the original Stock Adjusted Close Price
plot(ts_nasdaq.AdjClose,type='l',main='NASDAQ Composite Stock Price Time series')

adf.test(ts_nasdaq.AdjClose) #p-value greater than 0.05-> accept null-> data is Not stationary
CADFtest(ts_nasdaq.AdjClose)

#Computes the Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test for the null hypothesis that x is level or trend stationary
kpss.test(ts_nasdaq.AdjClose, null = c("Level", "Trend"), lshort = TRUE) #p-value<0.05, reject null, Not Stationary

ArchTest(ts_nasdaq.AdjClose) #reject null, Arch effects seen

## Differencing the series
diff.nasdaq=diff(ts_nasdaq.AdjClose)

any(is.na(diff.nasdaq))

#Plot differences of original series
plot(diff.nasdaq,type='l',main='Difference NASDAQ Composite')

adf.test(diff.nasdaq) #p-value less than 0.05-> reject null-> diff data is stationary

#Take log of original series and plot the log price
log.nasdaq=log(ts_nasdaq.AdjClose)
any(is.na(log.nasdaq))

plot(log.nasdaq,type='l',main='Log NASDAQ Composite')

adf.test(log.nasdaq) #p-value greater than 0.05-> accept null-> log data is Not stationary

#Differencing log price and plot
difflog.nasdaq=diff(log.nasdaq)
plot(difflog.nasdaq,type='l',main='Difference Log NASDAQ Composite')
any(is.na(difflog.nasdaq))

adf.test(difflog.nasdaq) #p-value less than 0.05-> reject null-> diff log data is stationary
CADFtest(difflog.nasdaq)
kpss.test(difflog.nasdaq, null = c("Level", "Trend"), lshort = TRUE) #p-value>0.05, accept null, It is Stationary

###
#Histogram
hist(difflog.nasdaq, xlab="Weekly Diff Log of stock prices", prob=TRUE, main="Histogram for Weekly Diff Log of stock prices")
xfit<-seq(min(difflog.nasdaq),max(difflog.nasdaq),length=40)
yfit<-dnorm(xfit,mean=mean(difflog.nasdaq),sd=sd(difflog.nasdaq))
lines(xfit, yfit, col="blue", lwd=2)

#QQ-plot
qqnorm(difflog.nasdaq)
qqline(difflog.nasdaq, col = 2) 

## Model Identification

# ACF and PACF of Log NASDAQ Composite
acf.nasdaq=acf(ts_nasdaq.AdjClose,main='ACF NASDAQ Composite',lag.max=100) #Decreases but does not die, needs differencing
pacf.nasdaq=pacf(ts_nasdaq.AdjClose,main='PACF NASDAQ Composite',lag.max=100,ylim=c(-0.5,1)) #Cuts off after lag 1

# ACF and PACF of Log NASDAQ Composite
acf.nasdaq=acf(log.nasdaq,main='ACF NASDAQ Composite',lag.max=100) #Decreases but does not die, needs differencing
pacf.nasdaq=pacf(log.nasdaq,main='PACF NASDAQ Composite',lag.max=100,ylim=c(-0.5,1)) #Cuts off after lag 1

#ACF and PACF of Differenced log NASDAQ Composite
acf.nasdaq=acf(difflog.nasdaq,main='ACF Difference Log NASDAQ Composite',lag.max=100) # 1
pacf.nasdaq=pacf(difflog.nasdaq,main='PACF Difference Log NASDAQ Composite',lag.max=100,ylim=c(-0.5,1)) #3 or 2 or 1
eacf(difflog.nasdaq) #2,1 and 2,2 and 6,0 and 5,1 and 4,1 and 4,3 and 5,4

acf.nasdaq=acf(difflog.nasdaq^2,main='ACF of Square of Difference Log NASDAQ Composite',lag.max=100) # 1 or 8 or 6
acf.nasdaq=acf(abs(difflog.nasdaq),main='ACF of Absolute of Difference Log NASDAQ Composite',lag.max=100) # 1 or 6 or 7 or 8
#MA observed in ACF of Squared and abs diff log data

pacf.nasdaq=pacf(nasdaq.AdjClose^2,main='PACF Square of NASDAQ Composite',lag.max=100,ylim=c(-0.5,1)) #Cuts off after lag 1
pacf.nasdaq=pacf(abs(nasdaq.AdjClose),main='PACF Abs of NASDAQ Composite',lag.max=100,ylim=c(-0.5,1)) #Cuts off after lag 1


#arima_x = auto.arima(difflog.nasdaq)
#arima_x #1,0,2 zero mean

#arima_n = auto.arima(nasdaq.AdjClose)
#arima_n #orig=0,2,1 #log=1,1,2 #diff=0,1,1

#Ljung Box test on squared values of the stock price returns
#H0: null hypothesis is there exists no autocorrelation
Box.test(difflog.nasdaq^2, type="Ljung") #reject null, there is autocorrelation

basicStats(difflog.nasdaq)

## ARIMA models
arima010=stats::arima(log.nasdaq,order=c(0,1,0),method='ML')
summary(arima010)

arima111=stats::arima(log.nasdaq,order=c(1,1,1),method='ML')
summary(arima111)

arima112=stats::arima(log.nasdaq,order=c(1,1,2),method='ML')
summary(arima112)

arima314=stats::arima(log.nasdaq,order=c(3,1,4),method='ML')
summary(arima314) #best

arima211=stats::arima(log.nasdaq,order=c(2,1,1),method='ML')
summary(arima211)

arima212=stats::arima(log.nasdaq,order=c(2,1,2),method='ML')
summary(arima212)


arima010$aic
arima111$aic
arima112$aic
arima314$aic
arima211$aic
arima212$aic

Box.test(rstandard(arima314), lag=20, type = 'Ljung-Box') #p-value=0.98>0.05, accept null, thus model is just fine, fits well
plot(residuals(arima314),type='h',ylab='Standardized Residuals') 
qqnorm(residuals(arima314)) ;qqline(residuals(arima314), col='red')

res.arima314=arima314$res
squared.res.arima314=res.arima314^2

acf.squared314=acf(squared.res.arima314,main='ACF Squared Residuals',lag.max=100) #1 or 4
pacf.squared314=pacf(squared.res.arima314,main='PACF Squared Residuals',lag.max=100,ylim=c(-0.5,1)) #AR(1) or AR(2)

acf.squared314=acf(abs(res.arima314),main='ACF abs Residuals',lag.max=100) #1 or 6
pacf.squared314=pacf(abs(res.arima314),main='PACF abs Residuals',lag.max=100,ylim=c(-0.5,1)) #AR(1) or AR(2) or AR(3)

ArchTest(res.arima314) #p-value<0.05->reject null-> has ARCH effect and therefore GARCH model can be fitted


###GARCH modelling

#Univariate eGARCH model with sstd distribution GARCH(4,1)

ug_spec41 <- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                        variance.model = list(model = 'eGARCH',garchOrder = c(4,1)),
                        distribution.model = 'sstd')

#Model estimation
ugfit41 = ugarchfit(spec = ug_spec41, data = res.arima314, out.sample = 125)
ugfit41 

#Univariate eGARCH model with sstd distribution GARCH(5,1)

ug_spec51 <- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                        variance.model = list(model = 'eGARCH',garchOrder = c(5,1)),
                        distribution.model = 'sstd')

#Model estimation
ugfit51 = ugarchfit(spec = ug_spec51, data = res.arima314, out.sample = 125)
ugfit51 


#Univariate eGARCH model with sstd distribution GARCH(2,1)

ug_spec21 <- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                      variance.model = list(model = 'eGARCH',garchOrder = c(2,1)),
                      distribution.model = 'sstd')

#Model estimation
ugfit21 = ugarchfit(spec = ug_spec21, data = res.arima314, out.sample = 125)
ugfit21 

#Univariate eGARCH model with sstd distribution GARCH(2,2)

ug_spec22 <- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                        variance.model = list(model = 'eGARCH',garchOrder = c(2,2)),
                        distribution.model = 'sstd')

#Model estimation
ugfit22 = ugarchfit(spec = ug_spec22, data = res.arima314, out.sample = 125)
ugfit22 
infocriteria(ugfit22)

par(mar=c(1,1,1,1))
plot(ugfit22, which='all')

#Univariate eGARCH model with sstd distribution GARCH(2,3)

ug_spec23 <- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                        variance.model = list(model = 'eGARCH',garchOrder = c(2,3)),
                        distribution.model = 'sstd')

#Model estimation
ugfit23 = ugarchfit(spec = ug_spec23, data = res.arima314, out.sample = 125)
ugfit23 

#Univariate gjrGARCH model with sstd distribution GARCH(2,2)

ug_spec21_gjr <- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                        variance.model = list(model = 'gjrGARCH',garchOrder = c(2,1)),
                        distribution.model = 'sstd')

#Model estimation
ugfit21_gjr = ugarchfit(spec = ug_spec21_gjr, data = res.arima314, out.sample = 125)
ugfit21_gjr


#Univariate gjrGARCH model with sstd distribution GARCH(2,3)

ug_spec23_gjr <- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                        variance.model = list(model = 'gjrGARCH',garchOrder = c(2,3)),
                        distribution.model = 'sstd')

#Model estimation
ugfit23_gjr = ugarchfit(spec = ug_spec23_gjr, data = res.arima314, out.sample = 125)
ugfit23_gjr 

###



######--------------------------------
#We choose the best model

infocriteria(ugfit41) #BIC= -5.525844
infocriteria(ugfit51) #BIC= -5.505368
infocriteria(ugfit21) #BIC= -5.537102
infocriteria(ugfit22) #BIC= -5.523385
infocriteria(ugfit23) #BIC= -5.540685

#Best 2
infocriteria(ugfit21_gjr) #BIC= -5.476419
infocriteria(ugfit23_gjr) #BIC= -5.468274


#Choosing the model with the least BIC

ugfit_best = ugfit21_gjr

plot(ugfit_best, which='all')


ugfore_best <- ugarchforecast(ugfit_best, n.ahead = 20, n.roll=100)
ugfore_best


plot(ugfore_best, which='all')

# Estimated standardized returns
stdret <- residuals(ugfit_best, standardize = TRUE)
chart.Histogram(stdret, methods = c("add.normal", "add.density"),
                colorset=c("gray","red","blue"))

chart.QQPlot(stdret)
# Goodness of fit for the mean prediction
e <- residuals(ugfit_best)
mean(e^2)

# Goodness of fit for the variance prediction
d <- e^2 - sigma(ugfit_best)^2
mean(d^2)

# Compute the likelihood
likelihood(ugfit_best)

acf(stdret) #No significant correlations observed
Box.test(stdret, type = "Ljung-Box") #pvalue >0.05, good fit
shapiro.test(coredata(stdret)) #p-value 3.022e-07<0.05, residuals are normal


######Estimate all models
variance.models <- c("sGARCH", "gjrGARCH", "eGARCH")
distribution.models <- c("norm", "std", "sstd")
c <- 1
for (variance.model in variance.models) {
  for (distribution.model in distribution.models) {
    garchspec <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                            variance.model = list(model = variance.model),
                            distribution.model = distribution.model)
    garchfit <- ugarchfit(data = ts_nasdaq.AdjClose, spec = garchspec)
    if (c==1) {
      msigma <- sigma(garchfit)
    } else {
      msigma <- merge(msigma, sigma(garchfit))
    } 
    c <- c + 1
  }
}

plot(msigma)
plot(ts_nasdaq.AdjClose)

avesigma <- xts(rowMeans(msigma), order.by = time(msigma))
plot(avesigma)