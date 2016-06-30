
Tsanalysis= function(Maindata, St , Category)
{
  data=subset(Maindata, State== St & Cat== Category)
  data2=subset(data,Year <= 2012)
  ts1= ts(data[,"Sales"],start=c(1990,1),frequency=12)
  ts2= ts(data2[,"Sales"],start=c(1990,1),frequency=12)
  sfunc1= ma(ts1, order=12) # 12 month moving average
  sfunc2= ma(ts2,order=12)
  plot.ts(sfunc1)
  plot.ts(sfunc2)
  autoar=auto.arima(ts2)
  tt=forecast(autoar,h=38)
  par(mfrow=c(2,1))
  plot.ts(ts1)
  lines(sfunc1, col="red")
  plot(forecast(autoar,h=38))
  lines(sfunc2, col="red")
  print(tt)
  print(subset(data, Year>2012)[,c("Year","Sales")])
  print(accuracy(tt,subset(data, Year>2012)[,"Sales"]))
  return(tt)
  }


Tsanalysis(g)
#
tsm2=ts(b[,3],start=c(1990,1),frequency= 12)
tsq1= aggregate(tsm1,nfrequency = 4, FUN=mean)
dtsq1=decompose(tsq1)
dtsm1=decompose(tsm1)
dtsm2=decompose(tsm2)
plot.ts(dtsm1$seasonal)
plot.ts(dtsm1$random)
plot.ts(dtsm1$trend)
plot.ts(dtsm1$x)
plot.ts(dtsq1$seasonal)
plot.ts(dtsq1$random)
plot.ts(dtsq1$trend)
plot.ts(dtsq1$x)#

auto.arima(tsm1)
auto.arima(tsq1)
auto.arima(tsm2)
fitmonth= arima(tsm1,order=c(2,0,2))
armonth= forecast.Arima(fitmonth, h=24)

fitquarter= arima(tsq1,order=c(2,0,0))
arquarter=forecast.Arima(fitquarter,h=12)
plot.forecast(arquarter)
fitmonth2=arima(tsm2,order=c(2,0,2))
armonth2=forecast.Arima(fitmonth2,h= 84)
par(mfrow=c(2,1))
plot.forecast(armonth)
plot.forecast(armonth2)



logts=log(timeseries1)
plot.ts(logts)
install.packages("TTR")
library("TTR")
install.packages("forecast")
library("forecast")

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
auto.arima(timeseries1)
ts1arima= arima(logts,order=c(3,1,2))
ts1arimafc= forecast.Arima(ts1arima, h=48)
plot.forecast(ts1arimafc)