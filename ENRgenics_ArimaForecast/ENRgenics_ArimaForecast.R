#install.packages("forecast")
library(forecast)
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
  ff=((tt$mean-subset(data, Year>2012)[,"Sales"])/subset(data, Year>2012)[,"Sales"])*100
  summary(ff)
  return(ff)
 }


Tsanalysis(reformeddata,"CA","TOTAL")
