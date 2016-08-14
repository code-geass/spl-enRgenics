install.packages("forecast")
install.packages("dplyr")
install.packages("tableplot")
install.packages("gridExtra")
library(forecast)
library(dplyr)
library(tableplot)
library(gridExtra)
# Sample Usage: Below is the sample usage of how to source this file into 
# other quantlets.  Note, I have used relative paths, but you may need to use 
# absolute paths OR set the correct working directory.  These variables 
# assume the quantlet is being sourced from a parallel folder

# load data from Quantlet
ENRGENICS_ADDOTHER_PATH = "C:/Users/Reethesh/Desktop/spl-enRgenics/ENRgenics_AddOther/ENRgenics_AddOther.r"
source(ENRGENICS_ADDOTHER_PATH)

# start here to directly load data from this file for testing
# paths for ImportEIA and climate Data
ENRGENICS_IMPORTEIA_PATH = "C:/Users/Reethesh/Desktop/spl-enRgenics/ENRgenics_ImportEIA/ENRgenics_ImportEIA.r"
ENRGENICS_CLIMATE_DATA_PATH = "C:/Users/Reethesh/Desktop/spl-enRgenics/data/climdiv-tmpcst-v1.0.0-20160605"
PATH_VARS = c(ENRGENICS_IMPORTEIA_PATH, ENRGENICS_CLIMATE_DATA_PATH)
#location of EIA data file
EIA_DATA_PATH = "C:/Users/Reethesh/Desktop/spl-enRgenics/data/sales_revenue.csv.0"
file = EIA_DATA_PATH
df = load_eia_data_with_all_others(file)
head(df)y


Tsanalysis= function(Maindata, St , Category)
{
  data=subset(Maindata, State== St & Cat== Category)
  data2=subset(data,Year <= 2012)
  ts1_monthly= ts(data[,"Sales"],start=c(1990,1),frequency=12)
  ts2_monthly= ts(data2[,"Sales"],start=c(1990,1),frequency=12)
  autoar_monthly=auto.arima(ts2_monthly)
  autoar_monthly1=auto.arima(ts1_monthly)
  a=12
  b=0
  for(i in 1:6 ){
  b[i]=tt_monthly(autoar = autoar_monthly, freq=a,g=ts1_monthly)
  a=a+6
  }
   c=matrix(b,nrow=6, ncol=1)
   rownames(c)=c("Periods=12","Periods=18","Periods=24","Periods=30","Periods=36","Periods=42")
   colnames(c)="RMSE"
   mini=which.min(c)
   grid.table(c)
   plot(forecast(autoar_monthly1,h=6+(6*mini)),ylab="Forecasted Sales(MWh)", xlab="Time(Monthly)")
}
tt_monthly=function(autoar,freq,g)
{
  a=forecast.Arima(autoar,h=freq)
  b=accuracy(a,x=g)
 return(b["Test set","RMSE"])
}


Tsanalysis(df,"WI","TOTAL")

