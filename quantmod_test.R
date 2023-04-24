
library(quantmod)
library(dplyr)
library(lubridate)


# git remote add origin https://github.com/allanx38/Trade.git
# git branch -M main
# git push -u origin main

# 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

# new one
# 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

# Get Data

# simple
  getSymbols("AAPL")

# Add Dates - return to 'env' ...
  to_dt = today()
  from_dt = to_dt - 1200
  x <- getSymbols("AAL.L", src="yahoo",  from = from_dt, to = to_dt, env=NULL, return.class = 'data.frame')

# last ..  
  AAPL <- last(AAPL,'1 years')
  AAPL <- first(AAPL,'3 years')
  
# To extract columns, we use Op, Hi, Lo, Cl, Vo and Ad
  # xts objects
  Open <- Op(AAPL)   #Open Price
  High <- Hi(AAPL)    # High price
  Low <- Lo(AAPL)  # Low price
  Close<- Cl(AAPL)   #Close Price
  Volume <- Vo(AAPL)   #Volume
  AdjClose <- Ad(AAPL) # Adjusted close
  
# Volume
 
  

# Plotting    
class(x)
chartSeries(x) %>% addMACD()

# get data
  getSymbols("AAPL")
  
  chartSeries(AAPL,
              type="line",
              subset='2007',
              theme=chartTheme('white'))
  lineChart(AAPL)
  
  chartSeries(AAPL,
              type="bar",
              subset='2007-05::2007-06',
              theme=chartTheme('white'))
  barChart(AAPL,theme=chartTheme('white'))
  
  chartSeries(AAPL,
              type="candlesticks",
              subset='2007-05',
              theme=chartTheme('white'))
  # ??
  candleChart(AAPL)


barChart(to.monthly(AAL.L),up.col='white',dn.col='blue') 
chartSeries(to.weekly(AAL.L),up.col='white',dn.col='blue')
candleChart(AAL.L,multi.col=TRUE,theme="white")

# Technical Analysis - TTR package
  # use CL function to get close prices
# SMA
  sma <- SMA(Cl(AAPL),n=20)
  tail(sma)

  
  
# Charting with Indicators  
  chartSeries(AAPL,
              subset='2007-05::2009-01',
              theme=chartTheme('white'))
  addSMA(n=30,on=1,col = "blue")
  addSMA(n=200,on=1,col = "red")
  
  
  chartSeries(AAPL,
              subset='2007-05::2009-01',
              theme=chartTheme('white'))
  addMACD(fast=12,slow=26,signal=9,type="EMA")
  
  macd <- MACD(Cl(AAPL))
  tail(macd)
  
  chartSeries(AAPL,
              subset='2007-05::2009-01',
              theme=chartTheme('white'))
  addWPR()
  
    
chartSeries(to.weekly(AAL.L))
addMACD()
addBBands()

# FTSE
  to_dt = today()
  from_dt = to_dt - 1200
  x <- getSymbols("AAL.L", src="yahoo",  from = from_dt, to = to_dt, env=NULL)

  chartSeries(x,
              subset='2018::',
              theme=chartTheme('white'))
  addMACD()
  addRSI()
  addSMA(n=50)

  macd <- MACD(Cl(x))
  x <- cbind(x,macd)
  
# 


# Doing quantitative research implies a lot of data crunching and one needs clean and reliable data to achieve this. 
# What is really needed is clean data that is easily accessible (even without an internet connection). 
# The most efficient way to do this for me has been to maintain a set of csv files. 
# Obviously this process can be handled in many ways but I found very efficient and simple overtime to maintain a directory 
# where I store and update csv files. I have one csv file per instrument and each file is named after the instrument
# it contains. The reason I do so is twofold: First, I don’t want to download (price) data from Yahoo, Google etc… 
# every time I want to test a new idea but more importantly once I identified and fixed a problem, I don’t want to have to do 
# it again the next time I need the same instrument. Simple yet very efficient so far. The process is summarized in the chart below.



# In everything that follows, I assume that data is coming from Yahoo. 
# The code will have to be amended for data from Google, Quandl etc… In addition I present the process of updating 
# daily price data. The setup will be different for higher frequency data and other type of dataset (i.e. different from prices).

# 1 – Initial data downloading (listOfInstruments.R & historicalData.R)
# The file listOfInstruments.R is a file containing only the list of all instruments.


##########################################
## List of securities (Yahoo tickers)
## thertrader@gmail.com - Nov. 2015
##########################################
theInstruments = c("^GSPC",
                   "SPY",
                   "QQQ",
                   "DDM",
                   "EFA",
                   "EEM",
                   "EWJ")


##########################################
## Daily prices from Yahoo
## thertrader@gmail.com - Nov. 2015
##########################################
library(quantmod)

startDate = "2000-01-01"
thePath = "D:\\daily\\data\\"
source(paste(thePath,"code\\listOfInstruments.r",sep=""))

for (ii in theInstruments){
  print(ii)
  data = getSymbols(Symbols = ii,
                    src = "yahoo",
                    from = startDate,
                    auto.assign = FALSE)
  colnames(data) = c("open","high","low","close","volume","adj.")
  write.zoo(data,paste(thePath,ii,".csv",sep=""),sep=",",row.names=FALSE)
}

# 2 – Update existing data (updateData.R)

##########################################
## Update data files
## thertrader@gmail.com - Nov. 2015
##########################################
library(quantmod)

lookback = 60
startDate = Sys.Date() - lookback
thePath = "D:\\daily\\data\\"
theFiles = list.files(path=thePath,pattern=".csv")

for (ii in theFiles){
  data = read.csv(paste(thePath,ii,sep=""))
  data = xts(data[,c("open","high","low","close","volume","adj.")],
             order.by = as.Date(data[,"Index"],format="%Y-%m-%d"))
  lastHistoricalDate = index(data[nrow(data),])
  
  recent = getSymbols(Symbols = substr(ii,1,nchar(ii)-4),
                      src = "yahoo",
                      from = startDate,
                      auto.assign = FALSE)
  colnames(recent) = c("open","high","low","close","volume","adj.")
  
  pos = match(as.Date(lastHistoricalDate,format="%Y-%m-%d"),index(recent))
  
  if (!is.na(pos)){
    if (pos == nrow(recent))
      print("File already up-to-date")
    
    if (pos < nrow(recent)){
      dt = NULL
      dt = rbind(data,recent[(pos+1):nrow(recent),])
      write.zoo(dt,paste(thePath,ii,sep=""),sep=",",row.names=FALSE)
    }
  }
  
  if (is.na(pos))
    print("Error: dates do not match")