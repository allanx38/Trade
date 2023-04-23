
# load libs
library(quantmod) #wrapper for ttr functions ...
library(TTR)
library(dplyr)
library(lubridate)

# load ticksymbols
f100 <- read.csv("Data/FTSE100.csv", stringsAsFactors = F)

comp_pair <- function(p1,p2){

  p1 <- paste0("Data/StockData/FTSE100/",f100$Tick[p1],".csv")
  ds1 <- read.csv(p1, stringsAsFactors = F)
  p2 <- paste0("Data/StockData/FTSE100/",f100$Tick[p2],".csv")
  ds2 <- read.csv(p2, stringsAsFactors = F)
  
  pr <- inner_join(ds1 %>% select(Tick,Date,Close) %>% rename(Tick1 = Tick, Close1 = Close),
                   ds2 %>% select(Tick,Date,Close) %>% rename(Tick2 = Tick, Close2 = Close),
                   by = "Date")
  pr$Diff <- round(pr$Close1 - pr$Close2)
  pr$ma50 <- round(TTR::runMean(pr$Diff,n=100))
  pr$maDiff <- round(pr$Diff - pr$ma50)
  pr$prev_maDiff <- lag(pr$maDiff)
  pr$sig <- 0
  pr$sig <- ifelse(pr$maDiff > 0 & pr$prev_maDiff < 0, 1, pr$sig)
  pr$sig <- ifelse(pr$maDiff < 0 & pr$prev_maDiff > 0, 2, pr$sig)

    return(pr)
}

pr <- comp_pair(13,16)

pr %>% filter(Date > "2020-01-01", sig != 0) %>% count() 
pr %>% filter(Date > "2018-02-01") %>% View()

debugonce(pair_xover_data_set)
res <- pair_xover_data_set()
res$Date <- as.Date(res$Date)
res$nxtDate <- lead(res$Date)
res$nxtDiff <- lead(res$Diff)
res$nxtCode <- lead(res$Code)
res$pl <- ifelse( res$Code==res$nxtCode, ifelse(res$sig==1,res$nxtDiff - res$Diff, res$Diff - res$nxtDiff),0)

res <- add_weekdays(res)

-401 - -388

res %>% filter(sig==1) %>% group_by() %>% summarise(pl = sum(pl))
res %>% filter(sig==1) %>% filter(pl>0) %>% View()
res %>% filter(sig==1) %>% filter(pl<0) %>% View()
res %>% filter(sig==2) %>% group_by() %>% summarise(pl = sum(pl,na.rm = T))
res %>% filter(sig==2) %>% filter(pl>0) %>% count()
res %>% filter(sig==2) %>% filter(pl<0) %>% count()

res <- test(res)

add_weekdays <- function(res){
  res$Wd <- 0
  for(i in 1:nrow(res)){
    Date1 <- res[i,"Date"]
    Date2 <- res[i,"nxtDate"]
    if(is.na(Date2)){break}
    diff <- Date1 - Date2
    #print(paste0(i, " - ", Date1," - ", Date2, " - ",diff))
    if(diff < 500){
      res$Wd[i] <- sum(!weekdays(seq(Date1, Date2, "days")) %in% c("Saturday", "Sunday"))    
    }
  }
  return(res)
}

unique(res$Code)

debugonce(pair_xover_freq)  
res <- pair_xover_freq("2020-07-08")
res_bk

debugonce(run_pair_xover_freq)
res <- run_pair_xover_freq("2020-09-22")
rr <- res %>% filter(Sig != 0,Freq < 10)
View(rr)

