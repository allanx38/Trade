library(TTR)
library(astr)
library(dplyr)
library(lubridate)
library(quantmod)
library(clipr)

get_yahoo_fin_tick <- function(sym,nm,daysback){
  to_dt = today()
  fr_dt = to_dt - daysback
  x <- quantmod::getSymbols(sym, from = fr_dt, to = to_dt, src="yahoo", env=NULL) 
  x <- x[,1:4]
  colnames(x) <- c('Open','High','Low','Close')
  x <- na.omit(x)
  x <- data.frame(Date=index(x), coredata(x)[,], stringsAsFactors = F)
  x$Tick <- nm
  x <- x %>% select(Tick,everything())
}  

gen_pair <- function(p1,p1_nm,p2,p2_nm,daysback=100){
  dax <- get_yahoo_fin_tick(p1,p1_nm,daysback)
  cac <- get_yahoo_fin_tick(p2,p2_nm,daysback)
  pair <- inner_join(dax %>% select(Date,Close) %>% rename(P1 = Close),
                     cac %>% select(Date,Close) %>% rename(P2 = Close),
                     by = "Date")
  pair$diff <- round(pair$P1 - pair$P2,2)
  pair$diffma10 <- round(runMean(pair$diff, n = 10),2)
  pair$diffma50 <- round(runMean(pair$diff, n = 50),2)
  pair$LS10 <- pair$diff - pair$diffma10
  pair$LS50 <- pair$diff - pair$diffma50
  return(pair)
}


dax <- get_yahoo_fin_tick("^GDAXI","DAX",50)
cac <- get_yahoo_fin_tick("^FCHI","CAC",50)

to_dt = today()
fr_dt = to_dt - 50
quantmod::getSymbols("^GDAXI", from = fr_dt, to = to_dt, src="yahoo", env=NULL)

# Dax/Cac
  res <- gen_pair("^GDAXI","DAX","^FCHI","CAC",daysback=200)
  write.csv(tail(res,n=40), "~/Documents/res.csv")
  tail(res,n=40) %>% clipr::write_clip()

# Visa Mastercard  
  vm <- gen_pair("MA","MA","V","V",2000)
  write.csv(vm, "~/Documents/vm.csv")
  tail(vm,n=20) %>% clipr::write_clip()
  
  
# AT&T Inc. T  & Verizon Communications Inc. VZ 
  av <- gen_pair("VZ","VZ","T","T",2000)
  write.csv(av, "~/Documents/av.csv")
  tail(cb,n=20) %>% clipr::write_clip()

    
# Colgate / Bed Bath & Beyond
    cb <- gen_pair("CL","CL","BBBY","BBBY",2000)
    write.csv(cb, "~/Documents/cb.csv")
    tail(cb,n=20) %>% clipr::write_clip()
    
    
# Zillow Twitter 
    zt <- gen_pair("Z","Z","TWTR","TWTR",2000)
    write.csv(zt, "~/Documents/zt.csv")
    tail(vm,n=20) %>% clipr::write_clip()
  
  
  # res2 <- gen_pair("IBM","IBM","ORCL","ORCL")
  # res3 <- gen_pair("MSFT","MSFT","IBM","IBM")
  # res_r <- gen_pair("MA","MCard","V","Visa")
  # res4 <- gen_pair("AZN.L","AZN","DCC.L","DCC")
    
    

# New Version -------------------------------------------------------------

library(TTR)
library(astr)
library(dplyr)
library(lubridate)
library(quantmod)
library(clipr)

get_yahoo_fin_tick <- function(sym,daysback){
  to_dt = today()
  fr_dt = to_dt - daysback
  x <- quantmod::getSymbols(sym, from = fr_dt, to = to_dt, src="yahoo", env=NULL) 
  x <- x[,1:4]
  colnames(x) <- c('Open','High','Low','Close')
  x <- na.omit(x)
  x <- data.frame(Date=index(x), coredata(x)[,], stringsAsFactors = F)
  x$Tick <- sym
  x <- x %>% select(Tick,everything())
}  

gen_pair <- function(p1,p2,daysback=100){
  dax <- get_yahoo_fin_tick(p1,daysback)
  cac <- get_yahoo_fin_tick(p2,daysback)
  pair <- inner_join(dax %>% select(Date,Close) %>% rename(P1 = Close),
                     cac %>% select(Date,Close) %>% rename(P2 = Close),
                     by = "Date")
  pair$diff <- round(pair$P1 - pair$P2,2)
  pair$diffma10 <- round(runMean(pair$diff, n = 10),2)
  pair$diffma25 <- round(runMean(pair$diff, n = 25),2)
  pair$diffma50 <- round(runMean(pair$diff, n = 50),2)
  pair$LS10 <- pair$diff - pair$diffma10
  pair$LS25 <- pair$diff - pair$diffma25
  pair$LS50 <- pair$diff - pair$diffma50
  return(pair)
}
    
  # Dax/Cac
  cd_up <- gen_pair("^GDAXI","^FCHI",daysback=1000)
  saveRDS(cd, "~/Documents/dac_cac_pair.rds")
  
  # Update
    cd_update <- gen_pair("^GDAXI","^FCHI",daysback=200)
    write.csv(cd_update, "~/Documents/dac_cac_pair_update.csv")
  # read previously saved data
    cd <- readRDS("~/Documents/dac_cac_pair.rds")
    cd <- rbind(cd, cd_update %>% filter(!is.na(LS50))) %>% distinct()
    saveRDS(cd, "~/Documents/dac_cac_pair.rds")
  
  # use data
    #cd <- readRDS("~/Documents/dac_cac_pair.rds")
  # Add Days data
    cd$DayMove <-cd$diff - lag(cd$diff)
    cd_up$DayMove <-cd_up$diff - lag(cd_up$diff)

    cd[580:605,]    %>% View()
    cd[680:695,]
    
    to_dt = today()
    fr_dt = to_dt - 100
    x <- quantmod::getSymbols("^GDAXI", from = fr_dt, to = to_dt, src="yahoo", env=NULL) 
    x <- x[,1:4]
    colnames(x) <- c('Open','High','Low','Close')
    x <- na.omit(x)
    x <- data.frame(Date=index(x), coredata(x)[,], stringsAsFactors = F)
    x$Tick <- sym
    x <- x %>% select(Tick,everything())
    
