
# load libs
  library(quantmod) #wrapper for ttr functions ...
  library(TTR)
  library(dplyr)
  library(lubridate)
  library(astr)
  
# source
  setwd("~/Documents/R Files/Trade")
  source("knn_trade_fnc.R")
  source("stock_fnc.R")
  

# Generate Data -----------------------------------------------------------

  # FTSE
    to_dt = today()
    fr_dt = to_dt - dysBack
    #x <- quantmod::getSymbols(DataSet$Tick[i], src="yahoo", env=NULL)
    try( x <- getSymbols("^FTSE", env=NULL) )
    x <- x[,1:4]
    colnames(x) <- c('Open','High','Low','Close')
    x <- na.omit(x)
    x <- data.frame(Date=index(x), coredata(x)[,], stringsAsFactors = F)
    x$Tick <- "FTSE"
    x <- x %>% select(Tick,everything())
    write.csv(x, "../Trade/Data/StockData/FTSE.csv", row.names = F)
  
  # load ticksymbols
    f100 <- read.csv("../Trade/Data/FTSE100.csv", stringsAsFactors = F)
    
  # get all ftse data
  # only need to run once
    debugonce(get_data)
    get_data(f100,"FTSE100")
    
  # append  
     to_dt = today()
     fr_dt = to_dt - 2
     debugonce(append_data)
     append_data(f100,"FTSE100",fr_dt,to_dt)
     
    #getSymbols(f100$Tick[5], from = fr_dt, to = to_dt, src="yahoo")
    #TTR::stockSymbols()
    #ge <- getYahooData(f100$Tick[5], 20200901, 20200914, adjust = FALSE)


# Use csv Files -----------------------------------------------------------
  
  # loop thru set of tick files
  # gen last MA of knn predictions
    debugonce(run_ma_disk)
    res <- run_ma_disk(f100)
  
    source("knn_trade_fnc.R")
  
  # Gen up to date predictions for interesting stocks, as found above
    debugonce(get_pred_data_set)
    get_pred_data_set("MKS.L") %>% View() # L
    get_pred_data_set("TSCO.L")  # L
    get_pred_data_set("HL.L")
    get_pred_data_set("BRBY.L") # L
    
    debugonce(get_data_set)
    get_data_set("MKS.L") %>% View()
    get_data_set("TSCO.L") %>% View()
    get_data_set("HL.L") %>% View()
  

# TA Section ... ----------------------------------------------------------

    # favourite at mo ...
    # ADX and SAR
    fn <- paste0("Data/StockData/FTSE100/",f100$Tick[6],".csv")
    fn <- paste0("Data/StockData/FTSE100/RMV.L.csv")
    ds <- read.csv(fn, stringsAsFactors = F) 
    
    tr <- ATR(ds[,c(4,5,6)], n=14)
    ds <- cbind(ds,tr)
    ds$atr <- round(ds$atr)
    
    dmi.adx <- ADX(ds[,c("High","Low","Close")])
    ds <- cbind(ds,dmi.adx)
    ds$dx_dir <- ifelse(ds$DIp>ds$DIn,"L","S")
    
    sar <- SAR(ds[,c("High","Low")])
    ds <- cbind(ds,sar)
    ds$sar_dir <- ifelse(ds$Close > ds$sar,"L","S")
    ds$tr_dir <- NA
    ds$tr_dir <- ifelse(ds$dx_dir=="L" & ds$sar_dir=="L","L",ds$tr_dir)
    ds$tr_dir <- ifelse(ds$dx_dir=="S" & ds$sar_dir=="S","S",ds$tr_dir)
    
    ds <- ds %>% select(Tick,Date,Open,High,Low,Close,atr,tr_dir,dx_dir,sar_dir,everything())
    
    ds$prev_tr_dir <- lag(ds$tr_dir)
    
    ds$PullBack <- NA
    ds$PullBack <- ifelse(ds$tr_dir=="L",ifelse(ds$Low<ds$Open-(ds$atr/2),"L",ds$PullBack),ds$PullBack)
    ds$PullBack <- ifelse(ds$tr_dir=="S",ifelse(ds$High>ds$Open+(ds$atr/2),"S",ds$PullBack),ds$PullBack)
    ds <- ds %>% select(Tick,Date,Open,High,Low,Close,PullBack,atr,tr_dir,prev_tr_dir,dx_dir,sar_dir,everything())
    
    ds$trStart <- NA
    ds$trStart <- ifelse(ds$tr_dir == "L" & ds$prev_tr_dir != "L", "L", ds$trStart)
    ds$trStart <- ifelse(ds$tr_dir == "L" & is.na(ds$prev_tr_dir), "L", ds$trStart)
    ds$trStart <- ifelse(ds$tr_dir == "S" & ds$prev_tr_dir != "S", "S", ds$trStart)
    ds$trStart <- ifelse(ds$tr_dir == "S" & is.na(ds$prev_tr_dir), "S", ds$trStart)
    
    
    ds$PrevtrStart <- lag(ds$trStart)
    ds <- ds %>% select(Tick,Date,Open,High,Low,Close,PullBack,atr,tr_dir,prev_tr_dir,trStart,PrevtrStart,dx_dir,sar_dir,everything())
    
    
    ds %>% filter(!is.na(tr_dir)) %>% count()
    ds %>% filter(!is.na(tr_dir),!is.na(PullBack)) %>% count()
    
    unique(f100$Tick)
    
    source("knn_trade_fnc.R")
    all_ds <- gen_adx_sar(f100)
    
    all_res <- run_count_whales(all_ds,f100)
    all_res$pl <- all_res$Close - all_res$Open
    all_res$pl_atr <- all_res$pl / all_res$atr
    all_res$hpl <- all_res$High - all_res$Open
    all_res$hpl_atr <- all_res$hpl / all_res$atr
    
    all_res$adjpl <- all_res$pl
    all_res$adjpl <- ifelse(all_res$pl < 0, all_res$hpl - all_res$atr, all_res$pl)
    all_res$adjpl <- ifelse(all_res$adjpl < all_res$pl, all_res$pl, all_res$adjpl)
    
    all_res$OpenDate <- as.Date(all_res$OpenDate)
    
    rr <- all_res %>% filter(OpenDate > "2020-01-01") %>% group_by(OpenDate) %>% 
      summarise(n(), pl = sum(adjpl))
    
    sum(rr$pl)  
    mean(rr$pl)
    
    all_res %>% filter(OpenDate == "2020-08-07")
    
    dt1 <- "2020-01-01"
    dt2 <- "2020-08-01"
    
    Jul <- all_res %>% filter(OpenDate > dt1, OpenDate < dt2) %>% 
      select(Tick,OpenDate,atr,Run,pl,adjpl) %>% 
      mutate(atrmult = adjpl / atr)
    
    Jul %>% filter(atrmult > 2) %>% count()
    Jul %>% filter(atrmult > 3) %>% count()
    Jul %>% filter(atrmult > 4) %>% count()
    
      Jul %>% arrange(OpenDate) %>% mutate(cumSum = cumsum(Jul$adjpl)) %>% View()
    
    Jul %>% filter(adjpl > 0) %>% count()
    
    sum(Jul$adjpl)
    Jul$runPL <- TTR::runSum(Jul$adjpl)
    
    
    #debugonce(count_lng_whales)
    res <- count_lng_whales(ds)
    res$pl <- res$Close - res$Open
    res$pl_atr <- res$pl / res$atr
    res$hpl <- res$High - res$Open
    res$hpl_atr <- res$hpl / res$atr
    
    res %>% filter(hpl_atr < 2) %>% count()
    res %>% filter(hpl_atr < 0.5) %>% count()
    res %>% filter(hpl_atr >= 0.5, hpl_atr <= 1) %>% count()
    res %>% filter(hpl_atr > 1) %>% count()
    res %>% filter(hpl_atr > 3) %>% count()
    res %>% filter(hpl_atr > 4) %>% count()
        
    sum(res$pl) / 155
    res %>% filter(pl > 0) %>% count()
    
    w <- res %>% filter(pl > 0)
    n <- res %>% filter(pl < 0, hpl > 20)
    l <- res %>% filter(pl < 0, hpl < 20)
    sum(w$pl) + sum(l$pl)
    
    
    
     # look for whales
    count_lng_whales <- function(ds){
      all_res <- NULL
      for(i in 1:nrow(ds)){
        if(!is.na(ds$PrevtrStart[i])){
          if(ds$PrevtrStart[i] == "L"){
            open_pr <- ds$Open[i]
            open_date <- ds$Date[i]
            high <- ds$High[i]
            low <- ds$Low[i]
            close <- ds$Close[i]
            atr <- ds$atr[i]
            res <- lng_whale(ds[i:nrow(ds),], open_pr, open_date, high, low, close,atr)
            all_res <- rbind(all_res,res)
          }
        }
      }
      return(all_res)
    }
    
    
    lng_whale <- function(ds, open_pr, open_date, high, low, close,p_atr){
      #browser()
      run <- 1
      high_num <- 0
      high_whale <- high
      low_num <- 0
      low_whale <- low
      close_pr <- close
      
      for(i in 1:nrow(ds)){
        
        if(is.na(ds$tr_dir[i])){
          break
        }
        if(ds$tr_dir[i] == "S"){
          break
        }
        
        if(ds$High[i] > high_whale){
          high_whale <- ds$High[i]
          high_num <- i
        }
        if(ds$Low[i] < low_whale){
          low_whale <- ds$Low[i]
          low_num <- i
        }
        close_pr <- ds$Close[i]
        run <- run + 1
      }
      res <- data.frame(Tick = ds$Tick[i],OpenDate = open_date, CloseDate = ds$Date[i], Run = run,Open = open_pr, High = high_whale, Low = low_whale, Close = close_pr, atr = p_atr)
    }
    
    
    debugonce(find_final_run)
    rr <- find_final_run(ds)
    rr
    
    # Sept 14
      # Short
      # SSE atr 28, 10 up then down... prev close 1216
      # LAND.L atr 18, 8 or 9 up then down, prev close 529
      # SDR.L atr 76, 30 up then down prevclose 2784.00
    
      # Long
        # HIK.L atr 78, 35 dn then long, prev close 2673
        # JMAT.L atr 74, 35 dn then long, prev close 2605 - 2570
        # CCL.L atr 70, 35 dn then long, prev close 1111 - 1080
    
    
    ## trading ....
    # 1. atr prof targ ...
    
    ds$prev_tr_dir
    
    debugonce(trade_atr)
    rr <- trade_atr(ds)
    sum(rr$PL)
    rr %>% filter(PL>0) %>% count()
    rr %>% filter(PL<0) %>% count()
    
    
    trade_atr <- function(pdata) {
      all_res <- NULL
      for(i in 75:nrow(pdata)){
        if(!is.na(pdata$prev_tr_dir[i])){
          if(pdata$prev_tr_dir[i]=="L"){
            #browser()
            open_pr <- pdata$Open[i]
            open_date <- pdata$Date[i]
            open_atr <- pdata$atr[i]
            sloss_lvl <- open_pr - (open_atr * 2)
            ptarg_lvl <- open_pr + (open_atr * 2)
            res <- single_trade(pdata[i:nrow(pdata),],"L",open_date,sloss_lvl,ptarg_lvl,open_atr)
            all_res <- rbind(all_res,res)
          }
        }
      }
      return(all_res)
    } # end of fnc
    
    single_trade <- function(pdata, Dir, open_date, sloss_lvl, ptarg_lvl, atr) {
      pt <- 0
      run = 1
      res <- NULL
      for(i in 1:nrow(pdata)){
        if(Dir == "L"){
          if(pdata$Low[i] < sloss_lvl){
            pt <- -(atr * 2)
            res <- data.frame(Tick = pdata$Tick[i],OpenDate = open_date, CloseDate = pdata$Date[i], PL = pt, Run = run)
            break
          }
          if(pdata$High[i] > ptarg_lvl){
            pt <- (atr * 2)
            res <- data.frame(Tick = pdata$Tick[i],OpenDate = open_date, CloseDate = pdata$Date[i], PL = pt, Run = run)
            break
          }
          run = run + 1
        }
      } #end for
      #browser()
      return(res)
    }
    
    
    
    
    source("knn_trade_fnc.R")
    #debugonce(sar_adx)
    res <- sar_adx(f100)
    # general dir
      table(res$tr_dir)
    # Long
      res_lng <- res %>% filter(tr_dir=="L") %>% arrange(desc(run)) 
      View(res_lng)
      res_lng %>% filter(trStart == "L") %>% select(Tick,Close)
   # Short
      res_sht <- res %>% filter(tr_dir=="S") %>% arrange(desc(run)) 
      View(res_sht)
      res_sht %>% filter(trStart == "S") %>% select(Tick,Close)
      
    
    
    # ADX - seems pretty good ...
    fn <- paste0("Data/StockData/FTSE100/",f100$Tick[20],".csv")
    ds <- read.csv(fn, stringsAsFactors = F)  
  
    dmi.adx <- ADX(ds[,c("High","Low","Close")])
    ds <- cbind(ds,dmi.adx)
    ds$dx_dir <- ifelse(ds$DIp>ds$DIn,"L","S")
    
    # Directional Movement Index (+DI and -DI)
    # The +DI is the percentage of the true range that is up. 
    # The -DI is the percentage of the true range that is down. 
    # A buy signal is generated when the +DI crosses up over the -DI. 
    # A sell signal is generated when the -DI crosses up over the +DI. 
    # You should wait to enter a trade until the extreme point is reached. 
    # That is, you should wait to enter a long trade until the price reaches the high of the bar 
    # on which the +DI crossed over the -DI, and wait to enter a short trade until the price 
    # reaches the low of the bar on which the -DI crossed over the +DI.
    
    # Directional Movement Index (DX) - essentially diff bet +ve and -ve ...
    # The DX is usually smoothed with a moving average (i.e. the ADX). The values range from 0 to 100, 
    # but rarely get above 60. To interpret the DX, consider a high number to be a strong trend, and a 
    # low number, a weak trend.
    
    
    # SAR Parabolic Stop-and-Reverse - good
    #fn <- paste0("Data/StockData/FTSE100/",f100$Tick[10],".csv")
    #ds <- read.csv(fn, stringsAsFactors = F) 
    sar <- SAR(ds[,c("High","Low")])
    ds <- cbind(ds,sar)
    ds$sar_dir <- ifelse(ds$Close > ds$sar,"L","S")
    
    
    
    
    
   # Commodity Channel Index (CCI) - mmm not sure ...
    fn <- paste0("Data/StockData/FTSE100/",f100$Tick[2],".csv")
    ds <- read.csv(fn, stringsAsFactors = F) 
    cci <- CCI(ds[,c("High","Low","Close")])
    ds <- cbind(ds,cci)
    
    # The CCI is designed to detect beginning and ending market trends. 
    # The range of 100 to -100 is the normal trading range. CCI values outside of this range 
    # indicate overbought or oversold conditions. You can also look for price divergence in the CCI. 
    # If the price is making new highs, and the CCI is not, then a price correction is likely.
    
    # A basic CCI trading system is:  
    # Buy (sell) if CCI rises above 100 (falls below -100) and sell (buy) when it falls below 100 
    # (rises above -100)
    
    
    # CLV - Close Location Value --- mmm
    fn <- paste0("Data/StockData/FTSE100/",f100$Tick[2],".csv")
    ds <- read.csv(fn, stringsAsFactors = F) 
    clv <- CLV(ds[,c("High","Low","Close")])
    ds <- cbind(ds,clv)
    
    
    
    # Chande Momentum Oscillator (CMO)
    fn <- paste0("Data/StockData/FTSE100/",f100$Tick[2],".csv")
    ds <- read.csv(fn, stringsAsFactors = F) 
    cmo <- CMO(ds[,"Close"], n = 14)
    ds <- cbind(ds,cmo)

    # 1.  Values over/under +/- 50 indicate overbought/oversold conditions.
    # 2.  High CMO values indicate strong trends.
    # 3.  When the CMO crosses above/below a moving average of the CMO, it is a buy/sell signal.
    
    
    
    
    
  
    
    
    
    
    
    
    
    
     
        
  # run trade - single tick
    run_ma_ta_ind(20)
 # run trade all
  # debugonce(run_ma_ta)
  res <- run_ma_ta()
  res %>% filter(Score >= 0.5) %>% count()
  res %>% filter(Score < 0.5) %>% count()
  mean(res$Score)
  
  res_na <-  run_ma_ta_na()
  
  res_na %>% group_by(Score) %>% summarise(mn = mean(Score_na))
  
  
  
      z <- scale(test_knn)
  
  # 
    calc_class_err(actual = y_test_knn,
                        predicted = class::knn(train = scale(train_knn), 
                                               test  = scale(test_knn),
                                               cl    = y_train_knn,
                                               k     = 3))
  
  
  actual = y_test_knn
  
  bind_cols( as.data.frame(actual),as.data.frame(predicted) )
  
  


                    
 
  res2
  

  
  
#####
  to_dt = today()
  fr_dt = to_dt - 600
  
  tick_data <- get_yahoo(f100$Tick[19],fr_dt,to_dt)
  td <- tick_data
  ln <- nrow(tick_data)
  td <- add_ta_simpleMA(tick_data, 50, 2, 3)
  
  train_knn <- td %>% filter(rn > 56, rn < ln-100) 
  y_train_knn <- train_knn$Dir
  train_knn <- train_knn %>% select(starts_with("ta"),Close)
  
  test_knn <- td %>% filter(rn > ln-100, !is.na(CloseLead)) 
  #test_knn2 <- td %>% filter(rn > ln-100) 
  y_test_knn <- test_knn$Dir
  test_knn <- test_knn %>% select(starts_with("ta"),Close)
  
  # calc scores, with scaling ... 
  predicted = class::knn(train = scale(train_knn), 
                                               test  = scale(test_knn),
                                               cl    = y_train_knn,
                                               k     = 5)
  
  
  p_res <- bind_cols(td %>% filter(rn > ln-100, !is.na(CloseLead)) , as.data.frame(predicted) )
  p_res$match <- ifelse(p_res$predicted==p_res$Dir,1,0)
  p_res$mat_ma <- round(runMean(p_res$match, n=10),2)
  
  class::knn(train = scale(train_knn), 
             test  = scale(test_knn),
             cl    = y_train_knn,
             k     = 5)
  
  actual = y_test_knn
  rr <- bind_cols(as.data.frame(actual),as.data.frame(predicted) )
  rr$match <- ifelse(rr$actual==rr$predicted,1,0)
  rr$ma5 <- round(runMean(rr$match, n=7),2)
  
  #rr <- rr %>% filter(!is.na(ma5))
  rr <- rr %>% mutate(rn = row_number())
  rr <- rr %>% mutate(grp = floor(rn/7))
  rr %>% group_by(grp) %>% summarise(sm = sum(ma5), n())
  
  floor(33/7)
  
  rr$rn <- 1
  for(i in 1:nrow(rr)){
    for(j in 1:7){
      
    }
  }
  
  
  
    
# Train Data --------------------------------------------------------------
  # trade data
    
    
  # set dates
    to_dt = today()
    fr_dt = to_dt - 200
  # get data  
    tick_data <- get_yahoo(f100$Tick[61],fr_dt,to_dt)
    
    td <- tick_data
    # Add ma data
      #td$ma25 <- round(runMean(td$Close, n=25),2)
      td$ta_ma50 <- round(runMean(td$Close, n=50),2)
      #td$ma25_50_diff <- td$ma25 - td$ma50
      td$ta_ma50diff <- td$Close - td$ma50
      td <- lag_n(td,3,"ma50diff")
      #td <- lag_n(td,3,"ma25_50_diff")
    # What happended in future ...
      td$Close3 <- lead(td$Close,3)
      td$Dir <- ifelse(td$Close<td$Close3,"L","S")
      td <- td %>% mutate(rn = row_number())
    # Aroon
      trend <- aroon( td[,c("High", "Low")], n=20 )
      td <- cbind(td,trend)
      td <- td %>% rename(ta_aroonUp = aroonUp, ta_aroonDn = aroonDn, ta_oscillator = oscillator)
      td <- lag_n(td,3,"ta_oscillator")
    

# Class Package -----------------------------------------------------------

library(class)
    
    f100 <- read.csv("Data/FTSE100.csv", stringsAsFactors = F)
    to_dt = today()
    fr_dt = to_dt - 150    
    tick_data <- get_yahoo(f100$Tick[2],fr_dt,to_dt)
    
    td <- tick_data

  # Add TA
    #debugonce(add_ta_simpleMA)
    td <- add_ta_simpleMA(tick_data, 50, 2, 3)
    td <- add_aroon(td)
      
  # knn from Class
    # order is imp ... need Dir, but not in train set ,,,
    ln <- nrow(td)
    train_knn <- td %>% filter(rn > 56, rn < ln-13) 
    y_train_knn <- train_knn$Dir
    train_knn <- train_knn %>% select(starts_with("ta"),Close)
      
    test_knn <- td %>% filter(rn > ln-13, !is.na(CloseLead)) 
    y_test_knn <- test_knn$Dir
    y_test_aroon <- test_knn$ta_oscillator
    y_test_aroonup <- test_knn$ta_aroonUp
    y_test_aroondn <- test_knn$ta_aroonDn
    test_knn <- test_knn %>% select(starts_with("ta"),Close)
    
  # calc scores, with scaling ... 
    calc_class_err(actual    = y_test_knn,
                   predicted = class::knn(train = scale(train_knn), 
                                          test  = scale(test_knn),
                                          cl    = y_train_knn,
                                          k     = 3))
    
    
  # aal, abf = 43% error ...
  # adm = 14%  
  # anto = 71%
    
    y_test_knn
    y_test_aroonup
    y_test_aroondn
    y_test_aroon
    
  # Scale
    class::knn(train = scale(train_knn), 
               test  = scale(test_knn),
               cl    = y_train_knn,
               k     = 3) 
    
    class::knn(train = train_knn, 
               test  = test_knn,
               cl    = y_train_knn,
               k     = 3)
    
      
    
      
# neighbr -----------------------------------------------------------------
      # neighbr pakcage
      library(neighbr)
      
      train <- td %>% filter(rn > 52, rn < 175) %>% 
        select(Close,ma50,ma501,ma502,ma503,Dir,rn)
      
      test_all <- td %>% filter(rn > 175, !is.na(Close3))
      test <- test_all %>% select(Close,ma50,ma501,ma502,ma503)
      
      fit <- knn(train_set=train,
                 test_set=test,
                 k=5,
                 categorical_target="Dir",
                 #continuous_target= "Close3",
                 comparison_measure="euclidean",
                 return_ranked_neighbors=3,
                 id="rn")
      
      fit$test_set_scores
      
      vignette("neighbr-help")
      
      
      # EXAMPLE  
      data(iris)
      # add an ID column to the data for neighbor ranking
      iris$ID <- c(1:150)
      # train set contains all predicted variables, features, and ID column
      train_set <- iris[1:145,]
      # omit predicted variables or ID column from test set
      test_set <- iris[146:150,-c(4,5,6)]
      fit <- knn(train_set=train_set,
                 test_set=test_set,
                 k=5,
                 categorical_target="Species",
                 continuous_target= "Petal.Width",
                 comparison_measure="euclidean",
                 return_ranked_neighbors=3,
                 id="ID")
      
      fit$test_set_scores
      

# ADX_SAR -----------------------------------------------------------------

library(astr)
library(TTR)
library(dplyr)
      
  # list of tick symbols
    f100 <- read.csv("../Trade/Data/FTSE100.csv", stringsAsFactors = F)
  # read  data set
    dn <- "../Trade/Data/StockData/FTSE100/"
    ds <- read.csv(paste0(dn,f100$Tick[4],".csv"), stringsAsFactors = F )
    
   
    
  # add sar_adx  
    res <- astr::sar_adx(ds)
    
    # data set - interface
    # Tick,Date,O,H,L,C,atr,trDir,trStart,pullBack
    
    names(res)
    
  # look for whales ...
    lw <- count_lng_whales(res)
    lw$pl <- lw$Close - lw$Open
    lw$hpl <- lw$High - lw$Open
    lw$lpl <- lw$Low - lw$Open
    
    lw$WL <- ifelse(lw$pl>0,"W","L")
    lw$abs_pl <- abs(lw$pl)
    lw$adj_pl <- lw$pl
    
    lw %>% filter(WL=="L") %>% summarise(sum(adj_pl))
    lw %>% filter(WL=="W") %>% summarise(sum(adj_pl))
    lw %>% summarise(sum(adj_pl))
    
    # adj 1 - neg pl, but high > atr and high atr > low atr
    for(i in 1:nrow(lw)){
      # max neg is atr
      if(lw$WL[i]=="L" & lw$abs_pl[i] > lw$atr[i]){
        lw$adj_pl[i] = -lw$atr[i] 
      }
      # if hpl > atr sloss is zero ...
      if(lw$WL[i]=="L" & lw$High_atr[i] < lw$Low_atr[i] & lw$hpl[i] >= lw$atr[i]){
        lw$adj_pl[i] = 0
      }
    }
    
    for(i in 1:nrow(lw)){
      # adj 2 - + pl, but low atr exits ...
      if(lw$WL[i]=="W" & lw$Low_atr[i] != 0){
        lw$adj_pl[i] = -lw$atr[i]
      }
    }
    
    lw %>% summarise(sum(adj_pl))
    
    lw %>% filter(WL=="W", adj_pl<0) %>% summarise(sum(adj_pl))
    lw %>% filter(WL=="W", adj_pl>0) %>% summarise(sum(adj_pl))
    lw %>% filter(WL=="L") %>% summarise(sum(adj_pl))
    lw %>% filter(WL=="L") %>% count()
      
    sum(lw$pl)
    lw %>% filter(pl>0) %>% count()
    lw %>% filter(pl>0) %>% group_by() %>% summarise(mean = mean(pl))
    lw %>% filter(pl<0) %>% group_by() %>% summarise(mean = mean(pl))

    
    trend <- aroon( ds[,c("High", "Low")], n=20 )
    ds2 <- cbind(ds,trend)
    
  # trading System - using sar_adx
  # 
    library(astr)
    library(TTR)
    library(dplyr)
    library(lubridate)
    library(quantmod)
    
    # list of tick symbols
      f100 <- read.csv("../Trade/Data/FTSE100.csv", stringsAsFactors = F)
    # get data
      astr::get_data(f100,"FTSE100", 500)
    
    # read  data set
    dn <- "../Trade/Data/StockData/FTSE100/"
    ds <- read.csv(paste0(dn,f100$Tick[4],".csv"), stringsAsFactors = F )
    
    res_sar <- astr::sar_adx(ds,1)
    res_macd <- astr::macd_data_set(ds,1)
    res_aroon <- astr::aroon_data_set(ds,1)
    
    rr <- astr::pullBackTrade(res_macd %>% filter(!is.na(atr), Date > "2018-01-01" ), 1, 1)
    rr <- add_pl_pb(rr)
    sum(rr$atrpl)
    
    rr <- astr::pullBackTrade(res_sar %>% filter(!is.na(atr), Date > "2018-01-01" ), 1, 1)
    rr <- add_pl_pb(rr)
    sum(rr$atrpl)
    
    debugonce(pullBackTrade)
    rr <- astr::pullBackTrade(res_aroon %>% filter(!is.na(atr), Date > "2018-01-01" ), 1, 1)
    rr <- add_pl_pb(rr)
    sum(rr$atrpl)
    
    # loop thru a few f100 ticks
      # sar_adx or macd_data_set adds pull back signal - pass in atr factor
      # pullBackTrade create the results set, based on pull backs, pass in atr for pTarg and sLoss
      #debugonce(loop_pullBackTrade)
      rr_all <- loop_pullBackTrade(astr::sar_adx, f100,1,pTarg = 1, sLoss = 1)
      rr_all <- loop_pullBackTrade(astr::macd_data_set, f100,1,pTarg = 1, sLoss = 1)
      rr_all <- loop_pullBackTrade(astr::aroon_data_set, f100,1,pTarg = 1, sLoss = 1)
      
      sum(rr_all$atrpl)
      sum(rr_all[rr_all$atrpl==1,"atrpl"])
      sum(rr_all[rr_all$atrpl < 0,"atrpl"])
      rr_all %>% filter(atrpl == 0) %>% count()
      rr_all %>% filter(atrpl == 0) %>% mutate(pl = Close - Open, hpl = High - Open) %>% 
        select(Tick,Open,High,Low,Close,pl,hpl,Atr,atrpl) %>% View()
      
        #summarise(smpl = sum(pl))
      
    loop_pullBackTrade <- function(FUN, index_lp, atr_fac,pTarg,sLoss){
      dn <- "../Trade/Data/StockData/FTSE100/"
      pall_rr <- NULL
      for(i in 1:20){
        pds <- read.csv(paste0(dn,f100$Tick[i],".csv"), stringsAsFactors = F ) 
        pres_sar <- FUN(pds,atr_fac)
        prr <- astr::pullBackTrade(pres_sar %>% filter(!is.na(atr), Date > "2018-10-01" ), pTarg, sLoss)
        prr <- astr::add_pl_pb(prr,pTarg,sLoss)
        pall_rr <- rbind(pall_rr,prr)
      }
      return(pall_rr)
    }
    
    
    
    rr %>% filter(sLossNum == 0, pTargNum == 0) %>% View()
    rr %>% filter(sLossNum > 0) %>% count()
    rr %>% filter(pTargNum > 0) %>% count()
    rr %>% filter(sLossNum > 1, pTargNum != 0) %>% count()
    rr %>% filter(sLossNum == 0, pTargNum > 0) %>% count()
    rr %>% filter(sLossNum == 0, pTargNum == 0) %>% count()
    rr %>% filter(sLossNum == 0, pTargNum < sLossNum) %>% count()
    
   
    
    
    
    
    nrow(res_macd[!is.na(res_macd$pullBack),]) / nrow(res_macd)

    res <- res_macd
    res <- res_sar
    
    res %>% filter(!is.na(trStart)) %>% select(Tick,Date) %>% arrange(desc(Date)) %>% mutate(rn = row_number())
    res %>% filter(!is.na(trStart)) %>% select(Tick,Date) %>% group_by() %>% filter(Date == max(Date))
    
    debugonce(loop_index_TA)
    rr <- loop_index_TA(f100,sar_adx,1)
    
    debugonce(loop_index_TA_last)
    rr <- loop_index_TA_last(f100,sar_adx,1)
    
    pb <- rr %>% filter(!is.na(pullBack)) %>% select(Tick) %>% distinct()
    rr %>% filter(Tick %in% pb$Tick) %>% View()
    
    res <- res %>% filter(Date > "2019-09-09")
    debugonce(outer_trade)
    trpb <- astr::outer_trade(res, lng_tradePB, pTarg = 1, sLoss = 2)
    #trl <- astr::outer_trade(res, lng_trade, pTarg = 1, sLoss = 2) 
    
    tr <- trpb
    tr <- astr::Add_pl(tr)
  
    sum(tr$pl)
    sum(tr$adjpl)
    mean(tr$adjpl[tr$adjpl!=0])
    mean(tr[tr$adjpl>0,"pl"])
    mean(tr[tr$adjpl<0,"pl"])
    tr %>% filter(adjpl>0) %>% count() / tr %>% filter(tr$adjpl!=0) %>% count() * 100
    tr$Year <- lubridate::year(tr$OpenDate)
    tr %>% group_by(Year) %>% summarise(sm = sum(adjpl))
    
    res_macd <- astr::macd_data_set(ds,1)
    
   # loop
    # 1. macd, pull back, 1, 1
      r_lp_1_1 <- loop_index(f100, macd_data_set,1, lng_tradePB, pTarg = 1, sLoss = 1)
      r_lp_1_1 %>% group_by_year(2018) %>% View()
      sum(r_lp_1_1$adjpl)
      mean(r_lp_1_1$adjpl)
      
    # 1. macd, pull back, 1, 2
      r_lp_1_2 <- loop_index(f100, macd_data_set, lng_tradePB, pTarg = 1, sLoss = 2)
      r_lp_1_2 %>% group_by_year(2018) %>% View()
      sum(r_lp_1_2$adjpl)
      mean(r_lp_1_2$adjpl)
    
    # 3. sar_adx, pull back, 1, 1
      sar_lp_1_1 <- loop_index(f100, sar_adx, lng_tradePB, pTarg = 1, sLoss = 1)
      sar_lp_1_1 %>% group_by_year(2018) %>% View()
      sum(sar_lp_1_1$adjpl)
      mean(sar_lp_1_1[sar_lp_1_1$adjpl!=0,"adjpl"])
      
    # 1. sar_adx, pull back, 1, 1
      sar_lp_1_2 <- loop_index(f100, sar_adx, 1, lng_tradePB, pTarg = 1, sLoss = 2)
      sar_lp_1_2 %>% group_by_year(2018) %>% View()
      sum(sar_lp_1_2$adjpl)
      mean(sar_lp_1_2$adjpl)
      mean(sar_lp_1_2[sar_lp_1_1$adjpl!=0,"adjpl"])
      
      
    group_by_year <- function(pdata,yr){
      pdata$Year <- lubridate::year(pdata$OpenDate)
      res <- pdata %>% filter(Year > yr) %>% group_by(Tick,Year) %>% summarise(sm = sum(adjpl))
      return(res)
    }
    
    
    
    ds4 <- read.csv(paste0(dn,f100$Tick[1],".csv"), stringsAsFactors = F )
    ds5 <- read.csv(paste0(dn,f100$Tick[4],".csv"), stringsAsFactors = F )
    dt4 <- add_ta_set(ds4, FUN = trDir_add_adx_sar)
    dt5 <- add_ta_set(ds5, FUN = trDir_add_adx_sar)
    dt45 <- inner_join(dt4 %>% select(Tick,Date,Close,atr,trDir),
                       dt5 %>% select(Tick,Date,Close,atr,trDir),
                       by = "Date")
    
    dt45$Diff  <- dt45$Close.y - dt45$Close.x
    dt45$Trade <- ifelse(dt45$trDir.x != dt45$trDir.y, "Y","")
    
    
    rr <- loop_index_TA_last_HH(f100,sar_adx,1)

    rr <- loop_index_TA_last_HH(f100,aroon_data_set,1)
    rr %>% filter(HHR == 1, trDir == "L") %>% View()
    rr %>% filter(LLR == 1, trDir == "S") %>% View()
    
    dn <- "../Trade/Data/StockData/FTSE100/"
    ds5 <- read.csv(paste0(dn,f100$Tick[7],".csv"), stringsAsFactors = F )
    dt5 <- add_ta_set(ds5, FUN = trDir_add_aroon)
    dt5$pLow <- lag(dt5$Low)
    dt5$pHigh <- lag(dt5$High)
    dt5$LL <- dt5$pLow - dt5$Low
    dt5$HH <- dt5$High - dt5$pHigh
    
    dt5$HHR <- 0
    dt5$HHRmx <- 0
    rn <- 1
    prevrn <- 1
    
    for(i in 2:nrow(dt5)){
      if(dt5$HH[i] > 0){
        dt5$HHR[i] <- rn
      } else {
        dt5$HHRmx[i-1] <- prevrn
        rn <- 0
      }
      prevrn <- rn
      rn <- rn + 1
    }
    
    dt5$LLR <- 0
    dt5$LLRmx <- 0
    rn <- 1
    for(i in 2:nrow(dt5)){
      if(dt5$LL[i] > 0){
        dt5$LLR[i] <- rn
      } else {
        dt5$LLRmx[i-1] <- prevrn
        rn <- 0
      }
      prevrn <- rn
      rn <- rn + 1
    }
    
    
    dt5L <- dt5 %>% filter(trDir == "L")
    dt5S <- dt5 %>% filter(trDir == "S")
    table(dt5$HHRmx)
    table(dt5$HHR)
    table(dt5L$HHR)
    nrow( dt5L[dt5L$HHR==1,] )
    nrow( dt5L[dt5L$HHR==2,] )
    29/47
    34/82
    nrow( dt5S[dt5S$LLR==1,] )
    nrow( dt5S[dt5S$LLR==2,] )
    14/28
    
    dt5 %>% select(Tick,Date,Open,High,Low,Close,atr,trDir,HH,HHR,LL,LLR) %>% View()
    
    dt5 <- dt5 %>% select(Tick,Date,Open,High,Low,Close,atr,trDir,LL,trStart)
    dt5 <- dt5 %>% mutate(across(is.numeric, round, 0)) 
    dt5 %>% filter(trDir == "S") %>% select(Tick,Date,Open,High,Low,Close,atr,trDir,LL,trStart) %>% View()
    dt5 %>% filter(trDir == "L") %>% select(Tick,Date,Open,High,Low,Close,atr,trDir,HH,trStart) %>% View()
    dt5 %>% select(Tick,Date,Open,High,Low,Close,atr,trDir,HH,LL,trStart) %>% View()
    
    debugonce(lag_lead_n)
    dt5 <- lag_lead_n(dt5, 7, "High", lead)
    dt5 <- lag_lead_n(dt5, 7, "Low", lead)
    dt5$PTH <- dt5$pHigh + dt5$atr
    dt5$SLH <- dt5$pHigh - dt5$atr
    dt5$HL <- ifelse(dt5$High - dt5$Close > dt5$Close - dt5$Low, "HL","LH")
    
    
    
    
    dt5 <- lag_n(dt5, 7, "Low")
    dt5$l1 <- dt5$Low - dt5$Low1
    dt5$l2 <- dt5$Low1 - dt5$Low2
    dt5$l3 <- dt5$Low2 - dt5$Low3
    dt5$l4 <- dt5$Low3 - dt5$Low4
    dt5$l5 <- dt5$Low4 - dt5$Low5
    
    
    debugonce(loop_index_TA)
    f100a <- f100 %>% filter(Tick != "RBS.L")
    rr_all <- loop_add_ta_set(f100a)
    
    table(rr_all$trDir)
    
    # trend direction/strength
    # aroon, SeeCCI,ADX,TDI,VHF,GMMA
    
    # Aroon
      dt <- ds
      dt <- add_ta_set(dt, FUN = trDir_add_aroon)
      
   # Know Sure Thing
      dt <- ds
      dt <- add_ta_set(dt, FUN = trDir_add_kst)  
      
   # TRIX
      dt <- ds
      debugonce(add_ta_set)
      dt <- add_ta_set(dt, FUN = trDir_add_trix)  
      
   # ma 20 50
      dt <- ds
      dt <- add_ta_set(dt, FUN = trDir_add_20_50_ma)  
      
  
   dt <- ds  
    
   # Bands    
   y <- TTR::BBands(dt[,c("High","Low","Close")])
   dt <- cbind(dt,y)
   
   dt <- ds
   y <- TTR::DonchianChannel(dt[,c("High","Low")])
   dt <- cbind(dt,y)
   
   dt <- ds
   y <- TTR::CLV(dt[,c("High","Low","Close")])
   dt <- cbind(dt,y)
   
   dt <- ds
   cmo <- CMO(dt[,"Close"])
   dt <- cbind(dt,cmo)
   
   dt <- ds
   cci <- CCI(dt[,c("High","Low","Close")])
   dt <- cbind(dt,cci)
   
   
   dt <- ds
   cmo <- CMO(dt[,c("Close")])
   dt <- cbind(dt,cmo)
  
   # CTI in vignette not pkg  
   dt <- ds
   cti <- CTI(dt[,"Close"])
   dt <- cbind(dt,cmo)
   
   dt <- ds
   gmma <- GMMA(dt[,"Close"])
   dt <- cbind(dt,gmma)
   
   
   dt <- ds
   kst <- KST(dt[,"Close"])
   dt <- cbind(dt,kst)
   
   dt <- ds
   snr <- SNR(dt[,c("High","Low","Close")],n=10)
   dt <- cbind(dt,snr)
   
   dt <- ds
   snr <- TDI(dt[,c("Close")],n=10)
   dt <- cbind(dt,snr)
   
   
   dt <- ds
   trix <- TRIX(dt[,c("Close")],n=10)
   dt <- cbind(dt,trix)

    
   TTR::RSI()
   TTR::SNR()
   TTR::stoch()
   TTR::SMI()
   TTR::TDI()
   TTR::TRIX()
   TTR::ultimateOscillator()
   TTR::VHF()
   TTR::volatility()
   TTR::williamsAD()
   TTR::WPR()
   TTR::ZigZag()
   
   tt <- res_aroon %>% select(Tick,Date,Open,High,Low,Close,atr,trDir) %>% 
     mutate(mx = runMax(High,n=7), mn = runMin(Low,n=7))
   tt$Diff <- ifelse(tt$trDir=="L",tt$mx-tt$Close,tt$Close-tt$mn)
    
 

# HH System ---------------------------------------------------------------

   # load libs
   library(quantmod) #wrapper for ttr functions ...
   library(TTR)
   library(dplyr)
   library(lubridate)
   library(astr)
   
  # Fri 20 - mainly L
  # Mon 22 - mainly L
   # Tue 23 - 11 x S, 9 x L
   
   # load ticksymbols
   f100 <- read.csv("../Trade/Data/FTSE100.csv", stringsAsFactors = F)
   
   # get all ftse data
   # only need to run once
   astr::get_data(f100,"FTSE100",200)
   
   #debugonce(loop_index_TA_last_HH) 
   rr <- loop_index_TA_last_HH(f100,aroon_data_set,1)
   L <- rr %>% filter(HHR == 1, trDir == "L")
   S <- rr %>% filter(LLR == 1, trDir == "S")
   View(L)
   View(S)
    rr %>% filter(LLR == 1) %>% View()
   
   td <- today()
   write.csv(L, paste(td, " - L_HH.csv"))
   write.csv(S, paste(td, " - S_HH.csv"))
   
   # Fre 23/10/20  L(10) / s(6)
   # Tue 26/10 - L(40) S (1)
   # Wed 27/10 - L(4), S (7)
   # Thur l(4) S(10)
   # Fri 29/10 L(10) S(2)
   # Tue 3/11 L(2) S(26)
   # Wed 4/11 L(5) S(1)
   # Thur 5/11 L(1) S(35)
   # Mon 9/11 L(2) S(29)
   # Wed 11/11 L(1) S(2)
   
   dn <- "../Trade/Data/StockData/FTSE100/"
   loop_ds <- read.csv(paste0(dn,f100$Tick[27],".csv"), stringsAsFactors = F )
   loop_res <- aroon_data_set(loop_ds, 1)
   
   test <- read.csv(paste0(dn,f100$Tick[2],".csv"), stringsAsFactors = F )
   test <- astr::add_ATR(test)
   test$tr5 <- TTR::runMean(test$tr,5)
   test$Diff <- test$tr5 - test$atr
   
   # check if 1 - o - 1 is good ...
   # get data set
    tset <- read.csv(paste0(dn,f100$Tick[2],".csv"), stringsAsFactors = F )
   #
    tset <- aroon_data_set(tset, 1)
   #
    tset$tr <- tset$High - tset$Low
    tset$atr <- TTR::runMean(tset$tr,14)
   # add HH stop
    tset$pLow <- lag(tset$Low)
    tset$pHigh <- lag(tset$High)
    tset$LL <- tset$pLow - tset$Low
    tset$HH <- tset$High - tset$pHigh
    tset <- add_HH(tset)
    tset <- add_LL(tset)
    
    tset <- tset %>% select(Tick,Date,Open,High,Low,Close,atr,trDir,HH,HHR,LL,LLR)
    tset <- tset %>% mutate(across(where(is.numeric), round, 0))
    
    # flt.r
   