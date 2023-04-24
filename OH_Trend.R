
# OH trend ...
# 1. Basic strong trend - take MA of OH and OL distance
#    is basic trend if MA OH > MA OL? or vice versa
#    if diff between them is >= 0.2 of the ATR then TRADE L or S?
#
# 2. Trade 1 - open at day one of trend, close on last day
#    How do we know it is last day? Maybe day after, on open
#    a. Close at end
#    b. Close once make 1 ATR
#
# 3. Trade 2 - trade each day,
#    a. close if max of 3 / 5 days hits 1 ATR
#    b. close after 3 / 5 days etc
#
# 4. Trade 3 - look for pull back in trend
#    Trend is L, if MA of OL (3 days?) is above
#    MA of OH (3 days) go long ...

# # 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

# Basic Trend -------------------------------------------------------------

ast::load_libs()
saveRDS(ftse100_df, "/home/allanx38/ast/Data/FTSE100Ticks.rds")



# get data
  # Single Tick ...
  rm(idata)
  # from web
    idata <-  ast::get_yahoo_fin_tick("^FTSE", "FTSE100", 3000)
    write_csv(idata, "/home/allanx38/ast/Data/FTSE_Data/^FTSE.csv")

  # from disc
    idata <- read.csv("/home/allanx38/ast/Data/FTSE_Data/AAL.L.csv")
  # from disc - use new function ...
    idata <- get_ftse_csv_file("^FTSE.csv")
    idata %>% tail()

# Add trend
  base <- oh_base(idata, days_back = 5)
  # View
  base %>% select(1:8,atr,trn,rn,LS,OH_LS,OL,maOL,maOL3,maD_atr,mxH,mnL) %>%
    #filter(OH_LS!="") %>%
    View()

  # load FTSE tickers from file
  ftse_tick <- readRDS("/home/allanx38/ast/Data/YahooFtseSymbols.rds")

# Update files on disc
  # load FTSE tickers from saved csv
  # update FTSE data on file - loops thru file names ...
  # ftse_file_names <- list.files(path="/home/allanx38/ast/Data/FTSE_Data")
  #debugonce(update_ftse_tick_saved)
  ast::load_libs()
  update_ftse_tick_saved()
  read.csv("/home/allanx38/ast/Data/FTSE_Data/AAL.L.csv") %>% tail()

  rr <- ast::get_yahoo_fin_tick("AAL.L", "FTSE100", 100)
  rr %>% tail()

# Loop thru all ftse files and apply oh_base
  base_ftse <- loop_thru_ftse_oh(1, 5)
  saveRDS(base_ftse, "/home/allanx38/ast/Data/ftse_base_oh.rds")


# Trade 1 -----------------------------------------------------------------

  # trying out new functions ...
   #tick <- ast::get_ftse_ticks()
   #data <- ast::get_ftse_csv_file(tick$ftse_csv[1])

  # trade results ... group by trn
  # calc open, close, max etc ...
  oh_trade1_fnc <- function(trade_data){
    bind_rows(
    trade_data %>%
      filter(LS == "L") %>%
      group_by(Tick,trn) %>%
      summarise(Dir = "L",
                Tick = first(Tick),
                OpenD = first(Date),
                CloseD = last(Date),
                OpenP = first(Close),
                CloseP = last(Close),
                maxP = max(High),
                cpl = CloseP - OpenP,
                mpl = maxP - OpenP,
                atr = mean(atr, na.rm = T),
                n = n(),
                matr = if_else(mpl>atr,atr,cpl)) %>%
      group_by() %>%
      mutate(sm_cpl = sum(cpl), sm_mpl = sum(matr)),

    trade_data %>% filter(LS=="S") %>%
      group_by(Tick,trn) %>%
      summarise(Dir = "S",
                Tick = first(Tick),
                OpenD = first(Date),
                CloseD = last(Date),
                OpenP = first(Close),
                CloseP = last(Close),
                maxP = min(Low),
                cpl = OpenP - CloseP,
                mpl = OpenP - maxP,
                atr = mean(atr, na.rm = T),
                n = n(),
                matr = if_else(mpl>atr,atr,cpl)) %>%
      group_by() %>%
      mutate(sm_cpl = sum(cpl), sm_mpl = sum(matr))
    )
  }

  trade_data <- get_ftse_csv_file("AZN.L.csv")
  trade_data <- oh_base(trade_data, days_back = 5)
  oh_trade_res <- oh_trade1_fnc(trade_data)
  oh_trade_res_L <- oh_trade_res %>% filter(Dir=="L")

  bind_cols(
  oh_trade_res %>% group_by(Tick,Dir) %>%
    summarise(cpl = sum(cpl)) %>%
    pivot_wider(names_from = Dir, values_from = cpl) %>%
    mutate(Tot_cpl = L+S) %>%
    rename(L_cpl = L, S_cpl = S) %>%
    ungroup(),

  oh_trade_res %>% group_by(Tick,Dir) %>%
    summarise(atrpl = sum(matr)) %>%
    pivot_wider(names_from = Dir, values_from = atrpl) %>%
    mutate(Tot_atrpl = L+S) %>%
    rename(L_atrpl = L, S_atrpl = S) %>% ungroup() %>%
    select(-Tick)
  )


  ## Check current state

# Trade 2 -----------------------------------------------------------------

  # filter on FTSE 100

  ftse <-  get_ftse_csv_file("^FTSE.csv") %>%
    oh_base() %>%
    select(Date, LS, maD_atr) %>%
    rename(FLS = LS, FmaD_atr = maD_atr)

  trade_data <- get_ftse_csv_file("AZN.L.csv") %>% oh_base(days_back = 5)
  trade_data <- get_dow_csv_file("AAPL.csv") %>% oh_base(days_back = 5)
  trade_data <- left_join(trade_data, ftse, by = join_by(Date))

  # trade just on L
  oh_trade_res <- oh_trade1_lng_summary(trade_data %>% filter(LS == "L"))
  # trade on L and LS
  oh_trade_res_2 <- oh_trade1_lng_summary(trade_data %>% filter(LS == "L", FLS == "L"))
  # How did we do
  mean(oh_trade_res_2$cpl)
  mean(oh_trade_res_2$atr)
  sum(oh_trade_res_2$cpl)
  mean(oh_trade_res$cpl)
  mean(oh_trade_res$atr)

  # Add extra filter from FTSE ...
  tdata <- trade_data %>% filter(LS == "L", FLS == "L", FmaD_atr >= 0)
  oh_trade_res_3 <- oh_trade1_lng_summary(tdata)
  mean(oh_trade_res_3$cpl)
  mean(oh_trade_res_3$atr)
  sum(oh_trade_res_3$cpl)

  tdata_s <- trade_data %>% filter(LS == "S", FLS == "S", FmaD_atr <= -1)
  oh_trade_res_3s <- oh_trade1_sht_summary(tdata_s)
  mean(oh_trade_res_3s$cpl)
  mean(oh_trade_res_3s$atr)
  sum(oh_trade_res_3s$cpl)


  # select trade runs where LS = L and FLS = L on day 1 ...
  # mmm doesn't seem to be any good ...
  # selct rn =1
  lng_trn <- trade_data %>% filter(rn==1, LS == "L", FLS == "L") %>% pull(trn)
  oh_trade_res_4 <- oh_trade1_lng_summary( trade_data %>% filter(trn %in% lng_trn) )
  mean(oh_trade_res_4$cpl)
  mean(oh_trade_res_4$atr)


  #loop thru all ticks in index
  loop_trade1_index <- function(index_tick, FUN, tickFUN){

    all_res <- NULL
    index <-  FUN(index_tick) %>%
      oh_base() %>%
      select(Date, LS, maD_atr) %>%
      rename(FLS = LS, FmaD_atr = maD_atr)

    ticks <- tickFUN()
    for(i in 1:nrow(ticks)){

      trade_data <- FUN(ticks$csv[i]) %>% oh_base(days_back = 5)
      trade_data <- left_join(trade_data, index, by = join_by(Date))

      l_res <- oh_trade1_lng_summary(trade_data %>% filter(Date > "2021-15-04",
                                                           LS == "L",
                                                           FLS == "L"))
      s_res <- oh_trade1_sht_summary(trade_data %>% filter(Date > "2022-15-04",
                                                           LS == "S",
                                                           FLS == "S"))
      res <- bind_rows(l_res,s_res)
      all_res <- bind_rows(all_res, res)
    }

    return(all_res)
  }

  t1_summary <- function(t1_res, dir){
  t1_res %>% filter(Dir == dir) %>% group_by(Tick, Dir) %>%
    summarise(n = n(),
              av_cpl = mean(cpl),
              av_atr = mean(atr),
              atr_cp = av_cpl / av_atr * 100) %>%
    arrange(desc(atr_cp)) %>% janitor::adorn_rounding()
  }

  ftse_t1_res <- loop_trade1_index("^FTSE.csv", get_ftse_csv_file,  get_ftse_ticks)
  t1_lng_ticks <- t1_summary(ftse_t1_res, "L") %>% filter(atr_cp > 60) %>% select(Tick)
  saveRDS(t1_lng_ticks, "/home/allanx38/ast/Data/t1_lng_ticks.rds")
  t1_lng_ticks <- readRDS("/home/allanx38/ast/Data/t1_lng_ticks.rds")

  # Dow
  dow_t1_res <- loop_trade1_index("^DJI.csv", get_dow_csv_file, get_dow_ticks)
  t1_summary(dow_t1_res, "L") %>% View()
  loop_trade1_index("^GSPC.csv", get_sp500_csv_file, get_sp500_ticks)

  rr <- get_ftse_ticks()
  nrow(rr)

  # try SP500
  #sp500 <- get_sp500_ticks()

  sp5 <-  get_sp500_csv_file("^GSPC.csv") %>%
    oh_base() %>%
    select(Date, LS, maD_atr) %>%
    rename(FLS = LS, FmaD_atr = maD_atr)

  rm(trade_data)
  trade_data <- get_sp500_csv_file("UNH.csv") %>% oh_base(days_back = 5)
  trade_data <- left_join(trade_data, sp5, by = join_by(Date))
  trade_data <- trade_data %>% filter(Date > "2022-01-01")

  # trade just on L
  oh_trade_res <- oh_trade1_lng_summary(trade_data %>% filter(LS == "L"))
  # trade on L and LS
  oh_trade_res_2 <- oh_trade1_lng_summary(trade_data %>% filter(LS == "L", FLS == "L"))
  # How did we do
  mean(oh_trade_res_2$cpl)
  mean(oh_trade_res_2$matr)
  mean(oh_trade_res_2$atr)
  sum(oh_trade_res_2$cpl)
  mean(oh_trade_res$cpl)
  mean(oh_trade_res$atr)



# Ideas -------------------------------------------------------------------

L/S - against the trend?


# Trade 3 - Pull back -----------------------------------------------------

  # Ideas
  # 1. Add an indicator LSPB - L or S with pull back
  # 2. use 1. to start trade ...


  base <- ast::get_ftse_csv_file("AZN.L.csv")
  base <- base %>% oh_base()

  # oh_long has pull back
  base %>% oh_long_pb() %>% View()
  base %>% oh_long_pb() %>% group_by(trn) %>% summarise(fsm=sum(fpl, na.rm = T),
                                                  csm=sum(clpl,na.rm = T),
                                                  atr=mean(atr),
                                                  atrfpl = round(fsm/atr,1),
                                                  n=n())


  base %>% oh_short() %>% group_by() %>% summarise(fsm=sum(fpl),
                                                   csm=sum(clpl),
                                                   atr=mean(atr),
                                                   atrfpl = round(fsm/atr,1),
                                                   n=n())


ast::load_libs()




# Are any stocks ready to go?
  # Long
  oh_top_lng <- c("NG.L","UU.L","EXPN.L","PRU.L","SMIN.L","NXT.L","INF.L","RMV.L","BNZL.L","BP.L","SDR.L")
  #debugonce(todays_lng_oh_trades)
  todays_lng_oh_trades(oh_top_lng, 15, 0.5)
  # Short
  oh_top_sht <- c("BRBY.L","AAL.L","GSK.L","OCDO.L","SVT.L","PSON.L","BDEV.L","HL.L","NG.L","ABF.L" )
  todays_sht_oh_trades(oh_top_sht, -15, 0.5)





# Loop thru all ftse tick
  # Add base details ...
  base_ftse <- loop_thru_ftse_oh(1, 5)

  #base_ftse %>% oh_long() %>% View()

# Long results
base_ftse %>% filter(Date > '2021-01-01') %>%
  oh_long() %>% group_by(Tick) %>% summarise(fsm=sum(fpl, na.rm = T),
                                             csm=sum(clpl, na.rm = T),
                                             atr=round(mean(atr)),
                                             atrfsm = round(fsm/atr,1),
                                             atrcsm = round(csm/atr,1),
                                             n=n(),
                                             ratio = round(atrfsm/n,2)) %>%
  arrange(desc(ratio)) %>% slice(1:14) %>%
  mutate(sm_atrf = sum(atrfsm), sm_atrc = sum(atrcsm), sm_n = sum(n)) %>%
  filter(atr>10) %>%
  select(Tick)

# Short results
base_ftse %>% filter(Date > '2021-01-01') %>%
  oh_short() %>% group_by(Tick) %>% summarise(fsm=sum(fpl),
                                             csm=sum(clpl),
                                             atr=round(mean(atr)),
                                             atrfpl = round(fsm/atr,1),
                                             n=n(),
                                             ratio = round(atrfpl/n,2)) %>%
  arrange(desc(ratio)) %>% slice(1:12) %>%
  mutate(sm_atr = sum(atrfpl), sm_n = sum(n)) %>%
  View()







debugonce(get_yahoo_fin_tick_tc)
rr <- ast::get_yahoo_fin_tick_tc("RDSB.L", "RDSB.L", 600)
debugonce(get_yahoo_fin_tick)
rr <- ast::get_yahoo_fin_tick("RDSB.L", "RDSB.L", 600)

#res_all <- NULL

rm_tck <- c("RDSB.L","RDSA.L", "NMC.L","RSA.L","RBS.L", "SLA.L","MRW.L")
YahooFtseSymbols_loop <- YahooFtseSymbols %>% filter(!Tick %in% rm_tck)

ftse_res <- test()
ftse_res$ftse_oh_data %>% View()
ftse_res$res_all_n

saveRDS(ftse_res$ftse_oh_data,"/home/allanx38/ast/Trade/ftse_oh_data.rds")


res_all_n <- ftse_res$res_all_n
res_all_n %>% filter(mxplatr < 0.5) %>% count()
res_all_n %>% filter(mxplatr > 0.5, mxplatr < 1) %>% count()
res_all_n %>% filter(mxplatr >= 1) %>% count()

ftse_res_all <- ftse_res

test <- function(){
  res_all_n <- NULL
  ftse_oh_data <- NULL

  for(i in 1:nrow(YahooFtseSymbols_loop)){
    res <- NULL
    idata <- NULL
    tick <- YahooFtseSymbols_loop$Tick[i]
    print(paste0(tick," - ", i))
    #browser()
    gc()
    idata <-  ast::get_yahoo_fin_tick(tick, tick, 400)
    if(!is.null(idata)){
      base <- oh_base(idata, days_back = 5)
      ftse_oh_data <- bind_rows(ftse_oh_data,base %>% tail(15))
      res <- bind_rows(
      base %>% filter(OH_LS == "L") %>% group_by(trn) %>%
        summarise(Tick = first(Tick),
                  Dir = "L",
                  n = n(),
                  atr = mean(atr),
                  open = first(Close),
                  dir_mx = max(High),
                  mxplatr = round((dir_mx - open) / atr,1),
                  smmxplatr = sum(mxplatr),
                  atr = round(mean(atr),0),
                  close = last(Close),
                  clplatr = round((close - open) / atr,1),
                  openD = first(Date),
                  closeD = last(Date)),

      base %>% filter(OH_LS == "S") %>% group_by(trn) %>%
        summarise(Tick = first(Tick),
                  Dir = "S",
                  n = n(),
                  atr = mean(atr),
                  open = first(Close),
                  dir_mx = min(Low),
                  mxplatr = round((open - dir_mx) / atr,1),
                  atr = round(mean(atr),0),
                  close = last(Close),
                  clplatr = round((open - close) / atr,1),
                  openD = first(Date),
                  closeD = last(Date))
      ) %>% group_by() %>% mutate(smmxplatr = sum(mxplatr),
                                  smclplatr = sum(clplatr))

      res_all_n <- bind_rows(res_all_n, res)
    }
}
  res_list <- list(res_all_n = res_all_n, ftse_oh_data = ftse_oh_data)
  return(res_list)
}


res_all %>% select(Tick) %>% distinct() %>% count()
res_all2 <- res_all %>% distinct()
res_all2 %>% select(Tick) %>% distinct() %>% count()
res_all2 %>% filter(mxplatr < 0.5) %>% count()
res_all2 %>% filter(mxplatr > 0.5) %>% count()
res_all2 %>% filter(mxplatr > 0.99) %>% count()

base %>% select(1:6,Close3,mxH,mnL,atr,rn,trn,maOH,maOH3,maOL,maOL3,OH_LS,maD_atr) %>%
  filter(OH_LS == "L") %>%
  #filter(maD_atr > 20, (maOL3/atr) > 0.5) %>%
  View()


base %>% select(1:6,Close3,mxH,mnL,atr,rn,trn,maOH,maOH3,maOL,maOL3,OH_LS,maD_atr) %>%
  filter(OH_LS == "S") %>%
  #filter(maD_atr > 10,(maOH3/atr) > 0.1) %>%
  View()

# simple idea ...
# strong trend, one big pull back OH/OL near ATR
idata <-  ast::get_yahoo_fin_tick("VOD.L", "VOD.L", 600)
base <- oh_base(idata, days_back = 5)


# -------------------------

  getwd()
  loop_test(10)

loop_test <- function(rn_lp_num){

  data <- readRDS("Trade/ftse_oh_data.rds")
  tck <- data %>% select(Tick) %>% distinct()

  for(i in 1:nrow(tck)) {
    loop_data <- data %>% filter(Tick == tck$Tick[i])
    rn_num  <-  loop_data$rn[nrow(loop_data)]
    ohls  <-  loop_data$OH_LS[nrow(loop_data)]

    if(rn_num < rn_lp_num & ohls != ""){
      print(tck$Tick[i])
      #browser()
      print(loop_data %>%
              select(1:6,atr,rn,trn,maOH,maOH3,maOL,maOL3,OH_LS,maD_atr) %>%
              tail(n=7))
    }
  }
}


# --------------------------

base <- base_ftse

base$sig <- ""
# Long
base[ which( base$LS == "L" & base$maD_atr > 1 & base$OL > (base$atr*1) ), "sig" ] <- "L"
base <- base %>% mutate(lpl = mxH - Close, lplatr = round(lpl/atr,1))
base <- base %>% mutate(lcpl = Close3-Close, lcplatr = round(lcpl/atr,1))
basel <- base %>% filter(sig %in% c("L")) %>%
  select(1:7,atr,lpl,lplatr,lcpl,lcplatr,mxH,mnL,maD_atr,LS,sig,rn,OL,OH,) %>%
  filter(!is.na(lplatr))
basel$ct <- cut(basel$lplatr,breaks=c(-1,0.5,0.999,10))
basel %>% group_by(ct) %>% tally()
basel %>% filter(sig=="L") %>% count()
sum(basel$lcplatr, na.rm = T)

basel %>% group_by(Tick) %>% summarise(sm = sum(lcplatr,na.rm = T), n = n()) %>% View()

base[ which( base$LS == "S" & base$maD_atr < 0 & base$OH > (base$atr*0.8) ), "sig" ] <- "S"

# Misc --------------------------------------------------------------------

idata %>%
  mutate(nOpen = lead(Open)) %>%
  ast::add_ATR() %>%
  mutate(OH = High-Open,
         OL = Open-Low) %>%
  mutate(maOH = TTR::runMean(OH,n=15),
         maOH3 = TTR::runMean(OH,n=3),
         maOL = TTR::runMean(OL,n=15),
         maOL3 = TTR::runMean(OL,n=3),
         Diff = (maOH-maOL)) %>%
  mutate(maDiff = TTR::runMean(Diff, n = 7),
         maD_atr = maDiff/atr*100) %>%
  #mutate(LS = if_else(maOH > maOL, "L", "S")) %>%
  mutate(LS = if_else(maDiff > 0, "L", "S")) %>%
  mutate(mxH1 = lead(High,n=1),
         mxH2 = lead(High,n=2),
         mxH3 = lead(High,n=3)) %>%
  mutate(mnL1 = lead(Low,n=1),
         mnL2 = lead(Low,n=2),
         mnL3 = lead(Low,n=3)) %>%
  ast::add_ls_num() %>%
  #mutate(wd = lubridate::wday(Date, week_start = 1)) %>%

  #filter(LS == "L", maDiff < 20) %>%
  #filter(!is.na(LS), maDiff > 15) %>%
  #mutate(pl = if_else(LS == "L", Close - Open, Open - Close)) %>%
  #mutate(sm = sum(pl), avg = sm / n()) %>%
  #group_by(LS,wd) %>% summarise(sm = sum(pl), n = n(), avg = sm/n, atr = mean(atr))
  #mutate(L_tr = if_else(LS=="L" & OL > maOL, "T", "")) %>%
  filter(maD_atr > 20, (maOL3/atr) > 0.5) %>%
  rowwise() %>% mutate(mxH = max(mxH1,mxH2,mxH3, na.rm = T)) %>%
  rowwise() %>% mutate(mnL = min(mnL1,mnL2,mnL3, na.rm = T)) %>%
  mutate(mxpl = mxH-Close,
         mxplatr = mxpl/atr*100) %>%
  ast::rnd_all() %>%
  select(1:7,atr,mxpl,mxplatr,trn,rn,LS,OL,maOL,maOL3,maD_atr,mxH,mnL) %>%
  View()

rr %>% group_by(Tick) %>% summarise(n = n(),Avg = mean(r_len), atr = mean(r_atr)) %>%
  ast::rnd_all() %>% View()


idata %>% ast::add_ATR() %>% mutate(OH = High-Open, OL = Open-Low) %>%
  mutate(maOH = TTR::runMean(OH,n=15), maOL = TTR::runMean(OL,n=15), Diff = abs(maOH-maOL)) %>%
  mutate(maDiff = TTR::runMean(Diff, n = 7)) %>%
  mutate(LS = if_else(maOH > maOL, "L", "S")) %>%
  mutate(wd = lubridate::wday(Date, week_start = 1)) %>%
  mutate(H3 = lead(High,3), L3 = lead(Low,3), C3 = lead(Close,3)) %>%
  mutate(pl3 = if_else(LS=="L", C3 - Open, Open - C3 )) %>% ast::rnd_all() %>%
  select(Date,Open,High,Low,Close,atr,LS,C3,pl3) %>%
  mutate(sm = sum(pl3, na.rm = T), avg = sm/n()) %>%
  View()


# gets data using quantmod
# there is a try wrapped around the getSymbols
try_get_stock_data <- function(sym, nm, daysback) {

  to_dt = today()
  fr_dt = to_dt - daysback
  x <- try( quantmod::getSymbols(sym, from = fr_dt, to = to_dt, src="yahoo", env=NULL) )

  if(length(x)>4){
    x <- x[,1:4]
    colnames(x) <- c('Open','High','Low','Close')
    x <- na.omit(x)
    x <- data.frame(Date=index(x), coredata(x)[,], stringsAsFactors = F)
    x$Tick <- nm
    x <- x %>% select(Tick,everything())
  } else {
    x <- NULL
  }
  return(x)
}

# Creates a result set
# takes output from try_get_stock_data
# Adds maOH, maOL to use as trend
# Calculates open date, price etc
ohl_res_set <- function(pdata){
  res <- pdata %>% ast::add_ATR() %>% mutate(OH = High-Open, OL = Open-Low) %>%
    mutate(maOH = TTR::runMean(OH,n=15), maOL = TTR::runMean(OL,n=15), Diff = abs(maOH-maOL)) %>%
    mutate(maDiff = TTR::runMean(Diff, n = 7)) %>%
    mutate(LS = if_else(maOH > maOL, "L", "S")) %>%
    ast::add_ls_num() %>% filter(trn != 0) %>%
    select(-prevLS,-OL,-OH,-maOL,-maOH, -tr) %>% group_by(trn) %>%
    mutate(r_OpenD = first(Date), r_CloseD = last(Date),
           r_OpenP = first(Open), r_CloseP = last(Close)) %>%
    mutate(r_pl = if_else(LS=="L", r_CloseP - r_OpenP, r_OpenP - r_CloseP),
           r_len = max(rn),
           r_atr = mean(atr)) %>%
    ungroup() %>% select(Tick,LS, starts_with("r_")) %>% distinct() %>% ast::rnd_all() %>%
    mutate(r_ar_pl = round(r_pl/r_atr,1))
}


# loops through all members of an Index e.e FTSE
# gets data then creates results set
# all individual results are joined together
loop_oh <- function(ftse100_df2){
  res_all <- NULL
  for(i in 1:91){
    res <- NULL
    id = i
    # browser()
    idata <-  try_get_stock_data(paste0(ftse100_df2$Ticker[id],".L"), ftse100_df2$Ticker[id], daysback = 400)
    if(!is.null(idata)){
      res <- ohl_res_set(idata)
    }
  }
  return(res_all)
}

# run the loop, for all FTSE members
rr <- loop_oh(ftse100_df)

# save the results
# saveRDS(rr, "/home/allanx38/ast/Data/OH_Trend_Trade_results.rds")
oh_results <- readRDS("/home/allanx38/ast/Data/OH_Trend_Trade_results.rds")


# bit of analysis
# Q: which tick have best trends? a. Count a. trends, b. Avg length
rr %>% group_by(Tick) %>% summarise(n = n(),Avg = mean(r_len)) %>% View()

rr %>% filter(Tick == "FERG")


# bit of analysis
rr %>% group_by(r_len) %>% summarise(n = n(), pl = sum(r_pl)) %>% mutate(run_ct = cumsum(n)) %>%
  mutate(sm = sum(n), runper = round(run_ct/sm * 100)) %>% View()

# bit more analysis
rr %>% group_by(Tick,LS) %>%
  summarise(sm = sum(r_pl),
            n = n(),
            n_sm = sm/n,
            atr = mean(r_atr)) %>%
  ast::rnd_all() %>%
  mutate(n_sm_atr = round(n_sm/atr,1)) %>%
  arrange(LS) %>% View()




##################### TA

to_dt = lubridate::today()
fr_dt = to_dt - 600
x <- quantmod::getSymbols("AZN.L", from = fr_dt, to = to_dt, src="yahoo", env=NULL)
x <- x[,1:4]
colnames(x) <- c('Open','High','Low','Close')
x <- na.omit(x)
x <- data.frame(Date=index(x), coredata(x)[,], stringsAsFactors = F)
x$Tick <- "AZN.L"
x <- x %>% select(Tick,everything())
sym <- x



ast::load_libs()
debugonce(get_yahoo_fin_tick_tc)
sym <- get_yahoo_fin_tick_tc("AZN.L","AZN.L",600)

# ADX
  sym_ta <- sym %>% oh_base()
  adx <- ADX(sym_ta[,c("High","Low","Close")])
  sym_ta <- bind_cols(sym_ta,adx) %>%
    mutate(maxDX = max(DX,na.rm = T), minDX = min(DX, na.rm = T))
# Aroon
  sym_ta <- sym %>% oh_base()
  sym_ta <- sym_ta %>% add_Aroon()
  sym_ta$aroon_tr <- ""
  sym_ta$aroon_tr <- ifelse(sym_ta$aroonUp >= 70 & sym_ta$aroonDn <= 30, "L", sym_ta$aroon_tr)
  sym_ta$aroon_tr <- ifelse(sym_ta$aroonDn >= 70 & sym_ta$aroonUp <= 30, "S", sym_ta$aroon_tr)

  sym_ta %>% select(1:6,10,OH_LS,aroon_tr,maD_atr,aroonUp,aroonDn,oscillator) %>%
    ast::rnd_all(1) %>% View()

# MACD
  sym_ta <- sym_ta %>% add_MACD("Close")

  sym_ta %>% select(1:6,10,OH_LS,maD_atr,macd,signal) %>%
    ast::rnd_all(1) %>% View()
  max(sym_ta$macd, na.rm = T)
  min(sym_ta$macd, na.rm = T)
  max(sym_ta$signal, na.rm = T)
  min(sym_ta$signal, na.rm = T)

# RSI
  sym_ta <- sym %>% oh_base()
  rsi <- TTR::RSI(sym_ta[,"Close"]) %>% as.data.frame()
  colnames(rsi) <- "rsi"
  sym_ta <- bind_cols(sym_ta,rsi)
  sym_ta %>% select(1:6,10,OH_LS,maD_atr,rsi) %>%
    ast::rnd_all(1) %>% View()


# Trend
  # CCI Commodity Channel Index
  # CTI Ehler’s Correlation Trend Indicator

# Volatility
  # chaikinVolatility Chaikin Volatility
  # Chaikin Volatility measures the rate of change of the security’s trading range.

# Oscillator
  # CLV Close Location Value
  # CMO Chande Momentum Oscillator
  # DPO De-Trended Price Oscillator
  # DVI DV Intermediate Oscillator



sym_ta %>% select(1:6,10,maD_atr,DIp,DIn,DX,ADX,maxDX,minDX) %>%
  ast::rnd_all(1) %>%
  #arrange(desc(maD_atr)) %>%
  View()

tx <- temp()


  to_dt = lubridate::today()
  fr_dt = to_dt - 200
  #x <- NULL
  rm(x)
  tryCatch(
    {
      x <- quantmod::getSymbols("SGE.L", from = fr_dt, to = to_dt, src="yahoo", env=NULL)
    },
    warning = function(w){
      message("A warning occurred")
      print(w)
    },
    error = function(e){
      message("An error occurred")
      print(e)
    }
  )

  rm(x)
  x <- ast::get_yahoo_fin_tick_tc("SGE.L","AAL.L",600)



  library(logr)
  log <- "/home/allanx38/ast/Data/test.log"


debugonce(update_single_file)
update_single_file("WTB.L.csv")

debugonce(get_yahoo_fin_tick_tc)
ast::get_yahoo_fin_tick_tc(sym = "WTB.L",nm = "WTB.L",daysback = 50)

 update_single_file <- function(sym_p,daysback = 100){

      x <- NULL
      tdata <- NULL
      dt <- NULL
      x <- read.csv( paste0("/home/allanx38/ast/Data/FTSE_Data/", sym_p) )
      if(!is.null(x)){
        # read data from Yahoo
        ticker <- stringr::str_sub(ftse_file_names[i],0,-5)
        dt <- ast::get_yahoo_fin_tick_tc(sym = sym_p,nm = sym_p,daysback = 50)
        if(!is.null(dt)){
          dt <- dt %>% select(Date,Open,High,Low,Close,Tick)
          dt$Date <- as.character(dt$Date)
          # read data saved on disc
          cs <- read.csv( paste0("/home/allanx38/ast/Data/FTSE_Data/", ftse_file_names[i]) )
          # combine rows and save
          tdata <- bind_rows(cs,dt %>% filter(!Date %in% cs$Date)) %>% distinct()
          readr::write_csv(tdata, paste0("/home/allanx38/ast/Data/FTSE_Data/", ftse_file_names[i]))
        }
    }
}


# GG Plots ----------------------------------------------------------------

 library(ggplot2)
 ftse_sym <- read.csv("/home/allanx38/ast/Data/FTSE_Data/WPP.L.csv")

 ftse_sym %>%
   ggplot(aes(x = Date, y = Close)) +
   geom_line(

   ) +
   labs(title = "AAPL Line Chart", y = "Closing Price", x = "")
   --theme_tq()

 ftse_sym %>%
   ggplot(aes(x = date, y = close)) +
   geom_barchart(aes(open = open, high = high, low = low, close = close)) +
   labs(title = "AAPL Bar Chart", y = "Closing Price", x = "") +
   theme_tq()



