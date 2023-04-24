

# Add Base Trend ----------------------------------------------------------


#' oh_base
#'
#' days_back used in future Close value and High, Lows
#'
#' @param pdata
#' @param days_back
#'
#' @return
#' @export
#'
#'
oh_base <- function(pdata, days_back = 3){
  p_res <- pdata %>%
    mutate(nOpen = lead(Open)) %>%
    mutate(Close3 = lead(Close,n=days_back)) %>%
    ast::add_ATR() %>%
    mutate(OH = High-Open,
           OL = Open-Low) %>%

    mutate(maOH = TTR::runMean(OH,n=15),
           maOH3 = TTR::runMean(OH,n=3),
           maOL = TTR::runMean(OL,n=15),
           maOL3 = TTR::runMean(OL,n=3),
           Diff = (maOH-maOL)) %>%

    # this is the trend
    mutate(maDiff = TTR::runMean(Diff, n = 7),
           maD_atr = maDiff/atr*100)

  p_res$OH_LS <- ""
  p_res$OH_LS <- ifelse(p_res$maD_atr >= 20, "L", p_res$OH_LS)
  p_res$OH_LS <- ifelse(p_res$maD_atr <= -20, "S", p_res$OH_LS)

  p_res <- p_res %>%
    mutate(LS = if_else(maOH > maOL, "L", "S")) %>%
    #mutate(LS = if_else(maDiff > 0, "L", "S")) %>%

    ast::add_ls_num() %>%
    #add_oh_ls_num() %>%
    mutate(mxH = future_value(High, days_back, NA, max)) %>%
    mutate(mnL = future_value(Low, days_back, NA, min))
    #ast::rnd_all()

  p_res <- as.data.frame(p_res)
  return(p_res)
}



#' add_oh_ls_num
#' Adds trade # (trn) and indv row # (rn)
#' based on OH_LS - added to data set via oh_base fnc above
#'
#' @param f_data
#'
#' @return
#' @export
#'
#'
add_oh_ls_num <- function(f_data){
  rn_num <- 0
  tr_num <- 0
  f_data$rn  <- 0
  f_data$trn <- 0
  for(i in 2:nrow(f_data)){
    f_data$prevOH_LS <- lag(f_data$OH_LS)
    if(!is.na(f_data$prevOH_LS[i])){
      if(f_data$prevOH_LS[i] != f_data$OH_LS[i]){
        #browser()
        rn_num <- 1
        tr_num <- tr_num + 1
      } else {
        rn_num <- rn_num + 1
      }
    }
    f_data$rn[i] <- rn_num
    f_data$trn[i] <- tr_num
  }
  return(f_data)
}




#' update_ftse_tick_saved
#'
#' Loops through a list of file names, for each one downloads latest data
#' Combines with exisitng data and saves
#'
#' @return
#' @export
#'
#'
update_ftse_tick_saved <- function(){
  ftse_file_names <- list.files(path="/home/allanx38/ast/Data/FTSE_Data")
  for(i in 1 : length(ftse_file_names)){
    x <- NULL
    tdata <- NULL
    dt <- NULL
    x <- read.csv( paste0("/home/allanx38/ast/Data/FTSE_Data/", ftse_file_names[i]) )
    if(!is.null(x)){
      # read data from Yahoo
      ticker <- stringr::str_sub(ftse_file_names[i],0,-5)
      #dt <- ast::get_yahoo_fin_tick_tc(sym = ticker,nm = ticker,daysback = 50)
      try(dt <- get_yahoo_fin_tick(sym = ticker,nm = ticker, daysback = 50))
      if(!is.null(dt)){
        dt <- dt %>% select(Date,Open,High,Low,Close,Tick)
        dt$Date <- as.character(dt$Date)
        # read data saved on disc
        cs <- read.csv( paste0("/home/allanx38/ast/Data/FTSE_Data/", ftse_file_names[i]) )
        # remove 0 values ...
        cs <- cs %>% filter(Open != 0)
        # combine rows and save
        tdata <- bind_rows(cs,dt %>% filter(!Date %in% cs$Date)) %>% distinct()
        readr::write_csv(tdata, paste0("/home/allanx38/ast/Data/FTSE_Data/", ftse_file_names[i]))
      }
    }
  }
}



#' todays_lng_oh_trades
#'
#' @param lng_tick_vec
#'
#' @return
#' @export
#'
#' @examples
todays_lng_oh_trades <- function(lng_tick_vec, maD_num = 20, maOL3_num = 0.5){

  for(i in 1 : length(lng_tick_vec)){
    x <- NULL
    x <- read.csv( paste0("/home/allanx38/ast/Data/FTSE_Data/", lng_tick_vec[i], ".csv") )
    print(x$Tick[1])
    if(!is.null(x)){
      x <- x %>% filter(Date > '2021-01-01') %>% oh_base()
      last_row <- nrow(x)
      if(x$LS[last_row]=="L" & x$maD_atr[last_row] >= maD_num & (x$maOL3[last_row]/x$atr[last_row]) >= maOL3_num) {
        print("---------------")
        print(x$Tick[1])
        print( x %>% select(1:6,atr,LS,maOH,OL,maOL,maOL3,maD_atr,maDiff) %>% tail(n=15) )
      }
    }
  }
}



#' todays_sht_oh_trades
#'
#' @param sht_tick_vec
#'
#' @return
#' @export
#'
#' @examples
todays_sht_oh_trades <- function(sht_tick_vec,maD_num = -20, maOH3_num = 0.5){

  for(i in 1 : length(sht_tick_vec)){
    x <- NULL
    x <- read.csv( paste0("/home/allanx38/ast/Data/FTSE_Data/", sht_tick_vec[i], ".csv") )
    print(x$Tick[1])
    if(!is.null(x)){
      x <- x %>% filter(Date > '2021-01-01') %>% oh_base()
      last_row <- nrow(x)
      if(x$LS[last_row]=="S" & x$maD_atr[last_row] <= maD_num & (x$maOH3[last_row]/x$atr[last_row]) >= maOH3_num){
        print("---------------")
        print(x$Tick[1])
        print( x %>% select(1:6,atr,LS,maOL,OH,maOH,maOH3,maD_atr,maDiff) %>% tail(n=10) )
      }
    }
  }
}





# Trading -----------------------------------------------------------------


#' oh_trade1_lng_summary
#' pdata <- then summarizes data grouped by Tick,trn
#'
#'
#' @param pdata - tick data plus oh_base
#'
#' @return
#' @export
#'
#' @examples
oh_trade1_lng_summary <- function(pdata){
  pdata %>%
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
              matr = if_else(mpl>atr,atr,cpl), .groups = "drop") %>%
    group_by() %>%
    mutate(sm_cpl = sum(cpl), sm_mpl = sum(matr))
}



#' oh_trade1_sht_summary
#'
#' @param pdata - tick data plus oh_base
#'
#' @return
#' @export
#'
#' @examples
oh_trade1_sht_summary <- function(pdata){
  pdata %>%
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
              matr = if_else(mpl>atr,atr,cpl),.groups = "drop") %>%
    group_by() %>%
    mutate(sm_cpl = sum(cpl), sm_mpl = sum(matr))
}




#' loop_thru_ftse_oh
#'
#' @param start_loop
#' @param days_back
#'
#' @return
#' @export
#'
loop_thru_ftse_oh <- function(start_loop = 1, days_back = 3){
  ftse_file_names <- list.files(path="/home/allanx38/ast/Data/FTSE_Data")
  res_all <- NULL
  for(i in start_loop : length(ftse_file_names)){
    x <- NULL
    x <- read.csv( paste0("/home/allanx38/ast/Data/FTSE_Data/", ftse_file_names[i]) )
    if(!is.null(x)){
      # Add base details
      res <- oh_base(x, days_back)
      res_all <- bind_rows(res_all,res)
    }
  }
  return(res_all)
}




#' oh_long_pb
#' filters the base data - maD_atr and maOL3/atr
#'
#' @param pdata
#' @param maD_filter
#'
#' @return
#' @export
#'
oh_long_pb <- function(pdata,maD_filter = 20){
  pdata %>%
    filter(maD_atr > maD_filter,
           (maOL3/atr) > 0.5) %>%

    mutate(mxpl = mxH-Close,
           mxplatr = mxpl/atr*100) %>%

    mutate(clpl = Close3 - Close) %>%

    mutate(fpl = if_else(mxplatr>100,atr,clpl)) %>%

    ast::rnd_all() %>%

    select(1:8,atr,fpl,clpl,mxpl,mxplatr,trn,rn,LS,OL,maOL,maOL3,maD_atr,mxH,mnL)
}



oh_short_pb <- function(pdata,maD_filter = -20){
  pdata %>%
    filter(maD_atr < maD_filter,
           (maOH3/atr) > 0.5) %>%

    mutate(mxpl = Close - mnL,
           mxplatr = mxpl/atr*100) %>%

    mutate(clpl = Close - Close3) %>%

    mutate(fpl = if_else(mxplatr>100,atr,clpl)) %>%

    ast::rnd_all() %>%

    select(1:8,atr,fpl,clpl,mxpl,mxplatr,trn,rn,LS,OH,maOH,maOH3,maD_atr,mxH,mnL)
}



#' future_value
#'
#' gets a value, min, max etc in next few values
#'
#' @param invec
#' @param n
#' @param pad_val
#' @param FUN     a function such as max
#'
#' @return
#' @export
#'
#'
future_value <- function(invec, n = 3, pad_val = NA, FUN = sum) {
  FUN <- match.fun(FUN)
  apply(embed(c(invec[-1], rep(pad_val, n)), n), 1, {
    function(x) if (all(is.na(x))) NA else FUN(x[!is.na(x)])
  })
}


