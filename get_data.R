

# FTSE 100 ----------------------------------------------------------------

get_ftse_ticks <- function(){
  ftse_ticks <- list.files(path="/home/allanx38/ast/Data/FTSE_Data")
  ftse_ticks <- as.data.frame(ftse_ticks)
  ftse_ticks <- ftse_ticks %>% rename(csv = ftse_ticks)
  ftse_ticks$ftse_tick <- stringr::str_sub(ftse_ticks$csv, 0,-5)
  return(ftse_ticks)
}


get_ftse_csv_file <- function(csv_nm){
  read.csv( paste0("/home/allanx38/ast/Data/FTSE_Data/", csv_nm  ) )
}


# Dow ----------------------------------------------------------------

get_dow_ticks <- function(){
  dow_ticks <- list.files(path="/home/allanx38/ast/Data/Dow_Data")
  dow_ticks <- as.data.frame(dow_ticks)
  dow_ticks <- dow_ticks %>% rename(csv = dow_ticks)
  dow_ticks$dow_tick <- stringr::str_sub(dow_ticks$csv, 0,-5)
  return(dow_ticks)
}


get_dow_csv_file <- function(csv_nm){
  read.csv( paste0("/home/allanx38/ast/Data/Dow_Data/", csv_nm  ) )
}



# Dax ----------------------------------------------------------------

get_dax_ticks <- function(){
  dow_ticks <- list.files(path="/home/allanx38/ast/Data/Dax_Data")
  dow_ticks <- as.data.frame(dow_ticks)
  dow_ticks <- dow_ticks %>% rename(csv = dow_ticks)
  dow_ticks$dow_tick <- stringr::str_sub(dow_ticks$csv, 0,-5)
  return(dow_ticks)
}


get_dax_csv_file <- function(csv_nm){
  read.csv( paste0("/home/allanx38/ast/Data/Dax_Data/", csv_nm  ) )
}

# SP500 ----------------------------------------------------------------

get_sp500_ticks <- function(){
  dow_ticks <- list.files(path="/home/allanx38/ast/Data/SP500_Data")
  dow_ticks <- as.data.frame(dow_ticks)
  dow_ticks <- dow_ticks %>% rename(csv = dow_ticks)
  dow_ticks$dow_tick <- stringr::str_sub(dow_ticks$csv, 0,-5)
  return(dow_ticks)
}


get_sp500_csv_file <- function(csv_nm){
  read.csv( paste0("/home/allanx38/ast/Data/SP500_Data/", csv_nm  ) )
}





#' qm_gedata_df
#'
#' Gets data using Quantmod, returns
#' Defaults to Yahoo
#' Defaults to today going back 200 days a DATA FRAME
#'
#' @param sym_p = yahoo symbol, no default
#' @param src_p = source defaults to Yahoo
#' @param to_dt_p = when data ends, default is today
#' @param days_back_p  = days back (subtracted from to date) default is 200 days ago
#'
#' @return
#' @export
#'
qm_gedata_df <- function(sym_p,
                         src_p = "yahoo",
                         to_dt_p = lubridate::today(),
                         days_back_p = 200){

  # set dates
  from_date = to_dt_p - days_back_p

  x <- getSymbols(sym_p, src=src_p,  from = from_date, to = to_dt_p, env=NULL)
  x <- x[,1:4]
  colnames(x) <- c('Open','High','Low','Close')
  x <- na.omit(x)
  x <- data.frame(Date=index(x), coredata(x)[,], stringsAsFactors = F)
  x$Tick <- sym_p
  return(x)
}


#' get_yahoo_fin_tick
#'
#' Gets data using quantmod, returns data frame
#' Renames column names
#'
#' @param sym
#' @param nm
#' @param daysback
#'
#' @return
#' @export
#'
#'
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



#' get_yahoo_fin_tick_tc
#'
#' Same as get_yahoo_fin_tick, but wrapped in a try catch ...
#'
#' @param sym
#' @param nm
#' @param daysback
#'
#' @return
#' @export
#'
#'
get_yahoo_fin_tick_tc <- function(sym,nm,daysback){
  to_dt = lubridate::today()
  fr_dt = to_dt - daysback
  x <- NULL

  tryCatch(
    {
      x <- quantmod::getSymbols(sym, from = fr_dt, to = to_dt, src="yahoo", env=NULL)
    },
      warning = function(w){
      message('A warning occurred')
      print(w)
    },
      error = function(e){
      message('An error occurred')
      print(e)
      x <- NULL
    }
  )

  if(!is.null(x)){
    x <- x[,1:4]
    colnames(x) <- c('Open','High','Low','Close')
    x <- na.omit(x)
    x <- data.frame(Date=index(x), coredata(x)[,], stringsAsFactors = F)
    x$Tick <- nm
    x <- x %>% select(Tick,everything())
    return(x)
  }
}



#' save_qm_data_df
#'
#' Gets data from quantmod
#' SAves it to disk ...
#'
#' @param sym_p
#' @param source_p
#' @param to_date_p
#' @param days_backwards_p
#' @param csv_dir_and_nm
#'
#' @return
#' @export
#'
#'
save_qm_data_df <- function(sym_p,
                            source_p = "yahoo",
                            to_date_p = lubridate::today(),
                            days_backwards_p = 200,
                            csv_dir_and_nm){

  data_set <- NULL
  data_set <- try(qm_gedata_df(sym_p, src_p = source_p, to_dt_p = to_date_p, days_back_p = days_backwards_p))
  if(!is.null(data_set)){
    write_csv(data_set, csv_dir_and_nm)
  }

}

