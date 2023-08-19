ast::load_libs()
library(quantmod)
library(lubridate)
library(ggplot2)

qm_get_data <- function(sym, days_back){
  to_dt = today()
  from_dt = to_dt - days_back
  Data <- getSymbols(sym, src="yahoo",
                     from = from_dt,to = to_dt, env=NULL)
  return(Data)
}

# wrap chartSeries function and add Bolinger Band
qm_candle_macd_bb <- function(pData,data_name,sset){
  chartSeries(pData,
              type="candlesticks",
              subset=sset,
              theme=chartTheme('white'),
              TA = 'addMACD()',
              name = data_name)
  addBBands()
}

# get data and chart
qm_get_data_candle_macd_bb <- function(sym,days_back,sset){
  Data <- qm_get_data(sym,days_back)
  qm_candle_macd_bb(Data,sym,sset)
}


qm_candle_macd_ma <- function(pData,data_name,sset,sma_num = 150){
  chartSeries(pData,
              type="candlesticks",
              subset=sset,
              theme=chartTheme('white'),
              TA = 'addMACD()',
              name = data_name)
  addSMA(sma_num)
}

get_yahoo_data <- function(sym,nm,daysback){
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

gplot_pf <- function(br, g_title = ""){
  ggplot(br, aes(x=Date)) +
    geom_line(aes(y = Close), group = 1) +
    scale_x_date(date_breaks = "3 month", date_labels = "%y%m") +
    theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
    ggtitle(g_title) +
    theme(panel.border = element_rect(fill = NA)) +
    geom_line(aes(y = ma150), group = 1, color = "blue") +
    geom_text(data = br %>% filter(Date == max(Date)),
              aes(y = ma150, label = ma150_label),
              hjust = 0, nudge_x = 0.1) +
    # Allow labels to bleed past the canvas boundaries
    coord_cartesian(clip = 'off') +
    # Remove legend & adjust margins to give more space for labels
    # Remember, the margins are t-r-b-l
    theme(legend.position = 'none',
          plot.margin = margin(0.1, 0.5, 0.1, 0.1, "cm"))
}

get_data_plot_150 <- function(tck, daysback = 1500, nm){
  rmove <- get_yahoo_data(tck, tck, daysback)
  # add ma150
  rmove <- rmove %>% mutate(ma150 = TTR::runMean(Close, n=150),
                            ma150_label = "ma150")
  # plot
  gplot_pf(rmove, nm)
}



# BARC.L 275 shares cost €533 = €1.93 approx £1.80?
read.csv("/home/allanx38/ast/Data/FTSE_Data/BARC.L.csv") %>% tail()

# BRITVIC 60 shares €539 BVIC.L
  br <- get_yahoo_data("BVIC.L", "BVIC.L", 1500)
  # fix duff point
  br[which(br$Date=="2022-06-14"),"Close"] <-
    br[which(br$Date=="2022-06-13"),"Close"]
  # add ma150
  br <- br %>% mutate(ma150 = TTR::runMean(Close, n=150),
                      ma150_label = "ma150")
  # plot
  gplot_pf(br)


# BURBERRY GROUP - 25 Shares - 607 euro - approx 2100 open
  bur <- read.csv("/home/allanx38/ast/Data/FTSE_Data/BRBY.L.csv")
  bur <- bur %>% mutate(ma150 = TTR::runMean(Close, n=150),
                      ma150_label = "ma150")
  bur$Date <- as.Date(bur$Date)
  # plot
  gplot_pf(bur, "BURBERRY")


# DOMINO PIZZA 175 shares €568
  domino <- get_yahoo_data("DOM.L", "DOM.L", 1500)
  # add ma150
  domino <- domino %>% mutate(ma150 = TTR::runMean(Close, n=150),
                      ma150_label = "ma150")
  # plot
  gplot_pf(domino, "DOMINO")


# RIGHTMOVE 105 shares €581 - RMV.L
  rmove <- get_yahoo_data("RMV.L", "RMV.L", 1500)
  # add ma150
  rmove <- rmove %>% mutate(ma150 = TTR::runMean(Close, n=150),
                              ma150_label = "ma150")
  # plot
  gplot_pf(rmove, "RIGHTMOVE")

# STANDARD Chart 80 shares €553 - STAN.L
  st_ch <- get_yahoo_data("STAN.L", "STAN.L", 1500)
  # add ma150
  st_ch <- rmove %>% mutate(ma150 = TTR::runMean(Close, n=150),
                            ma150_label = "ma150")
  # plot
  gplot_pf(st_ch, "STANDARD CHARTERED")


# Superdry 45 shares €601 SDRY.L
  sdry <- get_yahoo_data("SDRY.L", "SDRY.L", 1500)
  # add ma150
  sdry <- sdry %>% mutate(ma150 = TTR::runMean(Close, n=150),
                            ma150_label = "ma150")
  # plot
  gplot_pf(sdry, "Superdry")



# ETF ---------------------------------------------------------------------

  iShares Core MSCI World UCITS ETF USD (Acc) (IWDA.L)
  iShares Core DAX UCITS ETF (DE) (EXS1.DE)
  iShares Core S&P 500 ETF (IVV)

  get_data_plot_150("IVV", 3000, "iShares Core S&P 500 ETF")
