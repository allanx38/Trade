

# Update FTSE 100
  ast::load_libs()
  ast::update_ftse_tick_saved()
  read.csv("/home/allanx38/ast/Data/FTSE_Data/HL.L.csv") %>% tail(n=10)

# Current
  ftse100 <- NULL
  ftse100 <- run_m_25_150_fnc(tail_num = 500)

# Today
  rday <- "2024-03-08"

# Long trend - Reverse Short
  sht_rev <- NULL
  sht_rev <- ftse100 %>% filter(Date==rday) %>%
    filter_rev_sht()
  sht_rev <- sht_rev %>%
    select_rev_sht() %>%
    mutate(ShtOpenPr = Close + (atr*0.75)) %>%
    arrange(desc(O_ma25a))
  sht_rev

# Short trend - Reverse Long
  lng_rev <- NULL
  lng_rev <-ftse100 %>% filter(Date>=rday) %>%
    filter_rev_lng()
  lng_rev <- lng_rev %>%
    select_rev_lng() %>%
    mutate(LngOpenPr = Close - (atr*0.75)) %>%
    arrange(desc(ma25_Oa))
  lng_rev

# Pull Back in trend
# Long
  lng_pb <- NULL
  lng_pb <-ftse100 %>% filter(Date==rday) %>%
    filter_lng() %>% adorn_rounding()
  lng_pb %>% select(LS,1:6,atr,rmxH5,rmxH5Ca,rmxH20Ca,rn)

  #ftse100 %>% filter(Tick=="GSK.L", Date >= rday) %>% mutate(OpenP = Open - (atr*0.5)) %>%
  #  select(LS,1:6,atr,OpenP,rmxH5,rmxH5Ca,rmxH20Ca) %>% tail() %>% adorn_rounding()

# Short
  sht_pb <- NULL
  sht_pb <-ftse100 %>% filter(Date==rday) %>%
    filter_sht() %>% adorn_rounding()
  sht_pb %>% select(LS,1:6,atr,rmnL5C,rmnL5Ca,rmnL20Ca,rn)

  #ftse100 %>% filter(Tick=="BATS.L", Date >= rday) %>% mutate(OpenP = Open + (atr*0.5)) %>%
  #  select(LS,1:6,atr,OpenP,rmnL5C,rmnL5Ca,rmnL20Ca) %>% tail() %>% adorn_rounding()
