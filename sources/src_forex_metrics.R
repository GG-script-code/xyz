
{
  param_D1_SMA<-readxl::read_xlsx(file.path(path_parameters, "parameters_metrics_D1.xlsx"), sheet="SMA")
  param_D1_EMA<-readxl::read_xlsx(file.path(path_parameters, "parameters_metrics_D1.xlsx"), sheet="EMA")
  param_D1_MACD<-readxl::read_xlsx(file.path(path_parameters, "parameters_metrics_D1.xlsx"), sheet="MACD")
  param_D1_RSI<-readxl::read_xlsx(file.path(path_parameters, "parameters_metrics_D1.xlsx"), sheet="RSI")
  param_D1_STOCH<-readxl::read_xlsx(file.path(path_parameters, "parameters_metrics_D1.xlsx"), sheet="STOCH")
  param_D1_BBands<-readxl::read_xlsx(file.path(path_parameters, "parameters_metrics_D1.xlsx"), sheet="BBands")
  param_D1_ADX<-readxl::read_xlsx(file.path(path_parameters, "parameters_metrics_D1.xlsx"), sheet="ADX")
  
  param_metrics_D1<-list("SMA"=param_D1_SMA
                      ,"EMA"=param_D1_EMA
                      ,"MACD"=param_D1_MACD
                      ,"RSI"=param_D1_RSI
                      ,"STOCH"=param_D1_STOCH
                      ,"BBands"=param_D1_BBands
                      ,"ADX"=param_D1_ADX)
}


# Compute FOREX metrics ----
fnc_metrics_FX_Data<-function(path_Input=path_input_tqGet,path_Output=path_output_metrics,overwrite_Metrics=TRUE, param_Metrics_List=list("D1"=param_metrics_D1)){
  # path_Input=path_input_tqGet; path_Output=path_output_metrics; overwrite_Metrics=TRUE; param_Metrics_List=list("D1"=param_metrics_D1)
  
  for(Periodicity in c("daily")){
    # Periodicity<-"daily"
    
    if(Periodicity=="daily"){
      param_Metrics<-param_Metrics_List$D1
    }
    
    # list FOREX data DOWNLOADED
    FX_Data_List<-list.files(file.path(path_Input,Periodicity),pattern=".feather$")
    # list FOREX metrics COMPUTED
    FX_Metrics_List<-list.files(file.path(path_Output,Periodicity),pattern=".feather$")
    
    if(overwrite_Metrics){
      FX_Metrics_List<-paste0(FX_Metrics_List, "_overwrite")
    }
    
    pb<-progress_bar$new(total=(length(FX_Data_List)))
    
    for(FX_Dataset in FX_Data_List){
      # FX_Dataset<-FX_Data_List[str_detect(FX_Data_List,"USDCAD")%>%which()]
      
      print(FX_Dataset)
      
      # Upload the data for the FX symbol
      FX_Data<-read_feather(file.path(path_Input,Periodicity,FX_Dataset))
      # Initialize the name of the new FX metrics file
      FX_Metrics_File<-paste0(max(FX_Data$date)%>%str_remove_all("-")
                              ,"_",FX_Dataset)
      
      # If the new file is not in the FX matrics list,compute the metrics with the new data
      if(FX_Metrics_File%notin%FX_Metrics_List){
        
        FX_symbol<-unique(FX_Data$symbol)
        
        n_SMA<-param_Metrics$SMA%>%
          filter(symbol==FX_symbol)
        n_EMA<-param_Metrics$EMA%>%
          filter(symbol==FX_symbol)
        n_MACD<-param_Metrics$MACD%>%
          filter(symbol==FX_symbol)
        n_RSI<-param_Metrics$RSI%>%
          filter(symbol==FX_symbol)
        n_STOCH<-param_Metrics$STOCH%>%
          filter(symbol==FX_symbol)
        n_BBands<-param_Metrics$BBands%>%
          filter(symbol==FX_symbol)
        n_ADX<-param_Metrics$ADX%>%
          filter(symbol==FX_symbol)
        
        # Compute the metrics
        FX_Data<-FX_Data%>%
          filter_at(vars(open,high,low,close,adjusted),any_vars(!is.na(.)))%>%
          #> Trend-Following: SMA, EMA, MACD, Ichimoku ----
          #> Strategia	Short SMA	Long SMA	Adatta a
          #> Classica	50	200	Trend-Following lento
          #> Reattiva	20	100	Trend-Following più veloce
          #> Ibrida	10	50	Forex ad alta volatilità
          #' SMA
          tq_mutate(select=c("close")
                  ,mutate_fun=SMA,n=n_SMA$short_sma
                  ,col_rename="SMA_veloce")%>%
          tq_mutate(select=c("close")
                    ,mutate_fun=SMA,n=n_SMA$long_sma
                    ,col_rename="SMA_lento")%>%
          #' EMA
          tq_mutate(select=c("close")
                    ,mutate_fun=EMA,n=n_EMA$short_ema
                    ,col_rename="EMA_veloce")%>%
          tq_mutate(select=c("close")
                    ,mutate_fun=EMA,n=n_EMA$long_ema
                    ,col_rename="EMA_lento")%>%
          #' MACD
          tq_mutate(select=c("close")
                    ,mutate_fun=MACD,maType=SMA
                    ,nFast=n_MACD$macd_nF,nSlow=n_MACD$macd_nSl,nSig=n_MACD$macd_nSi
                    ,col_rename=c("macd", "macd_signal")
                    ,percent=TRUE)%>%
          mutate(macd_histo=macd-macd_signal)%>%
          #' Ichimoku
          fun_ichimoku()%>%
          select(-c(nine_period_high,nine_period_low
                    ,period26_high,period26_low
                    ,period52_high,period52_low))%>%
          #> Momentum: RSI, Stochastic, Williams %R ----
          #' Relative Strenght Index
          tq_mutate(select=close
                    ,mutate_fun=RSI,n=n_RSI$rsi_n
                    ,col_rename="RSI")%>%
          #' Stochastic Oscillator
          tq_mutate(select=c("high","low","close")
                    ,mutate_fun=stoch,maType=SMA
                    ,nFastK=n_STOCH$stoch_nFk
                    ,nFastD=n_STOCH$stoch_nFd
                    ,nSlowD=n_STOCH$stoch_nSd
                    ,smooth=n_STOCH$stoch_smooth)%>%
          #' Williams %R
          tq_mutate(select=c("high", "low", "close")
                    , mutate_fun=WPR, n=14
                    , col_rename="Williams %R")%>%
          #> #Volatilità: ATR, Bollinger Bands, Donchian Channels ----
          #' ATR
          tq_mutate(select=c("high", "low", "close")
                    , mutate_fun=ATR
                    , n=14
                    , col_rename=c("tr", "atr", "atr_UP", "atr_DW")
          )%>%
          mutate(atr_quant75=rollapply(atr
                                       , width=360
                                       , FUN=function(x){quantile(x, probs=0.75, na.rm=TRUE)}
                                       , fill=NA
                                       , align="right"
                                       , partial=TRUE))%>%
          mutate(atr_quant25=rollapply(atr
                                       , width=360
                                       , FUN=function(x){quantile(x, probs=0.25, na.rm=TRUE)}
                                       , fill=NA
                                       , align="right"
                                       , partial=TRUE))%>%
          #' Bollinger Bands
          tq_mutate(select=close
                    ,mutate_fun=BBands,maType=SMA
                    ,n=n_BBands$BB_n
                    ,sd=n_BBands$BB_sd)%>%
          rename("BB_dn"="dn"
                 ,"BB_mavg"="mavg"
                 ,"BB_up"="up"
                 ,"BB_pctB"="pctB")%>%
          mutate(BB_Width=BB_up-BB_dn)%>%
          mutate(BB_Width_quant75=rollapply(BB_Width
                                            , width=360
                                            , FUN=function(x){quantile(x, probs=0.75, na.rm=TRUE)}
                                            , fill=NA
                                            , align="right"
                                            , partial=TRUE))%>%
          mutate(BB_Width_quant25=rollapply(BB_Width
                                            , width=360
                                            , FUN=function(x){quantile(x, probs=0.25, na.rm=TRUE)}
                                            , fill=NA
                                            , align="right"
                                            , partial=TRUE))%>%
          #' Donchian Channels
          tq_mutate(select=c("high", "low")
                    ,mutate_fun=DonchianChannel
                    ,n=20
          )%>%
          rename("Donchian_High"="DonchianChannel"
                 ,"Donchian_Middle"="mid"
                 ,"Donchian_Low"="DonchianChannel..1")%>%
          #' Volatility
          tq_mutate(select=c("open","high","low","close")
                    ,mutate_fun=volatility,maType=SMA
                    ,n=10,calc="yang.zhang",N=260,mean0=FALSE
                    ,col_rename="volatility")%>%
          #> Volume-Based: OBV, Chaikin Money Flow ----
          #> Others ----
          #' Average Directional Movement Index
          tq_mutate(select=c("high","low","close")
                    ,mutate_fun=ADX,maType=SMA
                    ,n=n_ADX$adx_n)%>%
          drop_na()
        
        write_feather(FX_Data, file.path(path_Output,Periodicity,FX_Metrics_File))
        
      }else{
        # The FX metrics are updated and I have to update the FX data file
        if(FX_Metrics_File%in%FX_Metrics_List){
          message("METRICS: the latest computed dataset is the ",FX_Metrics_File," !!")
        }
      }
      
      #remove old metrics if the Max date in the FX data is bigger than the old one
      {
        old_FX_File_List<-FX_Metrics_List[str_detect(FX_Metrics_List,FX_Dataset)%>%which()]
        
        older_FX_File_List<-old_FX_File_List[(old_FX_File_List<FX_Metrics_File)%>%which()]
        
        if(length(older_FX_File_List)>0){
          
          for(old_FX_File in older_FX_File_List){
            
            old_FX_File_Date<-old_FX_File%>%str_extract("[0-9]{8}")%>%ymd()
            
            if(max(FX_Data$date)>ymd(old_FX_File_Date)){
              file.remove(file.path(path_Output,Periodicity,old_FX_File%>%str_remove_all("_overwrite")))
              message(old_FX_File," removed \1\n")
            }else{
              message("NO older file in the metrics")
            }
          }
        }
      }
      pb$tick()
    }
  }
  
  return("FX metrics updated!")
  
}

# fnc_metrics_FX_Data()


# Compute FOREX correlation ----
fnc_correlation_FX_Data<-function(path_Input=path_input_tqGet,path_Output=path_output_metrics){
  # path_Input=path_input_tqGet; path_Output=path_output_metrics
  
  for(Periodicity in c("daily")){
    # Periodicity<-"daily"
    
    # list FOREX data DOWNLOADED
    FX_Data_List<-list.files(file.path(path_Input,Periodicity),pattern=".*Rds")
    
    # list FOREX data DOWNLOADED
    FX_Data_List<-list.files(file.path(path_Input,Periodicity),pattern=".*Rds")
    
    df_Dates<-tibble(date=as.Date(NA))
    
    for(FX_Dataset in FX_Data_List){
      #FX_Dataset<-FX_Data_List[str_detect(FX_Data_List,"CADCHF")%>%which()]
      
      # Upload the data for the FX symbol
      FX_Data<-read_feather(file.path(path_Input,Periodicity,FX_Dataset))
      
      df_Dates<-bind_rows(df_Dates,FX_Data["date"])
    }
    
    df_Dates<-df_Dates%>%
      filter(!date%>%is.na())%>%
      group_by(date)%>%
      summarise(n=n())
    
    if(Periodicity=="daily"){
      from_date_filter<-(Sys.Date()%m-%months(6))
    }else if(Periodicity=="weekly"){
      from_date_filter<-(Sys.Date()%m-%months(24))
    }else if(Periodicity=="monthly"){
      from_date_filter<-(Sys.Date()%m-%months(48))
    }
    
    n_flt<-c((length(FX_Data_List)-2):length(FX_Data_List))
    
    df_Dates_flt<-df_Dates%>%
      filter(date>=from_date_filter)%>%
      filter(n%in%n_flt)%>%
      select(date)
    
    for(FX_Dataset in FX_Data_List){
      #FX_Dataset<-FX_Data_List[str_detect(FX_Data_List,"CADCHF")%>%which()]
      
      # Upload the data for the FX symbol
      FX_Data<-read_feather(file.path(path_Input,Periodicity,FX_Dataset))
      
      df_Dates_flt<-df_Dates_flt%>%
        #na.omit()%>%
        left_join(FX_Data[,c("date","adjusted")],by=join_by("date"))
      
      colnames(df_Dates_flt)[which(colnames(df_Dates_flt)=="adjusted")]<-FX_Data$symbol%>%unique()
    }
    
    # Cancella le colonne che contengono NA
    df_Dates_flt<-df_Dates_flt[,!colSums(is.na(df_Dates_flt))>0]
    
    fromDate<-min(df_Dates_flt$date)
    toDate<-max(df_Dates_flt$date)
    
    FX_data_corr_pearson<-df_Dates_flt%>%
      select(-c(date))%>%
      cor(method="pearson")%>%
      round(1)
    
    saveRDS(list(corr=FX_data_corr_pearson
                 ,from_to_dates=c(fromDate, toDate))
            ,file.path(path_Output,Periodicity
                       ,paste0("corr_pearson_",Periodicity,".Rds")))
    
    FX_data_corr_kendall<-df_Dates_flt%>%
      select(-c(date))%>%
      cor(method="kendall")%>%
      round(1)
    
    saveRDS(list(corr=FX_data_corr_kendall
                 ,from_to_dates=c(fromDate, toDate))
            ,file.path(path_Output,Periodicity
                       ,paste0("corr_kendall_",Periodicity,".Rds")))
    
    FX_data_corr_spearman<-df_Dates_flt%>%
      select(-c(date))%>%
      cor(method="spearman")%>%
      round(1)
    
    saveRDS(list(corr=FX_data_corr_spearman
                 ,from_to_dates=c(fromDate, toDate))
            ,file.path(path_Output,Periodicity
                       ,paste0("corr_spearman_",Periodicity,".Rds")))
    
    message(Periodicity," correlation computed!!!")
  }
  return("Correlation computed!!!")
}



