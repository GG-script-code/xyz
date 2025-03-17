
#Trend-Following: SMA, EMA, MACD, Ichimoku
#Momentum: RSI, Stochastic, Williams %R
#Volatilità: ATR, Bollinger Bands, Donchian Channels
#Volume-Based: OBV, Chaikin Money Flow


# Compute FOREX metrics ----
fnc_trading_signals<-function(path_Output=path_output_signals, path_Input=path_output_metrics){
  # path_Output=path_output_signals; path_Input=path_output_metrics
  
  for(Periodicity in c("daily")){
    # Periodicity<-"daily"
    
    FX_Metrics_List<-list.files(file.path(path_Input,Periodicity),pattern=".feather$")
    
    pb<-progress_bar$new(total=(length(FX_Metrics_List)))
    
    for(FX_Metrics in FX_Metrics_List){
      # FX_Metrics<-FX_Metrics_List[str_detect(FX_Metrics_List,"CADCHF")%>%which()]
      
      # Upload the data for the FX symbol
      FX_metrics_data<-read_feather(file.path(path_Input,Periodicity,FX_Metrics))
      
      FX_symbol<-unique(FX_metrics_data$symbol)
      
      print(FX_symbol)
      
      # Compute the metrics
      FX_signals<-FX_metrics_data%>%
        mutate(Sig_SMA=case_when(
          ((SMA_veloce>SMA_lento)&
             (close>senkou_span_b))~1
          ,((SMA_veloce<SMA_lento)&
              (close<senkou_span_b))~-1
          ,TRUE~0
        ))%>%
        mutate(Sig_EMA=case_when(
          ((EMA_veloce>EMA_lento)&
             (close>senkou_span_b))~1
          ,((EMA_veloce<EMA_lento)&
              (close<senkou_span_b))~-1
          ,TRUE~0
        ))%>%
        mutate(Sig_MACD=case_when(
          ((macd>macd_signal)&
             (close>senkou_span_b))~1
          ,((macd<macd_signal)&
              (close<senkou_span_b))~-1
          ,TRUE~0
        ))%>%
        mutate(Sig_RSI=case_when(
          ((RSI<70)&
             (close>senkou_span_b))~1
          ,((RSI>30)&
              (close<senkou_span_b))~-1
          ,TRUE~0
        ))%>%
        mutate(Sig_STOCH=case_when(
          ((stoch<0.80)&(fastK>fastD)&
             (close>senkou_span_b))~1
          ,((stoch>0.20)&(fastK<fastD)&
              (close<senkou_span_b))~-1
          ,TRUE~0
        ))%>%
        mutate(Sig_BB=case_when(
          ((close>senkou_span_b)&
             (close>BB_mavg)&
             (BB_Width>=BB_Width_quant75))~1
          ,((close<senkou_span_b)&
              (close<BB_mavg)&
              (BB_Width>=BB_Width_quant75))~-1
          ,TRUE~0
        ))%>%
        mutate(Sig_ADX=case_when(
          ((close>senkou_span_b)&
             (ADX>20))~1
          ,((close<senkou_span_b)&
              (ADX>20))~-1
          ,TRUE~0
        ))%>%
        # Calcolo della pendenza della Kijun-Sen
        mutate(kijun_slope=(kijun_sen-lag(kijun_sen, 1)))%>%
        mutate(Sig_ICHI=case_when(
          ((BB_Width>=BB_Width_quant75)&
             (atr>atr_quant75))&   #Volatility_Filter
            (close>senkou_span_a)&
            (close>senkou_span_b)&
            (tenkan_sen>kijun_sen)&
            (chikou_span>lag(close, 26))&
            (ADX>20)&
            (RSI>30)&
            (abs(kijun_slope)>atr)~1
          ,((BB_Width>=BB_Width_quant75)&
              (atr>atr_quant75))&
            (close<senkou_span_a)&
            (close<senkou_span_b)&
            (tenkan_sen<kijun_sen)&
            (chikou_span<lag(close, 26))&
            (ADX>20)&
            (RSI<70)&
            (abs(kijun_slope)>atr)~-1
          ,TRUE~0
        ))%>%
        mutate(Sig_MeanRev=case_when(
          (RSI<30)&
            (close<BB_dn)&
            (atr<atr_quant75)~1
          ,(RSI>70)&
            (close>BB_up)&
            (atr<atr_quant75)~-1
          ,TRUE~0
        ))%>%
        mutate(Tot_signals=(Sig_SMA+
                              Sig_EMA+
                              Sig_MACD+
                              Sig_RSI+
                              Sig_STOCH+
                              Sig_BB+
                              Sig_ADX+
                              Sig_ICHI+
                              Sig_MeanRev))%>%
        select(c("symbol", "date", "close", "atr", "Tot_signals", "Sig_SMA", "Sig_EMA", "Sig_MACD", "Sig_RSI", "Sig_STOCH", "Sig_BB", "Sig_ADX", "Sig_ICHI", "Sig_MeanRev"))%>%
        arrange(date%>%desc())
      # %>%
      #   mutate(Signal=case_when(
      #     Tot_signals>=3~(+1)
      #     ,Tot_signals<=-3~(-1)
      #     ,TRUE~NA_real_
      #   )
      #   ,Return=if_else(lag(close)>0,(close/lag(close, default=first(close)))-1, 0)
      #   ,Strategy_Return=Return*replace_na(lag(Signal),0))%>%
      #   # **Stop-Loss Dinamico con ATR**
      #   mutate(Stop_Loss=case_when(
      #     Signal==1~close-(atr*0.50)
      #     ,Signal==-1~close+(atr*0.50)
      #     ,TRUE~NA_real_)
      #     ,Take_Profit=case_when(
      #       Signal==1~close+(atr*1.00)
      #       ,Signal==-1~close-(atr*1.00)
      #       ,TRUE~NA_real_)
      #     )
      
      # FX_signals$Strategy_Return%>%sum(na.rm=TRUE)
      
      write_feather(FX_signals, file.path(path_Output,Periodicity,paste0(FX_symbol, "_signals.feather")))
      
      pb$tick()
    }

  }
  
  return("FX signals updated!")
  
}

