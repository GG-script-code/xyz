
#Trend-Following: SMA, EMA, MACD, Ichimoku
#Momentum: RSI, Stochastic, Williams %R
#Volatilità: ATR, Bollinger Bands, Donchian Channels
#Volume-Based: OBV, Chaikin Money Flow


# Compute FOREX metrics ----
fnc_trading_signals<-function(path_Output=path_output_signals, path_Input=path_output_metrics){
  # path_Output=path_output_signals; path_Input=path_output_metrics
  
  for(Periodicity in c("daily", "H1")){
    # Periodicity<-"H1"
    
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
        mutate(Return=if_else(lag(close)>0,((close/lag(close, default=first(close)))-1), 0))%>%
        mutate(sma_StrRet=Return*replace_na(lag(Sig_SMA),0)
               ,ema_StrRet=Return*replace_na(lag(Sig_EMA),0)
               ,macd_StrRet=Return*replace_na(lag(Sig_MACD),0)
               ,rsi_StrRet=Return*replace_na(lag(Sig_RSI),0)
               ,stoch_StrRet=Return*replace_na(lag(Sig_STOCH),0)
               ,bb_StrRet=Return*replace_na(lag(Sig_BB),0)
               ,adx_StrRet=Return*replace_na(lag(Sig_ADX),0)
               ,ichi_StrRet=Return*replace_na(lag(Sig_ICHI),0)
               ,meanRev_StrRet=Return*replace_na(lag(Sig_MeanRev),0))%>%
        mutate(Sig_ALL=case_when(
          Tot_signals>4~(+1)
          ,Tot_signals<(-4)~(-1)
          ,TRUE~NA_real_)
        ,all_StrRet=Return*replace_na(lag(Sig_ALL),0))%>%
        # **Stop-Loss Dinamico con ATR**
        mutate(Stop_Loss=case_when(
          Sig_ALL==1~close-(atr*0.50)
          ,Sig_ALL==-1~close+(atr*0.50)
          ,TRUE~NA_real_)
          ,Take_Profit=case_when(
            Sig_ALL==1~close+(atr*1.00)
            ,Sig_ALL==-1~close-(atr*1.00)
            ,TRUE~NA_real_)
          )
      
      # FX_signals$all_StrRet%>%sum(na.rm=TRUE)
      # FX_signals$sma_StrRet%>%sum(na.rm=TRUE)
      # FX_signals$ema_StrRet%>%sum(na.rm=TRUE)
      # FX_signals$macd_StrRet%>%sum(na.rm=TRUE)
      # FX_signals$rsi_StrRet%>%sum(na.rm=TRUE)
      # FX_signals$stoch_StrRet%>%sum(na.rm=TRUE)
      # FX_signals$bb_StrRet%>%sum(na.rm=TRUE)
      # FX_signals$adx_StrRet%>%sum(na.rm=TRUE)
      # FX_signals$ichi_StrRet%>%sum(na.rm=TRUE)
      # FX_signals$meanRev_StrRet%>%sum(na.rm=TRUE)
      
      # returns_strat<-FX_signals%>%
      #   select(all_of(c("sma_StrRet", "ema_StrRet", "macd_StrRet", "rsi_StrRet", "stoch_StrRet", "bb_StrRet", "adx_StrRet"#, "ichi_StrRet", "meanRev_StrRet"
      #                   )))%>%drop_na()
      # 
      # mean_ret <- colMeans(returns_strat)%>%scale()
      # cov_ret <- cov(returns_strat)
      # 
      # varianze<-cov_ret%>%diag()
      # mean_sharpe_proxy <- mean_ret / varianze  # Rendimento atteso su rischio
      # 
      # n <- length(mean_ret)
      # Dmat <- 2 * as.matrix(cov_ret)      # Matrice di varianza pesata (2x per QP)
      # dvec <- as.numeric(mean_sharpe_proxy)        # Rendimenti da massimizzare
      # Amat <- cbind(rep(1, n), diag(n))     # Vincoli: somma = 1, pesi >= 0
      # bvec <- c(1, rep(0, n))
      # meq <- 1                              # Solo il primo vincolo è di uguaglianza
      # 
      # opt <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = meq)
      # w_opt <- opt$solution
      # names(w_opt) <- colnames(returns_strat)
      # 
      # w_opt_clean <- w_opt
      # w_opt_clean[w_opt_clean < 0] <- 0
      # w_opt_clean <- w_opt_clean / sum(w_opt_clean)  # Ribilancia per sommare a 1
      # print(round(w_opt_clean, 4))
      
      FX_signals<-FX_signals%>%arrange(date%>%desc())
      
      write_feather(FX_signals
                    , file.path(path_Output,Periodicity,paste0(FX_symbol, "_signals.feather")))
      writexl::write_xlsx(FX_signals
                          , file.path(path_Output,Periodicity,paste0(FX_symbol, "_signals.xlsx")))
      
      pb$tick()
    }

  }
  
  return("FX signals updated!")
  
}

