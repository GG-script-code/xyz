# Funzione per calcolare l'Omega Ratio
calculate_omega_ratio<-function(returns=Strategy_Return, threshold=0) {
  excess_returns<-returns-threshold
  positive_returns<-sum(excess_returns[excess_returns>0], na.rm=TRUE)
  negative_returns<-abs(sum(excess_returns[excess_returns<0], na.rm=TRUE))
  
  omega_ratio<-positive_returns/negative_returns
  return(omega_ratio)
}

fun_composite_score<-function(Strategy_Return){
  
  strRet_mean<-mean(Strategy_Return, na.rm=TRUE)
  strRet_sd<-sd(Strategy_Return, na.rm=TRUE)
  
  # Calcola le metriche alternative
  {
    sharpe_ratio<-strRet_mean/strRet_sd
    sortino_ratio<-strRet_mean/sd(Strategy_Return[Strategy_Return<0], na.rm=TRUE)
    max_drawdown<-maxDrawdown(Strategy_Return, geometric=TRUE)
    calmar_ratio<-strRet_mean/abs(max_drawdown)
    profit_factor<-sum(Strategy_Return[Strategy_Return>0], na.rm=TRUE)/abs(sum(Strategy_Return[Strategy_Return<0], na.rm=TRUE))
    ulcer_index<-UlcerIndex(Strategy_Return)%>%c()
    omega_ratio<-calculate_omega_ratio(Strategy_Return)
    information_ratio<-mean(Strategy_Return-strRet_mean, na.rm=TRUE)/sd(Strategy_Return-strRet_mean, na.rm=TRUE)
    sterling_ratio<-strRet_mean/strRet_sd
  }
  
  {
    if(is.na(sharpe_ratio)|is.nan(sharpe_ratio)|is.infinite(sharpe_ratio)){
      sharpe_ratio<-0
    }
    if(is.na(sortino_ratio)|is.nan(sortino_ratio)|is.infinite(sortino_ratio)){
      sortino_ratio<-0
    }
    if(is.na(calmar_ratio)|is.nan(calmar_ratio)|is.infinite(calmar_ratio)){
      calmar_ratio<-0
    }
    if(is.na(profit_factor)|is.nan(profit_factor)|is.infinite(profit_factor)){
      profit_factor<-0
    }
    if(is.na(ulcer_index)|is.nan(ulcer_index)|is.infinite(ulcer_index)){
      ulcer_index<-0
    }
    if(is.na(omega_ratio)|is.nan(omega_ratio)|is.infinite(omega_ratio)){
      omega_ratio<-0
    }
    if(is.na(information_ratio)|is.nan(information_ratio)|is.infinite(information_ratio)){
      information_ratio<-0
    }
  }
  
  # Composite Score
  composite_score<-(sharpe_ratio*0.25)+
    (sortino_ratio*0.2)+
    (calmar_ratio*0.2)+
    (profit_factor*0.1)+
    (ulcer_index*0.1)+
    (omega_ratio*0.1)+
    (information_ratio*0.05)
  
  return(composite_score)
  
}

# **Definizione della funzione obiettivo (Sharpe Ratio)**
optimize_trend_following<-function(short_sma, long_sma
                                   , short_ema, long_ema
                                   , macd_nF, macd_nSl, macd_nSi
                                   , rsi_n
                                   , stoch_nFk, stoch_nFd, stoch_nSd, stoch_smooth
                                   , BB_n, BB_sd
                                   , adx_n
                                   , forex_data=FX_Data){
  
  # Creazione dei segnali di trading
  data<-forex_data%>%
    #' SMA
    tq_mutate(select=c("close")
              ,mutate_fun=SMA,n=round(short_sma)
              ,col_rename="SMA_veloce")%>%
    tq_mutate(select=c("close")
              ,mutate_fun=SMA,n=round(long_sma)
              ,col_rename="SMA_lento")%>%
    #' #' EMA
    tq_mutate(select=c("close")
              ,mutate_fun=EMA,n=round(short_ema)
              ,col_rename="EMA_veloce")%>%
    tq_mutate(select=c("close")
              ,mutate_fun=EMA,n=round(long_ema)
              ,col_rename="EMA_lento")%>%
    #' MACD
    tq_mutate(select=c("close")
              ,mutate_fun=MACD,maType=SMA
              ,nFast=round(macd_nF),nSlow=round(macd_nSl),nSig=round(macd_nSi)
              ,col_rename=c("macd", "macd_signal")
              ,percent=TRUE)%>%
    #' Ichimoku
    fun_ichimoku()%>%
    #' Relative Strenght Index
    tq_mutate(select=close
              ,mutate_fun=RSI,n=round(rsi_n)
              ,col_rename="RSI")%>%
    #' Stochastic Oscillator
    tq_mutate(select=c("high","low","close")
              ,mutate_fun=stoch,maType=SMA
              ,nFastK=round(stoch_nFk)
              ,nFastD=round(stoch_nFd)
              ,nSlowD=round(stoch_nSd)
              ,smooth=round(stoch_smooth))%>%
    #' Bollinger Bands
    tq_mutate(select=close
              ,mutate_fun=BBands,maType=SMA
              ,n=round(BB_n),sd=round(BB_sd))%>%
    rename("BB_dn"="dn"
           ,"BB_mavg"="mavg"
           ,"BB_up"="up"
           ,"BB_pctB"="pctB")%>%
    #Usa Bollinger Band Width (BBW) per confermare un breakout solo se la volatilità sta aumentando
    mutate(BB_Width=BB_up-BB_dn)%>%
    mutate(BB_Width_quantile=rollapply(BB_Width
                                       , width=100
                                       , FUN=function(x){quantile(x, probs=0.75)}
                                       , fill=NA
                                       , align="right"
                                       , partial=FALSE))%>%
  #' Average Directional Movement Index
  #Aggiungere ADX (Average Directional Index) per filtrare i segnali con trend debole
  tq_mutate(select=c("high","low","close")
            ,mutate_fun=ADX,maType=SMA
            ,n=round(adx_n))%>%
    drop_na()%>%
    mutate(Signal=case_when(
      ((SMA_veloce>SMA_lento)&
         (EMA_veloce>EMA_lento)&
         (macd>macd_signal)&
         (fastK>fastD)&
         (RSI<70)&
         (close>senkou_span_b)&
         (close>BB_up)&
         (BB_Width>BB_Width_quantile)&
         (ADX>20))~1
      ,((SMA_veloce<SMA_lento)&
          (EMA_veloce<EMA_lento)&
          (macd<macd_signal)&
          (fastK<fastD)&
          (RSI>30)&
          (close<senkou_span_b)&
          (close<BB_dn)&
          (BB_Width>BB_Width_quantile)&
          (ADX>20))~-1
      ,TRUE~0
    )
    ,Return=if_else(lag(close)>0,(close/lag(close, default=first(close)))-1, 0)
    ,Strategy_Return=Return*replace_na(lag(Signal),0)
    )
  
  # Composite Score
  composite_score<-fun_composite_score(data$Strategy_Return)
  
  return(list(Score=composite_score))
}

optimize_SMA<-function(short_sma=50, long_sma=200, forex_data=FX_Data){
  
  # Creazione dei segnali di trading
  data<-forex_data%>%
    #' SMA
    tq_mutate(select=c("close")
              ,mutate_fun=SMA,n=round(short_sma)
              ,col_rename="SMA_veloce")%>%
    tq_mutate(select=c("close")
              ,mutate_fun=SMA,n=round(long_sma)
              ,col_rename="SMA_lento")%>%
    #' Ichimoku
    fun_ichimoku()%>%
    drop_na()%>%
    mutate(Signal=case_when(
      ((SMA_veloce>SMA_lento)&
         (close>senkou_span_b))~1
      ,((SMA_veloce<SMA_lento)&
          (close<senkou_span_b))~-1
      ,TRUE~0
    )
    ,Return=if_else(lag(close)>0,(close/lag(close, default=first(close)))-1, 0)
    ,Strategy_Return=Return*replace_na(lag(Signal),0)
    )
  
  # Composite Score
  composite_score<-fun_composite_score(data$Strategy_Return)
  
  return(list(Score=composite_score))
}

optimize_EMA<-function(short_ema, long_ema, forex_data=FX_Data){
  
  # Creazione dei segnali di trading
  data<-forex_data%>%
    #'EMA
    tq_mutate(select=c("close")
              ,mutate_fun=EMA,n=round(short_ema)
              ,col_rename="EMA_veloce")%>%
    tq_mutate(select=c("close")
              ,mutate_fun=EMA,n=round(long_ema)
              ,col_rename="EMA_lento")%>%
    #' Ichimoku
    fun_ichimoku()%>%
    drop_na()%>%
    mutate(Signal=case_when(
      ((EMA_veloce>EMA_lento)&
         (close>senkou_span_b))~1
      ,((EMA_veloce<EMA_lento)&
          (close<senkou_span_b))~-1
      ,TRUE~0
    )
    ,Return=if_else(lag(close)>0,(close/lag(close, default=first(close)))-1, 0)
    ,Strategy_Return=Return*replace_na(lag(Signal),0)
    )
  
  # Composite Score
  composite_score<-fun_composite_score(data$Strategy_Return)
  
  return(list(Score=composite_score))
}

optimize_MACD<-function(macd_nF, macd_nSl, macd_nSi, forex_data=FX_Data){
  
  # Creazione dei segnali di trading
  data<-forex_data%>%
    #' MACD
    tq_mutate(select=c("close")
              ,mutate_fun=MACD,maType=SMA
              ,nFast=round(macd_nF),nSlow=round(macd_nSl),nSig=round(macd_nSi)
              ,col_rename=c("macd", "macd_signal")
              ,percent=TRUE)%>%
    #' Ichimoku
    fun_ichimoku()%>%
    drop_na()%>%
    mutate(Signal=case_when(
      ((macd>macd_signal)&
         (close>senkou_span_b))~1
      ,((macd<macd_signal)&
          (close<senkou_span_b))~-1
      ,TRUE~0
    )
    ,Return=if_else(lag(close)>0,(close/lag(close, default=first(close)))-1, 0)
    ,Strategy_Return=Return*replace_na(lag(Signal),0)
    )
  
  # Composite Score
  composite_score<-fun_composite_score(data$Strategy_Return)
  
  return(list(Score=composite_score))
}

optimize_RSI<-function(rsi_n, forex_data=FX_Data){
  
  # Creazione dei segnali di trading
  data<-forex_data%>%
    #' Ichimoku
    fun_ichimoku()%>%
    #' Relative Strenght Index
    tq_mutate(select=close
              ,mutate_fun=RSI,n=round(rsi_n)
              ,col_rename="RSI")%>%
    drop_na()%>%
    mutate(Signal=case_when(
      ((RSI<70)&
         (close>senkou_span_b))~1
      ,((RSI>30)&
          (close<senkou_span_b))~-1
      ,TRUE~0
    )
    ,Return=if_else(lag(close)>0,(close/lag(close, default=first(close)))-1, 0)
    ,Strategy_Return=Return*replace_na(lag(Signal),0)
    )
  
  # Composite Score
  composite_score<-fun_composite_score(data$Strategy_Return)
  
  return(list(Score=composite_score))
}

optimize_STOCH<-function(stoch_nFk, stoch_nFd, stoch_nSd, stoch_smooth, forex_data=FX_Data){
  
  # Creazione dei segnali di trading
  data<-forex_data%>%
    #' Ichimoku
    fun_ichimoku()%>%
    #' Stochastic Oscillator
    tq_mutate(select=c("high","low","close")
              ,mutate_fun=stoch,maType=SMA
              ,nFastK=round(stoch_nFk)
              ,nFastD=round(stoch_nFd)
              ,nSlowD=round(stoch_nSd)
              ,smooth=round(stoch_smooth))%>%
    drop_na()%>%
    mutate(Signal=case_when(
      ((stoch<0.80)&(fastK>fastD)&
         (close>senkou_span_b))~1
      ,((stoch>0.20)&(fastK<fastD)&
          (close<senkou_span_b))~-1
      ,TRUE~0
    )
    ,Return=if_else(lag(close)>0,(close/lag(close, default=first(close)))-1, 0)
    ,Strategy_Return=Return*replace_na(lag(Signal),0)
    )
  
  # Composite Score
  composite_score<-fun_composite_score(data$Strategy_Return)
  
  return(list(Score=composite_score))
}

optimize_BBands<-function(BB_n, BB_sd, forex_data=FX_Data){
  
  # Creazione dei segnali di trading
  data<-forex_data%>%
    #' Ichimoku
    fun_ichimoku()%>%
    #' Bollinger Bands
    tq_mutate(select=close
              ,mutate_fun=BBands,maType=SMA
              ,n=round(BB_n),sd=round(BB_sd))%>%
    rename("BB_dn"="dn"
           ,"BB_mavg"="mavg"
           ,"BB_up"="up"
           ,"BB_pctB"="pctB")%>%
    #Usa Bollinger Band Width (BBW) per confermare un breakout solo se la volatilità sta aumentando
    mutate(BB_Width=BB_up-BB_dn)%>%
    drop_na()%>%
    mutate(BB_Width_quantile=rollapply(BB_Width
                                       , width=100
                                       , FUN=function(x){quantile(x, probs=0.75)}
                                       , fill=NA
                                       , align="right"
                                       , partial=FALSE))%>%
    drop_na()%>%
    mutate(Signal=case_when(
      ((close>senkou_span_b)&
         (close>BB_up)&
         (BB_Width>BB_Width_quantile))~1
      ,((close<senkou_span_b)&
          (close<BB_dn)&
          (BB_Width>BB_Width_quantile))~-1
      ,TRUE~0
    )
    ,Return=if_else(lag(close)>0,(close/lag(close, default=first(close)))-1, 0)
    ,Strategy_Return=Return*replace_na(lag(Signal),0)
    )
  
  # Composite Score
  composite_score<-fun_composite_score(data$Strategy_Return)
  
  return(list(Score=composite_score))
}

optimize_ADX<-function(adx_n, forex_data=FX_Data){
  
  # Creazione dei segnali di trading
  data<-forex_data%>%
    #' Ichimoku
    fun_ichimoku()%>%
    #' Average Directional Movement Index
    #Aggiungere ADX (Average Directional Index) per filtrare i segnali con trend debole
    tq_mutate(select=c("high","low","close")
              ,mutate_fun=ADX,maType=SMA
              ,n=round(adx_n))%>%
    drop_na()%>%
    mutate(Signal=case_when(
      ((close>senkou_span_b)&
         (ADX>20))~1
      ,((close<senkou_span_b)&
          (ADX>20))~-1
      ,TRUE~0
    )
    ,Return=if_else(lag(close)>0,(close/lag(close, default=first(close)))-1, 0)
    ,Strategy_Return=Return*replace_na(lag(Signal),0)
    )
  
  # Composite Score
  composite_score<-fun_composite_score(data$Strategy_Return)
  
  return(list(Score=composite_score))
}

# Compute FOREX metrics ----
fnc_test_metrics_FX_Data<-function(path_Input=path_input_forexSb
                                   , to_be_optimize=c("SMA", "EMA", "MACD", "RSI", "STOCH", "BBands", "ADX")){
  # path_Input=path_input_forexSb; to_be_optimize=c("SMA", "EMA", "MACD", "RSI", "STOCH", "BBands", "ADX")
  
  set_init_points<-10; set_n_iter<-30; set_acq<-"ucb"; set_kappa<-2.5
  
  set_popSize<-10; set_maxiter<-30; set_elitism<-5
  
  
  test_results<-list()
  
  for(Periodicity in c("H1")){
    # Periodicity<-"H1"
    
    # list FOREX data DOWNLOADED
    FX_Data_List<-list.files(file.path(path_Input, Periodicity),pattern=".feather$")
    
    pb<-progress_bar$new(total=(length(FX_Data_List)))
    
    for(FX_Dataset in FX_Data_List){
      # FX_Dataset<-FX_Data_List[str_detect(FX_Data_List,"CADCHF")%>%which()]
      
      print(FX_Dataset)
      
      forex_symbol<-FX_Dataset%>%str_split_i("_", i=1)
      
      # Upload the data for the FX symbol
      FX_Data<-read_feather(file.path(path_Input,Periodicity,FX_Dataset))%>%
        slice_tail(n=10000)
      
      if("SMA"%in%to_be_optimize){
        opt_results<-BayesianOptimization(
          FUN=function(short_sma, long_sma){
            optimize_SMA(short_sma, long_sma, forex_data=FX_Data)
          }
          ,bounds=list(
            short_sma=c(20, 70)
            ,long_sma=c(100, 200)
          )
          ,init_points=set_init_points
          ,n_iter=set_n_iter
          ,acq=set_acq
          ,kappa=set_kappa
        )
        
        best_params_bay<-opt_results$Best_Par
        
        ga_results<-ga(
          type="real-valued"
          , fitness=function(x) -optimize_SMA(x[1], x[2], forex_data=FX_Data)$Score
          , lower=as.numeric(best_params_bay)*0.8  # Range attorno ai migliori parametri di BO
          , upper=as.numeric(best_params_bay)*1.2
          , popSize=set_popSize  # Dimensione della popolazione
          , maxiter=set_maxiter  # Numero di generazioni
          , elitism=set_elitism  # Mantiene i migliori individui
        )
        
        best_params_ga<-ga_results@solution
        
        x1<-mean(best_params_ga[,1],na.rm=T)
        x2<-mean(best_params_ga[,2],na.rm=T)
        
        test_results[[Periodicity]][["SMA"]][[forex_symbol]]<-tibble(symbol=forex_symbol
                                                                     ,short_sma=x1
                                                                     ,long_sma=x2)
      }
      gc()
      if("EMA"%in%to_be_optimize){
        opt_results<-BayesianOptimization(
          FUN=function(short_ema, long_ema){
            optimize_EMA(short_ema, long_ema, forex_data=FX_Data)
          }
          ,bounds=list(
            short_ema=c(20, 70)
            ,long_ema=c(100, 200)
          )
          ,init_points=set_init_points
          ,n_iter=set_n_iter
          ,acq=set_acq
          ,kappa=set_kappa
        )
        
        best_params_bay<-opt_results$Best_Par
        
        ga_results<-ga(
          type="real-valued"
          , fitness=function(x) -optimize_EMA(x[1], x[2], forex_data=FX_Data)$Score
          , lower=as.numeric(best_params_bay)*0.8  # Range attorno ai migliori parametri di BO
          , upper=as.numeric(best_params_bay)*1.2
          , popSize=set_popSize  # Dimensione della popolazione
          , maxiter=set_maxiter  # Numero di generazioni
          , elitism=set_elitism  # Mantiene i migliori individui
        )
        
        best_params_ga<-ga_results@solution
        
        x1<-mean(best_params_ga[,1],na.rm=T)
        x2<-mean(best_params_ga[,2],na.rm=T)
        
        test_results[[Periodicity]][["EMA"]][[forex_symbol]]<-tibble(symbol=forex_symbol
                                                                     ,short_ema=x1
                                                                     ,long_ema=x2)
      }
      gc()
      if("MACD"%in%to_be_optimize){
        opt_results<-BayesianOptimization(
          FUN=function(macd_nF, macd_nSl, macd_nSi){
            optimize_MACD(macd_nF, macd_nSl, macd_nSi, forex_data=FX_Data)
          }
          ,bounds=list(
            macd_nF=c(10, 15), macd_nSl=c(20, 30), macd_nSi=c(5, 10)
          )
          ,init_points=set_init_points
          ,n_iter=set_n_iter
          ,acq=set_acq
          ,kappa=set_kappa
        )
        
        best_params_bay<-opt_results$Best_Par
        
        ga_results<-ga(
          type="real-valued"
          , fitness=function(x) -optimize_MACD(x[1], x[2], x[3], forex_data=FX_Data)$Score
          , lower=as.numeric(best_params_bay)*0.8  # Range attorno ai migliori parametri di BO
          , upper=as.numeric(best_params_bay)*1.2
          , popSize=set_popSize  # Dimensione della popolazione
          , maxiter=set_maxiter  # Numero di generazioni
          , elitism=set_elitism  # Mantiene i migliori individui
        )
        
        best_params_ga<-ga_results@solution
        
        x1<-mean(best_params_ga[,1],na.rm=T)
        x2<-mean(best_params_ga[,2],na.rm=T)
        x3<-mean(best_params_ga[,3],na.rm=T)
        
        test_results[[Periodicity]][["MACD"]][[forex_symbol]]<-tibble(symbol=forex_symbol
                                                                     ,macd_nF=x1
                                                                     ,macd_nSl=x2
                                                                     ,macd_nSi=x3)
      }
      gc()
      if("RSI"%in%to_be_optimize){
        opt_results<-BayesianOptimization(
          FUN=function(rsi_n){
            optimize_RSI(rsi_n, forex_data=FX_Data)
          }
          ,bounds=list(
            rsi_n=c(10, 25)
          )
          ,init_points=set_init_points
          ,n_iter=set_n_iter
          ,acq=set_acq
          ,kappa=set_kappa
        )
        
        best_params_bay<-opt_results$Best_Par
        
        ga_results<-ga(
          type="real-valued"
          , fitness=function(x) -optimize_RSI(x[1], forex_data=FX_Data)$Score
          , lower=as.numeric(best_params_bay)*0.8  # Range attorno ai migliori parametri di BO
          , upper=as.numeric(best_params_bay)*1.2
          , popSize=set_popSize  # Dimensione della popolazione
          , maxiter=set_maxiter  # Numero di generazioni
          , elitism=set_elitism  # Mantiene i migliori individui
        )
        
        best_params_ga<-ga_results@solution
        
        x1<-mean(best_params_ga[,1],na.rm=T)
        
        test_results[[Periodicity]][["RSI"]][[forex_symbol]]<-tibble(symbol=forex_symbol
                                                                     ,rsi_n=x1)
      }
      gc()
      if("STOCH"%in%to_be_optimize){
        opt_results<-BayesianOptimization(
          FUN=function(stoch_nFk, stoch_nFd, stoch_nSd, stoch_smooth){
            optimize_STOCH(stoch_nFk, stoch_nFd, stoch_nSd, stoch_smooth, forex_data=FX_Data)
          }
          ,bounds=list(
            stoch_nFk=c(10, 20), stoch_nFd=c(2, 5), stoch_nSd=c(2, 5), stoch_smooth=c(1, 2)
          )
          ,init_points=set_init_points
          ,n_iter=set_n_iter
          ,acq=set_acq
          ,kappa=set_kappa
        )
        
        best_params_bay<-opt_results$Best_Par
        
        ga_results<-ga(
          type="real-valued"
          , fitness=function(x) -optimize_STOCH(x[1], x[2], x[3], x[4], forex_data=FX_Data)$Score
          , lower=as.numeric(best_params_bay)*0.8  # Range attorno ai migliori parametri di BO
          , upper=as.numeric(best_params_bay)*1.2
          , popSize=set_popSize  # Dimensione della popolazione
          , maxiter=set_maxiter  # Numero di generazioni
          , elitism=set_elitism  # Mantiene i migliori individui
        )
        
        best_params_ga<-ga_results@solution
        
        x1<-mean(best_params_ga[,1],na.rm=T)
        x2<-mean(best_params_ga[,2],na.rm=T)
        x3<-mean(best_params_ga[,3],na.rm=T)
        x4<-mean(best_params_ga[,4],na.rm=T)
        
        test_results[[Periodicity]][["STOCH"]][[forex_symbol]]<-tibble(symbol=forex_symbol
                                                                     ,stoch_nFk=x1
                                                                     ,stoch_nFd=x2
                                                                     ,stoch_nSd=x3
                                                                     ,stoch_smooth=x4)
      }
      gc()
      if("BBands"%in%to_be_optimize){
        opt_results<-BayesianOptimization(
          FUN=function(BB_n, BB_sd){
            optimize_BBands(BB_n, BB_sd, forex_data=FX_Data)
          }
          ,bounds=list(
            BB_n=c(15, 25), BB_sd=c(2,3)
          )
          ,init_points=set_init_points
          ,n_iter=set_n_iter
          ,acq=set_acq
          ,kappa=set_kappa
        )
        
        best_params_bay<-opt_results$Best_Par
        
        ga_results<-ga(
          type="real-valued"
          , fitness=function(x) -optimize_BBands(x[1], x[2], forex_data=FX_Data)$Score
          , lower=as.numeric(best_params_bay)*0.8  # Range attorno ai migliori parametri di BO
          , upper=as.numeric(best_params_bay)*1.2
          , popSize=set_popSize  # Dimensione della popolazione
          , maxiter=set_maxiter  # Numero di generazioni
          , elitism=set_elitism  # Mantiene i migliori individui
        )
        
        best_params_ga<-ga_results@solution
        
        x1<-mean(best_params_ga[,1],na.rm=T)
        x2<-mean(best_params_ga[,2],na.rm=T)
        
        test_results[[Periodicity]][["BBands"]][[forex_symbol]]<-tibble(symbol=forex_symbol
                                                                     ,BB_n=x1
                                                                     ,BB_sd=x2)
      }
      gc()
      if("ADX"%in%to_be_optimize){
        opt_results<-BayesianOptimization(
          FUN=function(adx_n){
            optimize_ADX(adx_n, forex_data=FX_Data)
          }
          ,bounds=list(
            adx_n=c(10, 25)
          )
          ,init_points=set_init_points
          ,n_iter=set_n_iter
          ,acq=set_acq
          ,kappa=set_kappa
        )
        
        best_params_bay<-opt_results$Best_Par
        
        ga_results<-ga(
          type="real-valued"
          , fitness=function(x) -optimize_ADX(x[1], forex_data=FX_Data)$Score
          , lower=as.numeric(best_params_bay)*0.8  # Range attorno ai migliori parametri di BO
          , upper=as.numeric(best_params_bay)*1.2
          , popSize=set_popSize  # Dimensione della popolazione
          , maxiter=set_maxiter  # Numero di generazioni
          , elitism=set_elitism  # Mantiene i migliori individui
        )
        
        best_params_ga<-ga_results@solution
        
        x1<-mean(best_params_ga[,1],na.rm=T)
        
        test_results[[Periodicity]][["ADX"]][[forex_symbol]]<-tibble(symbol=forex_symbol
                                                                     ,adx_n=x1)
      }
      gc()
      pb$tick()
    }
    print(paste0("FX ",Periodicity," metrics tested!"))
  }
  
  return(test_results)
  
}


test_metrics<-fnc_test_metrics_FX_Data()


writexl::write_xlsx(x=list("SMA"=test_metrics$daily$SMA%>%
                             bind_rows()%>%
                             mutate_if(is.numeric, round, digits=0)
                           ,"EMA"=test_metrics$daily$EMA%>%
                             bind_rows()%>%
                             mutate_if(is.numeric, round, digits=0)
                           ,"MACD"=test_metrics$daily$MACD%>%
                             bind_rows()%>%
                             mutate_if(is.numeric, round, digits=0)
                           ,"RSI"=test_metrics$daily$RSI%>%
                             bind_rows()%>%
                             mutate_if(is.numeric, round, digits=0)
                           ,"STOCH"=test_metrics$daily$STOCH%>%
                             bind_rows()%>%
                             mutate_if(is.numeric, round, digits=0)
                           ,"BBands"=test_metrics$daily$BBands%>%
                             bind_rows()%>%
                             mutate_if(is.numeric, round, digits=0)
                           ,"ADX"=test_metrics$daily$ADX%>%
                             bind_rows()%>%
                             mutate_if(is.numeric, round, digits=0)
                           )
                    ,path=file.path(path_parameters, paste0(Sys.Date(), "_parameters_metrics_H1.xlsx")))

