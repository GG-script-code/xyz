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

optimize_SMA<-function(short_sma=50, long_sma=200, forex_data=FX_Data){
  
  # short_sma=20; long_sma=35
  
  # Creazione dei segnali di trading
  data<-forex_data%>%
    #' SMA
    tq_mutate(select=c("close")
              ,mutate_fun=SMA,n=round(short_sma)
              ,col_rename="SMA_veloce")%>%
    tq_mutate(select=c("close")
              ,mutate_fun=SMA,n=round(long_sma)
              ,col_rename="SMA_lento")%>%
    drop_na()%>%
    mutate(Signal=case_when(
      ((SMA_veloce>SMA_lento)
      )~1
      ,((SMA_veloce<SMA_lento)
      )~-1
      ,TRUE~0
    )
    ,Return=if_else(lag(close)>0,(close/lag(close, default=first(close)))-1, 0)
    ,Strategy_Return=Return*replace_na(lag(Signal),0)
    )
  
  # sum(data$Strategy_Return, na.rm=T)
  
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
    drop_na()%>%
    mutate(Signal=case_when(
      ((EMA_veloce>EMA_lento)
      )~1
      ,((EMA_veloce<EMA_lento)
      )~-1
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
    drop_na()%>%
    mutate(Signal=case_when(
      ((macd>macd_signal)
      )~1
      ,((macd<macd_signal)
      )~-1
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
  
  # rsi_n=16
  
  # Creazione dei segnali di trading
  data<-forex_data%>%
    #' Relative Strenght Index
    tq_mutate(select=close
              ,mutate_fun=RSI,n=round(rsi_n)
              ,col_rename="RSI")%>%
    mutate(RSI_mean=rollapply(RSI
                              , width=5
                              , FUN=function(x){mean(x, na.rm=TRUE)}
                              , fill=NA
                              , align="right"
                              , partial=TRUE))%>%
    drop_na()%>%
    mutate(Signal=case_when(
      ((RSI_mean<30)
      )~1
      ,((RSI_mean>70)
      )~-1
      ,TRUE~0
    )
    ,Return=if_else(lag(close)>0,(close/lag(close, default=first(close)))-1, 0)
    ,Strategy_Return=Return*replace_na(lag(Signal),0)
    )
  
  # sum(data$Strategy_Return, na.rm=T)
  
  # Composite Score
  composite_score<-fun_composite_score(data$Strategy_Return)
  
  return(list(Score=composite_score))
}

optimize_STOCH<-function(stoch_nFk, stoch_nFd, stoch_nSd, stoch_smooth, forex_data=FX_Data){
  
  # stoch_nFk=9; stoch_nFd=5; stoch_nSd=5; stoch_smooth=1
  
  # Creazione dei segnali di trading
  data<-forex_data%>%
    #' Stochastic Oscillator
    tq_mutate(select=c("high","low","close")
              ,mutate_fun=stoch,maType=SMA
              ,nFastK=round(stoch_nFk)
              ,nFastD=round(stoch_nFd)
              ,nSlowD=round(stoch_nSd)
              ,smooth=round(stoch_smooth))%>%
    mutate(stoch_mean=rollapply(stoch
                                , width=5
                                , FUN=function(x){mean(x, na.rm=TRUE)}
                                , fill=NA
                                , align="right"
                                , partial=TRUE))%>%
    drop_na()%>%
    mutate(Signal=case_when(
      ((stoch_mean<0.30)&(fastK>fastD)
      )~1
      ,((stoch_mean>0.70)&(fastK<fastD)
      )~-1
      ,TRUE~0
    )
    ,Return=if_else(lag(close)>0,(close/lag(close, default=first(close)))-1, 0)
    ,Strategy_Return=Return*replace_na(lag(Signal),0)
    )
  
  # sum(data$Strategy_Return, na.rm=T)
  
  # Composite Score
  composite_score<-fun_composite_score(data$Strategy_Return)
  
  return(list(Score=composite_score))
}

optimize_BBands<-function(BB_n, BB_sd, forex_data=FX_Data){
  
  # BB_n=26; BB_sd=2
  
  # Creazione dei segnali di trading
  data<-forex_data%>%
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
    mutate(BB_Width_quant75=rollapply(BB_Width
                                      , width=360
                                      , FUN=function(x){quantile(x, probs=0.75)}
                                      , fill=NA
                                      , align="right"
                                      , partial=FALSE))%>%
    drop_na()%>%
    mutate(Signal=case_when(
      ((close>BB_mavg)&
          (BB_Width>=BB_Width_quant75))~1
      ,((close<BB_mavg)&
          (BB_Width>=BB_Width_quant75))~-1
      ,TRUE~0
    )
    ,Return=if_else(lag(close)>0,(close/lag(close, default=first(close)))-1, 0)
    ,Strategy_Return=Return*replace_na(lag(Signal),0)
    )
  
  # sum(data$Strategy_Return, na.rm=T)
  
  # Composite Score
  composite_score<-fun_composite_score(data$Strategy_Return)
  
  return(list(Score=composite_score))
}

optimize_ADX<-function(adx_n, forex_data=FX_Data){
  
  # adx_n=10
  
  # Creazione dei segnali di trading
  data<-forex_data%>%
    #' Ichimoku
    fun_ichimoku()%>%
    #' Average Directional Movement Index
    #Aggiungere ADX (Average Directional Index) per filtrare i segnali con trend debole
    tq_mutate(select=c("high","low","close")
              ,mutate_fun=ADX,maType=SMA
              ,n=round(adx_n))%>%
    mutate(ADX_mean=rollapply(ADX
                              , width=5
                              , FUN=function(x){mean(x, na.rm=TRUE)}
                              , fill=NA
                              , align="right"
                              , partial=TRUE))%>%
    drop_na()%>%
    mutate(Signal=case_when(
      ((senkou_span_a>senkou_span_b)&
         (ADX_mean>20))~1
      ,((senkou_span_a<senkou_span_b)&
          (ADX_mean>20))~-1
      ,TRUE~0
    )
    ,Return=if_else(lag(close)>0,(close/lag(close, default=first(close)))-1, 0)
    ,Strategy_Return=Return*replace_na(lag(Signal),0)
    )
  
  # sum(data$Strategy_Return, na.rm=T)
  
  # Composite Score
  composite_score<-fun_composite_score(data$Strategy_Return)
  
  return(list(Score=composite_score))
}

# Compute FOREX metrics ----
fnc_test_metrics_FX_Data<-function(to_be_optimize_FX=c("CADCHF", "EURCAD", "EURCHF", "EURGBP", "EURUSD", "GBPCAD", "GBPCHF", "GBPUSD", "USDCAD", "USDCHF")
                                   , to_be_optimize_param=c("SMA", "EMA", "MACD", "RSI", "STOCH", "BBands", "ADX")
                                   , path_Input=path_input_forexSb
                                   , path_Parameters=path_parameters
                                   , optim_ga=FALSE
                                   , parallel_BAY=TRUE){
  
  # path_Input=path_input_forexSb; path_Parameters=path_parameters; optim_ga=FALSE; parallel_BAy=TRUE; to_be_optimize_param=c("SMA", "EMA", "MACD", "RSI", "STOCH", "BBands", "ADX"); to_be_optimize_FX=c("CADCHF", "EURCAD", "EURCHF", "EURGBP", "EURUSD", "GBPCAD", "GBPCHF", "GBPUSD", "USDCAD", "USDCHF")
  
  set_init_points<-10; set_n_iter<-50; set_acq<-"ucb"; set_kappa<-2.5
  set_popSize<-10; set_maxiter<-50; set_elitism<-5
  
  short_SMA<-c(5, 30); long_SMA<-c(35, 120); short_EMA<-c(5, 30); long_EMA<-c(35, 120)

  optim_results<-list()
  
  for(Periodicity in c("H1")){
    # Periodicity<-"H1"
    
    file_param_metrics<-file.path(path_Parameters, paste0("parameters_metrics_", Periodicity,".xlsx"))
    
    {
      par_metr_bay_SMA<-readxl::read_xlsx(file_param_metrics, sheet="SMA")
      par_metr_bay_EMA<-readxl::read_xlsx(file_param_metrics, sheet="EMA")
      par_metr_bay_MACD<-readxl::read_xlsx(file_param_metrics, sheet="MACD")
      par_metr_bay_RSI<-readxl::read_xlsx(file_param_metrics, sheet="RSI")
      par_metr_bay_STOCH<-readxl::read_xlsx(file_param_metrics, sheet="STOCH")
      par_metr_bay_BBands<-readxl::read_xlsx(file_param_metrics, sheet="BBands")
      par_metr_bay_ADX<-readxl::read_xlsx(file_param_metrics, sheet="ADX")
      
      par_metr_ga_SMA<-readxl::read_xlsx(file_param_metrics, sheet="SMA")
      par_metr_ga_EMA<-readxl::read_xlsx(file_param_metrics, sheet="EMA")
      par_metr_ga_MACD<-readxl::read_xlsx(file_param_metrics, sheet="MACD")
      par_metr_ga_RSI<-readxl::read_xlsx(file_param_metrics, sheet="RSI")
      par_metr_ga_STOCH<-readxl::read_xlsx(file_param_metrics, sheet="STOCH")
      par_metr_ga_BBands<-readxl::read_xlsx(file_param_metrics, sheet="BBands")
      par_metr_ga_ADX<-readxl::read_xlsx(file_param_metrics, sheet="ADX")
    }
    
    # list FOREX data DOWNLOADED
    FX_Data_List<-list.files(file.path(path_Input, Periodicity),pattern=".feather$")
    
    pb<-progress_bar$new(total=(length(FX_Data_List)))
    
    for(FX_Dataset in FX_Data_List){
      # FX_Dataset<-FX_Data_List[str_detect(FX_Data_List,"CADCHF")%>%which()]
      
      print(FX_Dataset)
      
      forex_symbol<-FX_Dataset%>%str_split_i("_", i=1)
      
      if(forex_symbol%in%to_be_optimize_FX){
        
        # Upload the data for the FX symbol
        FX_Data<-read_feather(file.path(path_Input,Periodicity,FX_Dataset))#%>%slice_tail(n=(10000))
        
        if("SMA"%in%to_be_optimize_param){
          
          opt_results<-rBayesianOptimization::BayesianOptimization(
            FUN=function(short_sma, long_sma){
              optimize_SMA(short_sma, long_sma, forex_data=FX_Data)
            }
            ,bounds=list(
              short_sma=short_SMA
              ,long_sma=long_SMA
            )
            ,init_points=set_init_points
            ,n_iter=set_n_iter
            ,acq=set_acq
            ,kappa=set_kappa
          )
          
          best_params_bay<-opt_results$Best_Par
          
          best_params_bay_tbl<-tibble(symbol=forex_symbol
                                      ,short_sma=best_params_bay[1]
                                      ,long_sma=best_params_bay[2])
          
          par_metr_bay_SMA<-par_metr_bay_SMA%>%bind_rows(best_params_bay_tbl)
          
          if(optim_ga){
            
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
            
            best_params_ga_tbl<-tibble(symbol=forex_symbol
                                       ,short_sma=x1
                                       ,long_sma=x2)
            
            par_metr_ga_SMA<-par_metr_ga_SMA%>%bind_rows(best_params_ga_tbl)
          }
          
        }
        gc()
        if("EMA"%in%to_be_optimize_param){
          opt_results<-rBayesianOptimization::BayesianOptimization(
            FUN=function(short_ema, long_ema){
              optimize_EMA(short_ema, long_ema, forex_data=FX_Data)
            }
            ,bounds=list(
              short_ema=short_EMA
              ,long_ema=long_EMA
            )
            ,init_points=set_init_points
            ,n_iter=set_n_iter
            ,acq=set_acq
            ,kappa=set_kappa
          )
          
          best_params_bay<-opt_results$Best_Par
          
          best_params_bay_tbl<-tibble(symbol=forex_symbol
                                      ,short_ema=best_params_bay[1]
                                      ,long_ema=best_params_bay[2])
          
          par_metr_bay_EMA<-par_metr_bay_EMA%>%bind_rows(best_params_bay_tbl)
          
          if(optim_ga){
            
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
            
            best_params_ga_tbl<-tibble(symbol=forex_symbol
                                       ,short_ema=x1
                                       ,long_ema=x2)
            
            par_metr_ga_EMA<-par_metr_ga_EMA%>%bind_rows(best_params_ga_tbl)
            
          }
        }
        gc()
        if("MACD"%in%to_be_optimize_param){
          opt_results<-rBayesianOptimization::BayesianOptimization(
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
          
          best_params_bay_tbl<-tibble(symbol=forex_symbol
                                      ,macd_nF=best_params_bay[1]
                                      ,macd_nSl=best_params_bay[2]
                                      ,macd_nSi=best_params_bay[3])
          
          par_metr_bay_MACD<-par_metr_bay_MACD%>%bind_rows(best_params_bay_tbl)
          
          if(optim_ga){
            
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
            
            best_params_ga_tbl<-tibble(symbol=forex_symbol
                                       ,macd_nF=x1
                                       ,macd_nSl=x2
                                       ,macd_nSi=x3)
            
            par_metr_ga_MACD<-par_metr_ga_MACD%>%bind_rows(best_params_ga_tbl)
            
          }
          
        }
        gc()
        if("RSI"%in%to_be_optimize_param){
          opt_results<-rBayesianOptimization::BayesianOptimization(
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
          
          best_params_bay_tbl<-tibble(symbol=forex_symbol
                                      ,rsi_n=best_params_bay[1])
          
          par_metr_bay_RSI<-par_metr_bay_RSI%>%bind_rows(best_params_bay_tbl)
          
          if(optim_ga){
            
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
            
            best_params_ga_tbl<-tibble(symbol=forex_symbol
                                       ,rsi_n=x1)
            
            par_metr_ga_RSI<-par_metr_ga_RSI%>%bind_rows(best_params_ga_tbl)
            
          }
        }
        gc()
        if("STOCH"%in%to_be_optimize_param){
          opt_results<-rBayesianOptimization::BayesianOptimization(
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
          
          best_params_bay_tbl<-tibble(symbol=forex_symbol
                                      ,stoch_nFk=best_params_bay[1]
                                      ,stoch_nFd=best_params_bay[2]
                                      ,stoch_nSd=best_params_bay[3]
                                      ,stoch_smooth=best_params_bay[4])
          
          par_metr_bay_STOCH<-par_metr_bay_STOCH%>%bind_rows(best_params_bay_tbl)
          
          if(optim_ga){
            
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
            
            best_params_ga_tbl<-tibble(symbol=forex_symbol
                                       ,stoch_nFk=x1
                                       ,stoch_nFd=x2
                                       ,stoch_nSd=x3
                                       ,stoch_smooth=x4)
            
            par_metr_ga_STOCH<-par_metr_ga_STOCH%>%bind_rows(best_params_ga_tbl)
            
          }
        }
        gc()
        if("BBands"%in%to_be_optimize_param){
          opt_results<-rBayesianOptimization::BayesianOptimization(
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
          
          best_params_bay_tbl<-tibble(symbol=forex_symbol
                                      ,BB_n=best_params_bay[1]
                                      ,BB_sd=best_params_bay[2])
          
          par_metr_bay_BBands<-par_metr_bay_BBands%>%bind_rows(best_params_bay_tbl)
          
          if(optim_ga){
            
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
            
            best_params_ga_tbl<-tibble(symbol=forex_symbol
                                       ,BB_n=x1
                                       ,BB_sd=x2)
            
            par_metr_ga_BBands<-par_metr_ga_BBands%>%bind_rows(best_params_ga_tbl)
            
          }
          
        }
        gc()
        if("ADX"%in%to_be_optimize_param){
          opt_results<-rBayesianOptimization::BayesianOptimization(
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
          
          best_params_bay_tbl<-tibble(symbol=forex_symbol
                                      ,adx_n=best_params_bay[1])
          
          par_metr_bay_ADX<-par_metr_bay_ADX%>%bind_rows(best_params_bay_tbl)
          
          if(optim_ga){
            
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
            
            best_params_ga_tbl<-tibble(symbol=forex_symbol
                                       ,adx_n=x1)
            
            par_metr_ga_ADX<-par_metr_ga_ADX%>%bind_rows(best_params_ga_tbl)
            
          }
          
        }
        gc()
        
      }
      
      pb$tick()
    }
    
    list_par_metr_bay<-list("SMA"=par_metr_bay_SMA%>%arrange(symbol)%>%
                              mutate_if(is.numeric, round, digits=0)
                            ,"EMA"=par_metr_bay_EMA%>%arrange(symbol)%>%
                              mutate_if(is.numeric, round, digits=0)
                            ,"MACD"=par_metr_bay_MACD%>%arrange(symbol)%>%
                              mutate_if(is.numeric, round, digits=0)
                            ,"RSI"=par_metr_bay_RSI%>%arrange(symbol)%>%
                              mutate_if(is.numeric, round, digits=0)
                            ,"STOCH"=par_metr_bay_STOCH%>%arrange(symbol)%>%
                              mutate_if(is.numeric, round, digits=0)
                            ,"BBands"=par_metr_bay_BBands%>%arrange(symbol)%>%
                              mutate_if(is.numeric, round, digits=0)
                            ,"ADX"=par_metr_bay_ADX%>%arrange(symbol)%>%
                              mutate_if(is.numeric, round, digits=0)
    )
    
    writexl::write_xlsx(x=list_par_metr_bay
                        ,path=file.path(path_parameters
                                        , paste0(Sys.Date()
                                                 , "_parameters_metrics_bay_", Periodicity,".xlsx")))
    
    optim_results[[Periodicity]][["BAY"]]<-list_par_metr_bay
    
    if(optim_ga){
      
      list_par_metr_ga<-list("SMA"=par_metr_ga_SMA%>%arrange(symbol)%>%
                               mutate_if(is.numeric, round, digits=0)
                             ,"EMA"=par_metr_ga_EMA%>%arrange(symbol)%>%
                               mutate_if(is.numeric, round, digits=0)
                             ,"MACD"=par_metr_ga_MACD%>%arrange(symbol)%>%
                               mutate_if(is.numeric, round, digits=0)
                             ,"RSI"=par_metr_ga_RSI%>%arrange(symbol)%>%
                               mutate_if(is.numeric, round, digits=0)
                             ,"STOCH"=par_metr_ga_STOCH%>%arrange(symbol)%>%
                               mutate_if(is.numeric, round, digits=0)
                             ,"BBands"=par_metr_ga_BBands%>%arrange(symbol)%>%
                               mutate_if(is.numeric, round, digits=0)
                             ,"ADX"=par_metr_ga_ADX%>%arrange(symbol)%>%
                               mutate_if(is.numeric, round, digits=0)
      )
      
      writexl::write_xlsx(x=list_par_metr_ga
                          ,path=file.path(path_parameters
                                          , paste0(Sys.Date()
                                                   , "_parameters_metrics_ga_", Periodicity,".xlsx")))
      
      optim_results[[Periodicity]][["GA"]]<-list_par_metr_ga
    }
    
    print(paste0("FX ",Periodicity," metrics tested!"))
  }
  
  return(optim_results)
  
}



# # "CADCHF", "EURCHF", "GBPCHF", "USDCHF"
# test_metrics<-fnc_test_metrics_FX_Data(to_be_optimize_FX=c("CADCHF")
#                                        , to_be_optimize_param=c("SMA", "EMA", "MACD", "RSI", "STOCH", "BBands", "ADX")
#                                        , optim_ga=FALSE)

fnc_mlrMBO_metrics_FX_Data<-function(to_be_optimize_FX=c("CADCHF", "EURCAD", "EURCHF", "EURGBP", "EURUSD", "GBPCAD", "GBPCHF", "GBPUSD", "USDCAD", "USDCHF")
                                   , to_be_optimize_param=c("SMA", "EMA", "MACD", "RSI", "STOCH", "BBands", "ADX")
                                   , path_Input=path_input_forexSb
                                   , path_Parameters=path_parameters){
  
  # path_Input=path_input_forexSb; path_Parameters=path_parameters; to_be_optimize_param=c("SMA", "EMA", "MACD", "RSI", "STOCH", "BBands", "ADX"); to_be_optimize_FX=c("CADCHF", "EURCAD", "EURCHF", "EURGBP", "EURUSD", "GBPCAD", "GBPCHF", "GBPUSD", "USDCAD", "USDCHF")

  {
    # Configura learner e controllo MBO
    ctrl<-makeMBOControl()
    ctrl<-setMBOControlTermination(ctrl, iters=50) # Numero iterazioni
    ctrl<-setMBOControlInfill(ctrl, crit=makeMBOInfillCritCB()) # Confidence Bound per esplorazione/sfruttamento
    
    # Modello di regressione: Random Forest per ridurre il rumore e migliorare la robustezza
    lrn<-makeLearner("regr.randomForest", predict.type="se")
    
    # Parallelizzazione compatibile Windows
    parallelStartSocket(parallel::detectCores() - 1)
    
    # Carica i pacchetti anche nei worker paralleli
    packages<-c("tidyverse", "magrittr", "tidyquant", "tidymodels")
    lapply(packages, parallelLibrary)
  }
  
  {
    short_SMA<-c(5, 30); long_SMA<-c(35, 120); short_EMA<-c(5, 30); long_EMA<-c(35, 120)
    MACD_nF=c(10, 15); MACD_nSl=c(20, 30); MACD_nSi=c(5, 10)
    RSI_n=c(10, 25)
    STOCH_nFk=c(10, 20); STOCH_nFd=c(2, 5); STOCH_nSd=c(2, 5); STOCH_smooth=c(1, 2)
    B_B_n=c(15, 25); B_B_sd=c(2,3)
    ADX_n=c(10, 25)
  }
  
  optim_results<-list()
  
  for(Periodicity in c("H1")){
    # Periodicity<-"H1"
    
    file_param_metrics<-file.path(path_Parameters, paste0("parameters_metrics_", Periodicity,".xlsx"))
    
    {
      par_metr_mlr_SMA<-readxl::read_xlsx(file_param_metrics, sheet="SMA")
      par_metr_mlr_EMA<-readxl::read_xlsx(file_param_metrics, sheet="EMA")
      par_metr_mlr_MACD<-readxl::read_xlsx(file_param_metrics, sheet="MACD")
      par_metr_mlr_RSI<-readxl::read_xlsx(file_param_metrics, sheet="RSI")
      par_metr_mlr_STOCH<-readxl::read_xlsx(file_param_metrics, sheet="STOCH")
      par_metr_mlr_BBands<-readxl::read_xlsx(file_param_metrics, sheet="BBands")
      par_metr_mlr_ADX<-readxl::read_xlsx(file_param_metrics, sheet="ADX")
    }
    
    # list FOREX data DOWNLOADED
    FX_Data_List<-list.files(file.path(path_Input, Periodicity),pattern=".feather$")
    
    pb<-progress_bar$new(total=(length(FX_Data_List)))
    
    for(FX_Dataset in FX_Data_List){
      # FX_Dataset<-FX_Data_List[str_detect(FX_Data_List,"CADCHF")%>%which()]
      
      print(FX_Dataset)
      
      forex_symbol<-FX_Dataset%>%str_split_i("_", i=1)
      
      if(forex_symbol%in%to_be_optimize_FX){
        
        # Upload the data for the FX symbol
        FX_Data<-read_feather(file.path(path_Input,Periodicity,FX_Dataset))#%>%slice_tail(n=(1000))
        
        # Esporta funzioni e oggetti usati nella funzione obiettivo
        parallelExport(
          "calculate_omega_ratio", "fun_composite_score", "fun_ichimoku", "optimize_SMA", "optimize_EMA", "optimize_MACD", "optimize_RSI", "optimize_STOCH", "optimize_BBands", "optimize_ADX", "FX_Data"
        )
        
        if("SMA"%in%to_be_optimize_param){
          
          #️ Definizione funzione obiettivo (minimizziamo il -score)
          fun_obj_mlrMBO<-makeSingleObjectiveFunction(
            name="SMA_Optimization"
            ,fn=function(x){
              library(tidyverse); library(magrittr); library(tidyquant); library(tidymodels)
              
              # Aggiunta di un controllo per verificare i dati
              stopifnot(exists("FX_Data"))
              
              result<-optimize_SMA(x[["short_sma"]], x[["long_sma"]]
                                   , forex_data=FX_Data
              )
              return(result$Score*(-1))  # Minimizzazione
            }
            ,par.set=makeParamSet(
              makeIntegerParam("short_sma", lower=short_SMA[1], upper=short_SMA[2])
              ,makeIntegerParam("long_sma", lower=long_SMA[1], upper=long_SMA[2])
            )
            ,minimize=TRUE
          )
          
          # Esegui l’ottimizzazione
          res<-mbo(fun=fun_obj_mlrMBO, learner=lrn, control=ctrl)
          
          best_params_mlr_tbl<-tibble(symbol=forex_symbol
                                      ,short_sma=res$x$short_sma
                                      ,long_sma=res$x$long_sma
                                      ,score=(res$y*(-100))
                                      )
          
          par_metr_mlr_SMA<-par_metr_mlr_SMA%>%bind_rows(best_params_mlr_tbl)

        }
        gc()
        if("EMA"%in%to_be_optimize_param){
          #️ Definizione funzione obiettivo (minimizziamo il -score)
          fun_obj_mlrMBO<-makeSingleObjectiveFunction(
            name="EMA_Optimization"
            ,fn=function(x){
              library(tidyverse); library(magrittr); library(tidyquant); library(tidymodels)
              
              # Aggiunta di un controllo per verificare i dati
              stopifnot(exists("FX_Data"))
              
              result<-optimize_SMA(x[["short_ema"]], x[["long_ema"]]
                                   , forex_data=FX_Data
              )
              return(result$Score*(-1))  # Minimizzazione
            }
            ,par.set=makeParamSet(
              makeIntegerParam("short_ema", lower=short_EMA[1], upper=short_EMA[2])
              ,makeIntegerParam("long_ema", lower=long_EMA[1], upper=long_EMA[2])
            )
            ,minimize=TRUE
          )
          
          # Esegui l’ottimizzazione
          res<-mbo(fun=fun_obj_mlrMBO, learner=lrn, control=ctrl)
          
          best_params_mlr_tbl<-tibble(symbol=forex_symbol
                                      ,short_ema=res$x$short_ema
                                      ,long_ema=res$x$long_ema
                                      ,score=(res$y*(-100))
          )
          
          par_metr_mlr_EMA<-par_metr_mlr_EMA%>%bind_rows(best_params_mlr_tbl)

        }
        gc()
        if("MACD"%in%to_be_optimize_param){
          #️ Definizione funzione obiettivo (minimizziamo il -score)
          fun_obj_mlrMBO<-makeSingleObjectiveFunction(
            name="MACD_Optimization"
            ,fn=function(x){
              library(tidyverse); library(magrittr); library(tidyquant); library(tidymodels)
              
              # Aggiunta di un controllo per verificare i dati
              stopifnot(exists("FX_Data"))
              
              result<-optimize_MACD(x[["macd_nF"]], x[["macd_nSl"]], x[["macd_nSi"]]
                                   , forex_data=FX_Data
              )
              return(result$Score*(-1))  # Minimizzazione
            }
            ,par.set=makeParamSet(
              makeIntegerParam("macd_nF", lower=MACD_nF[1], upper=MACD_nF[2])
              ,makeIntegerParam("macd_nSl", lower=MACD_nSl[1], upper=MACD_nSl[2])
              ,makeIntegerParam("macd_nSi", lower=MACD_nSi[1], upper=MACD_nSi[2])
            )
            ,minimize=TRUE
          )
          
          # Esegui l’ottimizzazione
          res<-mbo(fun=fun_obj_mlrMBO, learner=lrn, control=ctrl)
          
          best_params_mlr_tbl<-tibble(symbol=forex_symbol
                                      ,macd_nF=res$x$macd_nF
                                      ,macd_nSl=res$x$macd_nSl
                                      ,macd_nSi=res$x$macd_nSi
                                      ,score=(res$y*(-100))
          )
          
          par_metr_mlr_MACD<-par_metr_mlr_MACD%>%bind_rows(best_params_mlr_tbl)

        }
        gc()
        if("RSI"%in%to_be_optimize_param){
          #️ Definizione funzione obiettivo (minimizziamo il -score)
          fun_obj_mlrMBO<-makeSingleObjectiveFunction(
            name="RSI_Optimization"
            ,fn=function(x){
              library(tidyverse); library(magrittr); library(tidyquant); library(tidymodels)
              
              # Aggiunta di un controllo per verificare i dati
              stopifnot(exists("FX_Data"))
              
              result<-optimize_RSI(x[["rsi_n"]]
                                    , forex_data=FX_Data
              )
              return(result$Score*(-1))  # Minimizzazione
            }
            ,par.set=makeParamSet(
              makeIntegerParam("rsi_n", lower=RSI_n[1], upper=RSI_n[2])
            )
            ,minimize=TRUE
          )
          
          # Esegui l’ottimizzazione
          res<-mbo(fun=fun_obj_mlrMBO, learner=lrn, control=ctrl)
          
          best_params_mlr_tbl<-tibble(symbol=forex_symbol
                                      ,rsi_n=res$x$rsi_n
                                      ,score=(res$y*(-100))
          )
          
          par_metr_mlr_RSI<-par_metr_mlr_RSI%>%bind_rows(best_params_mlr_tbl)
          
        }
        gc()
        if("STOCH"%in%to_be_optimize_param){
          #️ Definizione funzione obiettivo (minimizziamo il -score)
          fun_obj_mlrMBO<-makeSingleObjectiveFunction(
            name="STOCH_Optimization"
            ,fn=function(x){
              library(tidyverse); library(magrittr); library(tidyquant); library(tidymodels)
              
              # Aggiunta di un controllo per verificare i dati
              stopifnot(exists("FX_Data"))
              
              result<-optimize_STOCH(x[["stoch_nFk"]]
                                    , x[["stoch_nFd"]]
                                    , x[["stoch_nSd"]]
                                    , x[["stoch_smooth"]]
                                    , forex_data=FX_Data
              )
              return(result$Score*(-1))  # Minimizzazione
            }
            ,par.set=makeParamSet(
              makeIntegerParam("stoch_nFk", lower=STOCH_nFk[1], upper=STOCH_nFk[2])
              ,makeIntegerParam("stoch_nFd", lower=STOCH_nFd[1], upper=STOCH_nFd[2])
              ,makeIntegerParam("stoch_nSd", lower=STOCH_nSd[1], upper=STOCH_nSd[2])
              ,makeIntegerParam("stoch_smooth", lower=STOCH_smooth[1], upper=STOCH_smooth[2])
            )
            ,minimize=TRUE
          )
          
          # Esegui l’ottimizzazione
          res<-mbo(fun=fun_obj_mlrMBO, learner=lrn, control=ctrl)
          
          best_params_mlr_tbl<-tibble(symbol=forex_symbol
                                      ,stoch_nFk=res$x$stoch_nFk
                                      ,stoch_nFd=res$x$stoch_nFd
                                      ,stoch_nSd=res$x$stoch_nSd
                                      ,stoch_smooth=res$x$stoch_smooth
                                      ,score=(res$y*(-100))
          )
          
          par_metr_mlr_STOCH<-par_metr_mlr_STOCH%>%bind_rows(best_params_mlr_tbl)
          
        }
        gc()
        if("BBands"%in%to_be_optimize_param){
          #️ Definizione funzione obiettivo (minimizziamo il -score)
          fun_obj_mlrMBO<-makeSingleObjectiveFunction(
            name="BBands_Optimization"
            ,fn=function(x){
              library(tidyverse); library(magrittr); library(tidyquant); library(tidymodels)
              
              # Aggiunta di un controllo per verificare i dati
              stopifnot(exists("FX_Data"))
              
              result<-optimize_BBands(x[["BB_n"]]
                                    , x[["BB_sd"]]
                                    , forex_data=FX_Data
              )
              return(result$Score*(-1))  # Minimizzazione
            }
            ,par.set=makeParamSet(
              makeIntegerParam("BB_n", lower=B_B_n[1], upper=B_B_n[2])
              ,makeIntegerParam("BB_sd", lower=B_B_sd[1], upper=B_B_sd[2])
            )
            ,minimize=TRUE
          )
          
          # Esegui l’ottimizzazione
          res<-mbo(fun=fun_obj_mlrMBO, learner=lrn, control=ctrl)
          
          best_params_mlr_tbl<-tibble(symbol=forex_symbol
                                      ,BB_n=res$x$BB_n
                                      ,BB_sd=res$x$BB_sd
                                      ,score=(res$y*(-100))
          )
          
          par_metr_mlr_BBands<-par_metr_mlr_BBands%>%bind_rows(best_params_mlr_tbl)

        }
        gc()
        if("ADX"%in%to_be_optimize_param){
          #️ Definizione funzione obiettivo (minimizziamo il -score)
          fun_obj_mlrMBO<-makeSingleObjectiveFunction(
            name="ADX_Optimization"
            ,fn=function(x){
              library(tidyverse); library(magrittr); library(tidyquant); library(tidymodels)
              
              # Aggiunta di un controllo per verificare i dati
              stopifnot(exists("FX_Data"))
              
              result<-optimize_ADX(x[["adx_n"]]
                                    , forex_data=FX_Data
              )
              return(result$Score*(-1))  # Minimizzazione
            }
            ,par.set=makeParamSet(
              makeIntegerParam("adx_n", lower=ADX_n[1], upper=ADX_n[2])
            )
            ,minimize=TRUE
          )
          
          # Esegui l’ottimizzazione
          res<-mbo(fun=fun_obj_mlrMBO, learner=lrn, control=ctrl)
          
          best_params_mlr_tbl<-tibble(symbol=forex_symbol
                                      ,adx_n=res$x$adx_n
                                      ,score=(res$y*(-100))
          )
          
          par_metr_mlr_ADX<-par_metr_mlr_ADX%>%bind_rows(best_params_mlr_tbl)

        }
        gc()
        
      }
      
      pb$tick()
    }
    
    # Ferma i cluster
    parallelStop()
    
    # Esporta e salva i risultati
    list_par_metr_mlr<-list("SMA"=par_metr_mlr_SMA%>%arrange(symbol)%>%
                              mutate_if(is.numeric, round, digits=0)
                            ,"EMA"=par_metr_mlr_EMA%>%arrange(symbol)%>%
                              mutate_if(is.numeric, round, digits=0)
                            ,"MACD"=par_metr_mlr_MACD%>%arrange(symbol)%>%
                              mutate_if(is.numeric, round, digits=0)
                            ,"RSI"=par_metr_mlr_RSI%>%arrange(symbol)%>%
                              mutate_if(is.numeric, round, digits=0)
                            ,"STOCH"=par_metr_mlr_STOCH%>%arrange(symbol)%>%
                              mutate_if(is.numeric, round, digits=0)
                            ,"BBands"=par_metr_mlr_BBands%>%arrange(symbol)%>%
                              mutate_if(is.numeric, round, digits=0)
                            ,"ADX"=par_metr_mlr_ADX%>%arrange(symbol)%>%
                              mutate_if(is.numeric, round, digits=0)
    )
    
    writexl::write_xlsx(x=list_par_metr_mlr
                        ,path=file.path(path_parameters
                                        , paste0(Sys.Date()
                                                 , "_parameters_metrics_mlr_", Periodicity,".xlsx")))
    
    optim_results[[Periodicity]][["MLR"]]<-list_par_metr_mlr

    print(paste0("FX ",Periodicity," metrics tested!"))
  }
  
  return(optim_results)
  
}

# "CADCHF", "EURCAD", "EURCHF", "EURGBP", "EURUSD", "GBPCAD", "GBPCHF", "GBPUSD", "USDCAD", "USDCHF"
mlrMBO_metrics<-fnc_mlrMBO_metrics_FX_Data(to_be_optimize_FX=c("EURCAD")
                                       , to_be_optimize_param=c("SMA", "EMA", "MACD", "RSI", "STOCH", "BBands", "ADX"))

