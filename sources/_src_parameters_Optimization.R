# Installazione e caricamento dei pacchetti necessari
if (!requireNamespace("TTR",quietly=TRUE)) {
  install.packages("TTR")
}
if (!requireNamespace("rBayesianOptimization",quietly=TRUE)) {
  install.packages("rBayesianOptimization")
}
library(TTR)
library(rBayesianOptimization)

rm(list=ls())
gc()
fnc_RSI_param_BayOptim<-function(path_Input=path_input_tqGet,path_Output=path_output_metrics,overwrite_Metrics=TRUE){
  # path_Input=path_input_tqGet; path_Output=path_output_metrics
  
  maType_vec<-c("SMA","EMA"#,"DEMA","WMA","ZLEMA","HMA"
                )
  for(Periodicity in c("daily","weekly")){
    # Periodicity<-"daily"
    
    tbl_RSI_param<-tibble(Period=as.character(NA)
                          ,FX=as.character(NA)
                          ,MA_type=as.character(NA)
                          ,Par_Bay=as.numeric(NA)
                          ,Value_Bay=as.numeric(NA))
    
    # list FOREX data DOWNLOADED
    FX_Data_List<-list.files(file.path(path_Input,Periodicity),pattern=".*Rds")
    
    pb<-progress_bar$new(total=(length(FX_Data_List)))
    
    for(FX_Dataset in FX_Data_List){
      # FX_Dataset<-FX_Data_List[str_detect(FX_Data_List,"EURCHF")%>%which()]
      FX_data<-read_rds(file.path(path_Input,Periodicity,FX_Dataset))
      data<-FX_data$close%>%na.omit()
      
      for(maType in maType_vec){
        print(maType)
        # Funzione obiettivo per BayesianOptimization
        objectiveFunction_Bay<-function(n,selectedMaType=maType){
          rsi_values<-RSI(data,n=n,maType=selectedMaType)
          error<-mean((data-rsi_values)^2,na.rm=TRUE)
          return(list(Score=-error,Pred=0))
        }
        
        objectiveFunction<-function(n,selectedMaType=maType){
          rsi_values<-RSI(data,n=n,maType=selectedMaType)
          error<-mean((data-rsi_values)^2,na.rm=TRUE)
          return(error)
        }
        
        # Definisci i limiti dei parametri
        bounds<-list(n=c(10,100))
        
        # Esecuzione dell'ottimizzazione con BayesianOptimization
        results_Bay<-BayesianOptimization(
          FUN=objectiveFunction_Bay
          ,bounds=bounds
          ,init_points=20
          ,n_iter=100
          ,acq="ei"
        )
        
        # result_DEo<-DEoptim(objectiveFunction,lower=10,upper=100)
        # result_Gen<-GenSA(lower=10, upper=100, fn=objectiveFunction)
        # result_nlo<-nloptr(x0=25
        #                    ,eval_f=objectiveFunction
        #                    ,selectedMaType=maType
        #                    ,lb=10,ub=100
        #                    ,opts=list(algorithm="NLOPT_LN_SBPLX",xtol_rel=1e-8)
        #                    )
        
        tbl_RSI_param%<>%add_row(tibble(Period=Periodicity
                                        ,FX=FX_data$symbol%>%unique()
                                        ,MA_type=maType
                                        ,Par_Bay=results_Bay$Best_Par
                                        ,Value_Bay=results_Bay$Best_Value))%>%
          na.omit()%>%
          distinct()
      }
      pb$tick()
    }
    saveRDS(tbl_RSI_param,file.path(path_Output,Periodicity,paste0("param_RSI_BayOptim_",Periodicity,".Rds")))
    message(Periodicity)
  }
  return(NA)
}


