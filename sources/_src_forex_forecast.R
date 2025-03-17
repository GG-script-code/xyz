#' https://www.tidymodels.org/learn/models/time-series/

# parameters 
{
  time_delta<-months(36)
}

fnc_uniV_forecast<-function(FX_Selected="CADCHF"
                            ,FX_Periodicity="daily" # "daily" "weekly"
                            ,time_Delta=time_delta
                            ,pred_variable="RSI_10"
                            ,forecast_period="5 days"
                            ,path_Output_Metrics=path_output_metrics){
  
  # FX_Selected="GBPUSD"; FX_Periodicity="daily"; time_Delta=time_delta; pred_variable="open"; forecast_period="5 days"; path_Output_Metrics=path_output_metrics
  
  FX_symbol<-FX_Selected%>%
    as.character()%>%
    str_split_i("[.]",i=1)
  
  time_Filter<-(Sys.Date()%m-%time_Delta)
  
  FX_Metrics_List<-list.files(file.path(path_Output_Metrics,FX_Periodicity),pattern="^2.*Rds")
  FX_Metrics_File<-FX_Metrics_List[str_detect(FX_Metrics_List,FX_symbol)%>%which()]
  FX_Metrics_Data<-read_rds(file.path(path_Output_Metrics,FX_Periodicity,FX_Metrics_File))%>%
    filter(date>=time_Filter)
  forex_data<-FX_Metrics_Data%>%
    drop_na()%>%
    select(c("date",all_of(pred_variable)))#%>%as_tsibble(index=date)
  
  colnames(forex_data)<-c("date","value")
  
  # Divisione dei dati in set di addestramento e test 
  set.seed(123)
  data_split<-initial_time_split(forex_data,prop=0.8) 
  train_data<-training(data_split) 
  test_data<-testing(data_split)
  
  # Rolling Cross-Validation (per valutare i modelli)
  reSamples<-time_series_cv(
    forex_data,
    ,initial="2 years" # 2 anni per il training iniziale
    ,assess="6 months" # 6 mesi per il test
    ,skip="1 months" # Salta 3 mesi tra una finestra e l'altra
    ,cumulative=TRUE
  )
  
  # Crea modelli di forecast
  # Modello ARIMA
  model_arima<-arima_reg(seasonal_period=c("weekly"))%>%
    set_engine("auto_arima"
               , stepwise=FALSE
               , approximation=FALSE)%>%
    fit(value~date,data=train_data)
  
  # Modello ETS (Exponential Smoothing)
  model_ets<-exp_smoothing(seasonal_period=c("weekly"))%>%
    set_engine("ets")%>%
    fit(value~date,data=train_data)
  
  # Modello Prophet
  model_prophet<-prophet_reg(seasonality_weekly=TRUE)%>%
    set_engine("prophet")%>%
    fit(value~date,data=train_data)
  
  # Modello NNETAR (Rete Neurale)
  model_nnetar<-nnetar_reg(seasonal_period=c("weekly"))%>%
    set_engine("nnetar")%>%
    fit(value~date,train_data)
  
  # Modello Random Forest
  model_rf<-rand_forest(mode="regression"
    ,trees=500
  )%>%
    set_engine("ranger")%>%
    fit(value~date,train_data)
  
  # # Modello XGBoost
  # model_xgb<-boost_tree(
  #   mode="regression"
  #   ,trees=500
  #   ,learn_rate=0.1
  # )%>%
  #   set_engine("xgboost")%>%
  #   fit(value~date,train_data%>%
  #         future_frame("day",length_out=30))
  
  # Crea una tabella dei modelli
  models_tbl<-modeltime_table(
    model_arima
    ,model_ets
    ,model_prophet
    ,model_nnetar
    ,model_rf
    #,model_xgb
  )
  
  # Step 5: Valutazione con Cross-Validation
  cv_results<-models_tbl%>%
    modeltime_fit_resamples(
      resamples=reSamples
      ,control=control_resamples(verbose=TRUE
                                 ,allow_par=FALSE) # Evita problemi con il parallelismo
    )
  
  # Calcola le metriche di performance
  cv_metrics<-cv_results%>%
    modeltime_resample_accuracy()
  
  cv_metrics<-cv_metrics[,colSums(is.na(cv_metrics))==0]
  
  # Calcola i pesi proporzionali inversi all'RMSE
  weights<-cv_metrics%>%
    mutate(weight_mae=((1/mae)/sum(1/mae))# Normalizza i pesi
           ,weight_mape=((1/mape)/sum(1/mape))
           ,weight_mase=((1/mase)/sum(1/mase))
           ,weight_smape=((1/smape)/sum(1/smape))
           ,weight_rmse=((1/rmse)/sum(1/rmse)))%>%
    mutate(weight=((weight_mae+weight_mape+weight_mase+weight_smape+weight_rmse)/5)) 
  
  # Combina i modelli usando i pesi calcolati
  ensemble_model<-modeltime.ensemble::ensemble_weighted(
    object=models_tbl
    ,loadings=weights$weight
    ,scale_loadings=TRUE
  )
  
  # 5. Calibrazione dell'Ensemble sui dati di test
  ensemble_calibrated<-ensemble_model%>%
    modeltime_calibrate(new_data=test_data)
  
  # 7. Previsioni e visualizzazione
  frc_ENS_tbl<-ensemble_calibrated%>%
    modeltime_forecast(
      ,new_data=test_data
      ,actual_data=forex_data
      ,h=forecast_period
    )%>%
    #drop_na()%>%
    group_by(.index)%>%
    summarise(.value=mean(.value))%>%
    slice_tail(n=6)
  
  colnames(frc_ENS_tbl)<-c("date", pred_variable)
  # frc_ENS_tbl
  return(frc_ENS_tbl)
  
}

forex<-"CADCHF"
pred_vars<-c("open", "high", "low", "tenkan_sen", "kijun_sen", "senkou_span_a", "senkou_span_b", "chikou_span", "RSI_10", "RSI_14", "RSI_20", "macd_SMA", "signal_SMA", "histo_SMA", "macd_EMA", "signal_EMA", "histo_EMA", "BB_dn_SMA", "BB_mavg_SMA", "BB_up_SMA", "BB_pctB_SMA", "BB_dn_EMA", "BB_mavg_EMA", "BB_up_EMA", "BB_pctB_EMA", "stoch_SMA", "stoch_EMA", "fastK", "fastD", "ADX_SMA", "ADX_EMA", "DIp", "DIn", "DX", "volatility")

forecast_list<-list()

pb<-progress::progress_bar$new(total=length(pred_vars))
for(pred_var in pred_vars){
  pb$tick()
  forecast_list[[pred_var]]<-fnc_uniV_forecast(FX_Selected=forex, pred_variable=pred_var)
}

# saveRDS(forecast_list, file.path(path_output_forecast, "univariate", paste0("forecast_uni_", forex, ".Rds")))

forecast_list<-read_rds(file.path(path_output_forecast, "univariate", paste0("forecast_uni_", forex, ".Rds")))

forecast_tbl<-forecast_list$open%>%
  left_join(forecast_list$high, by=join_by("date"))%>%
  left_join(forecast_list$low, by=join_by("date"))%>%
  left_join(forecast_list$tenkan_sen, by=join_by("date"))%>%
  left_join(forecast_list$kijun_sen, by=join_by("date"))%>%
  left_join(forecast_list$senkou_span_a, by=join_by("date"))%>%
  left_join(forecast_list$senkou_span_b, by=join_by("date"))%>%
  left_join(forecast_list$chikou_span, by=join_by("date"))%>%
  left_join(forecast_list$RSI_10, by=join_by("date"))%>%
  left_join(forecast_list$RSI_14, by=join_by("date"))%>%
  left_join(forecast_list$RSI_20, by=join_by("date"))%>%
  left_join(forecast_list$macd_SMA, by=join_by("date"))%>%
  left_join(forecast_list$signal_SMA, by=join_by("date"))%>%
  left_join(forecast_list$histo_SMA, by=join_by("date"))%>%
  left_join(forecast_list$macd_EMA, by=join_by("date"))%>%
  left_join(forecast_list$signal_EMA, by=join_by("date"))%>%
  left_join(forecast_list$histo_EMA, by=join_by("date"))%>%
  left_join(forecast_list$BB_dn_SMA, by=join_by("date"))%>%
  left_join(forecast_list$BB_mavg_SMA, by=join_by("date"))%>%
  left_join(forecast_list$BB_up_SMA, by=join_by("date"))%>%
  left_join(forecast_list$BB_pctB_SMA, by=join_by("date"))%>%
  left_join(forecast_list$BB_dn_EMA, by=join_by("date"))%>%
  left_join(forecast_list$BB_mavg_EMA, by=join_by("date"))%>%
  left_join(forecast_list$BB_up_EMA, by=join_by("date"))%>%
  left_join(forecast_list$BB_pctB_EMA, by=join_by("date"))%>%
  left_join(forecast_list$stoch_SMA, by=join_by("date"))%>%
  left_join(forecast_list$stoch_EMA, by=join_by("date"))%>%
  left_join(forecast_list$fastK, by=join_by("date"))%>%
  left_join(forecast_list$fastD, by=join_by("date"))%>%
  left_join(forecast_list$ADX_SMA, by=join_by("date"))%>%
  left_join(forecast_list$ADX_EMA, by=join_by("date"))%>%
  left_join(forecast_list$DIp, by=join_by("date"))%>%
  left_join(forecast_list$DIn, by=join_by("date"))%>%
  left_join(forecast_list$DX, by=join_by("date"))%>%
  left_join(forecast_list$volatility, by=join_by("date"))

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
FX_Metrics_List<-list.files(file.path(path_output_metrics,"daily"),pattern="^2.*Rds")
FX_Metrics_File<-FX_Metrics_List[str_detect(FX_Metrics_List,forex)%>%which()]
FX_Forecast_Data<-read_rds(file.path(path_output_metrics,"daily",FX_Metrics_File))%>%
  filter(date>=(Sys.Date()%m-%time_delta)&
           date<min(forecast_tbl$date))%>%
  bind_rows(forecast_tbl)

# Divisione dei dati in set di addestramento e test 
set.seed(123)
data_split<-initial_time_split(FX_Forecast_Data,prop=0.8) 
train_data<-training(data_split) 
test_data<-testing(data_split)

# Ricetta per multivariate forecasting
recipe_forecast<-recipe(close~date+open+high+low+tenkan_sen+kijun_sen+senkou_span_a+senkou_span_b+chikou_span+RSI_10+RSI_14+RSI_20+macd_SMA+signal_SMA+histo_SMA+macd_EMA+signal_EMA+histo_EMA+BB_dn_SMA+BB_mavg_SMA+BB_up_SMA+BB_pctB_SMA+BB_dn_EMA+BB_mavg_EMA+BB_up_EMA+BB_pctB_EMA+stoch_SMA+stoch_EMA+fastK+fastD+ADX_SMA+ADX_EMA+DIp+DIn+DX+volatility
                          , data=train_data)%>%
  step_timeseries_signature(date)%>%
  step_rm(date)%>%  # Rimuove la colonna "date" dopo l'estrazione delle feature
  step_normalize(all_predictors(), -all_nominal())%>%
  step_dummy(all_nominal(),one_hot=TRUE)

#Step 4: Modelli multivariati
# Random Forest
rf_spec<-rand_forest(
  mtry=tune(),
  trees=tune(),
  min_n=tune()
)%>%
  set_engine("ranger")%>%
  set_mode("regression")

# XGBoost
boost_spec<-boost_tree(
  trees=tune(),
  tree_depth=tune(),
  learn_rate=tune(),
  loss_reduction=tune()
)%>%
  set_engine("xgboost")%>%
  set_mode("regression")

#Step 5: Griglia di tuning
# Griglia per Random Forest
rf_grid<-grid_regular(
  mtry(range=c(2, 5)),
  trees(range=c(500, 1000)),
  min_n(range=c(5, 20)),
  levels=5
)

# Griglia per XGBoost
boost_grid<-grid_regular(
  tree_depth(range=c(3, 10)),
  learn_rate(range=c(0.01, 0.3)),
  loss_reduction(range=c(0.01, 1)),
  trees(range=c(500, 1000)),
  levels=5
)

#Step 6: Cross-validation temporale
# Cross-validation temporale
reSamples<-time_series_cv(
  data=train_data,
  initial="3 years",
  assess="6 months",
  skip="1 month",
  cumulative=TRUE
)

#Step 7: Tuning dei modelli
# Tuning Random Forest
rf_tuned<-tune_grid(
  rf_spec,
  preprocessor=recipe_forecast,
  resamples=reSamples,
  grid=rf_grid,
  metrics=metric_set(rmse, mae, mape)
)

# Tuning XGBoost
boost_tuned<-tune_grid(
  boost_spec,
  preprocessor=recipe_forecast,
  resamples=reSamples,
  grid=boost_grid,
  metrics=metric_set(rmse, mae, mape)
)

#Step 8: Finalizza i modelli
# Migliori parametri per Random Forest
best_rf_params<-select_best(rf_tuned, metric="rmse")
final_rf<-finalize_model(rf_spec, best_rf_params)%>%
  fit(close~date+open+high+low+tenkan_sen+kijun_sen+senkou_span_a+senkou_span_b+chikou_span+RSI_10+RSI_14+RSI_20+macd_SMA+signal_SMA+histo_SMA+macd_EMA+signal_EMA+histo_EMA+BB_dn_SMA+BB_mavg_SMA+BB_up_SMA+BB_pctB_SMA+BB_dn_EMA+BB_mavg_EMA+BB_up_EMA+BB_pctB_EMA+stoch_SMA+stoch_EMA+fastK+fastD+ADX_SMA+ADX_EMA+DIp+DIn+DX+volatility, data=train_data)

# Migliori parametri per XGBoost
best_boost_params<-select_best(boost_tuned, metric="rmse")
final_boost<-finalize_model(boost_spec, best_boost_params)%>%
  fit(close~date+open+high+low+tenkan_sen+kijun_sen+senkou_span_a+senkou_span_b+chikou_span+RSI_10+RSI_14+RSI_20+macd_SMA+signal_SMA+histo_SMA+macd_EMA+signal_EMA+histo_EMA+BB_dn_SMA+BB_mavg_SMA+BB_up_SMA+BB_pctB_SMA+BB_dn_EMA+BB_mavg_EMA+BB_up_EMA+BB_pctB_EMA+stoch_SMA+stoch_EMA+fastK+fastD+ADX_SMA+ADX_EMA+DIp+DIn+DX+volatility, data=train_data)

#Step 9: Ensemble dei modelli
# Ensemble
ensemble_model<-modeltime_table(
  final_rf,
  final_boost
)%>%
  ensemble_average(type="mean")

#Step 10: Calibrazione e forecast per 5 giorni
# Calibrazione
calibration_tbl<-ensemble_model %>%
  modeltime_calibrate(test_data)

# Accuratezza
calibration_tbl %>%
  modeltime_accuracy()

# Previsione a 5 giorni
future_data<-cadchf_data%>%
  future_frame(.date_var=date, .length_out=5)%>%
  mutate(# Inserisci valori medi o stimati
    open=mean(train_data$open)
    ,high=mean(train_data$high)
    ,low=mean(train_data$low)
    ,tenkan_sen=mean(train_data$tenkan_sen)
    ,kijun_sen=mean(train_data$kijun_sen)
    ,senkou_span_a=mean(train_data$senkou_span_a)
    ,senkou_span_b=mean(train_data$senkou_span_b)
    ,chikou_span=mean(train_data$chikou_span)
    ,RSI_10=mean(train_data$RSI_10)
    ,RSI_14=mean(train_data$RSI_14)
    ,RSI_20=mean(train_data$RSI_20)
    ,macd_SMA=mean(train_data$macd_SMA)
    ,signal_SMA=mean(train_data$signal_SMA)
    ,histo_SMA=mean(train_data$histo_SMA)
    ,macd_EMA=mean(train_data$macd_EMA)
    ,signal_EMA=mean(train_data$signal_EMA)
    ,histo_EMA=mean(train_data$histo_EMA)
    ,BB_dn_SMA=mean(train_data$BB_dn_SMA)
    ,BB_mavg_SMA=mean(train_data$BB_mavg_SMA)
    ,BB_up_SMA=mean(train_data$BB_up_SMA)
    ,BB_pctB_SMA=mean(train_data$BB_pctB_SMA)
    ,BB_dn_EMA=mean(train_data$BB_dn_EMA)
    ,BB_mavg_EMA=mean(train_data$BB_mavg_EMA)
    ,BB_up_EMA=mean(train_data$BB_up_EMA)
    ,BB_pctB_EMA=mean(train_data$BB_pctB_EMA)
    ,stoch_SMA=mean(train_data$stoch_SMA)
    ,stoch_EMA=mean(train_data$stoch_EMA)
    ,fastK=mean(train_data$fastK)
    ,fastD=mean(train_data$fastD)
    ,ADX_SMA=mean(train_data$ADX_SMA)
    ,ADX_EMA=mean(train_data$ADX_EMA)
    ,DIp=mean(train_data$DIp)
    ,DIn=mean(train_data$DIn)
    ,DX=mean(train_data$DX)
    ,volatility=mean(train_data$volatility)
  )

# Forecast
forecast<-calibration_tbl %>%
  modeltime_forecast(
    new_data=future_data,
    actual_data=FX_Forecast_Data
  )

# Visualizza il forecast
forecast %>%
  plot_modeltime_forecast()





