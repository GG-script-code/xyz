#' https://www.tidymodels.org/learn/models/time-series/

# parameters 
{
  time_delta<-months(36)
}

fnc_multiV_forecast<-function(FX_Selected="CADCHF"
                              ,FX_Periodicity="daily" # "daily" "weekly"
                              ,time_Delta=time_delta
                              ,use_xgb=FALSE,use_rf=FALSE
                              ,path_Output_Metrics=path_output_metrics){
  
  # FX_Selected="GBPUSD"; FX_Periodicity="daily"; use_xgb=FALSE; use_rf=FALSE; path_Output_Metrics=path_output_metrics
  
  FX_symbol<-FX_Selected%>%
    as.character()%>%
    str_split_i("[.]",i=1)
  
  time_Filter<-(Sys.Date()%m-%time_Delta)
  
  FX_Metrics_List<-list.files(file.path(path_Output_Metrics,FX_Periodicity),pattern="^2.*Rds")
  FX_Metrics_File<-FX_Metrics_List[str_detect(FX_Metrics_List,FX_symbol)%>%which()]
  FX_Metrics_Data<-read_rds(file.path(path_Output_Metrics,FX_Periodicity,FX_Metrics_File))%>%
    filter(date>=time_Filter)
  forex_data<-FX_Metrics_Data%>%
    na.omit()%>%
    as_tsibble(index=date)
  
  # Divisione dei dati in set di addestramento e test 
  set.seed(123)
  data_split<-initial_time_split(forex_data,prop=0.8) 
  train_data<-training(data_split) 
  test_data<-testing(data_split)
  
  # Creare i resampling per il backtesting
  resamples<-rolling_origin(train_data,initial=100,assess=20,skip=10)
  
  recipe_formula<-recipe(close~open+high+low+adjusted+
                           tenkan_sen+kijun_sen+senkou_span_a+senkou_span_b+chikou_span+
                           RSI_10+RSI_14+RSI_20+
                           macd_SMA+signal_SMA+histo_SMA+macd_EMA+signal_EMA+histo_EMA+
                           BB_dn_SMA+BB_mavg_SMA+BB_up_SMA+
                           BB_pctB_SMA+BB_dn_EMA+BB_mavg_EMA+BB_up_EMA+BB_pctB_EMA+
                           fastK+fastD+stoch_SMA+stoch_EMA+
                           DIp+DIn+DX+ADX_SMA+ADX_EMA+
                           volatility
                         ,data=train_data)
  
  # lm ----
  {
    # Specificare il modello
    linear_spec<-linear_reg()%>%
      set_engine("lm")%>%
      set_mode("regression")
    
    # Creare il workflow
    linear_workflow<-workflow()%>%
      add_recipe(recipe_formula)%>%
      add_model(linear_spec)
    
    # Addestrare e valutare il modello sui resampling
    linear_results<-fit_resamples(
      linear_workflow
      ,resamples=roll_rs
      ,metrics=metric_set(rmse,rsq)
      ,control=control_resamples(save_pred=TRUE)
    )
    
    # Risultati del backtesting
    metrics_lm<-linear_results%>%
      collect_metrics()
    
    # Addestrare il modello
    linear_fit<-linear_workflow%>%
      fit(data=forex_data)
    
    # Previsioni
    forecast_linear<-linear_fit%>%
      predict(new_data=tail(forex_data,5))
    
    gc()
  }
  
  # elastic ----
  {
    # Specificare il modello
    elastic_spec<-linear_reg(penalty=0.1,mixture=0.5)%>%
      set_engine("glmnet")%>%
      set_mode("regression")
    
    # Creare il workflow
    elastic_workflow<-workflow()%>%
      add_recipe(recipe_formula)%>%
      add_model(elastic_spec)
    
    # Addestrare e valutare il modello sui resampling
    elastic_results<-fit_resamples(
      elastic_workflow
      ,resamples=roll_rs
      ,metrics=metric_set(rmse,rsq)
      ,control=control_resamples(save_pred=TRUE)
    )
    
    # Risultati del backtesting
    metrics_elastic<-elastic_results%>%
      collect_metrics()
    
    # Addestrare il modello
    elastic_fit<-elastic_workflow%>%
      fit(data=forex_data)
    
    # Previsioni
    forecast_elastic<-elastic_fit%>%
      predict(new_data=tail(forex_data,5))
    
    gc()
  }
  
  # mars ----
  {
    # Specificare il modello
    mars_spec<-mars()%>%
      set_engine("earth")%>%
      set_mode("regression")
    
    # Creare il workflow
    mars_workflow<-workflow()%>%
      add_recipe(recipe_formula)%>%
      add_model(mars_spec)
    
    # Addestrare e valutare il modello sui resampling
    mars_results<-fit_resamples(
      mars_workflow
      ,resamples=roll_rs
      ,metrics=metric_set(rmse,rsq)
      ,control=control_resamples(save_pred=TRUE)
    )
    
    # Risultati del backtesting
    metrics_mars<-mars_results%>%
      collect_metrics()
    
    # Addestrare il modello
    mars_fit<-mars_workflow%>%
      fit(data=forex_data)
    
    # Previsioni
    forecast_mars<-mars_fit%>%
      predict(new_data=tail(forex_data,5))
    
    gc()
  }
  
  # rf ----
  if(use_rf){
    {
      # Specificare il modello
      rf_spec<-rand_forest()%>%
        set_engine("ranger")%>%
        set_mode("regression")
      
      # Creare il workflow
      rf_workflow<-workflow()%>%
        add_recipe(recipe_formula)%>%
        add_model(rf_spec)
      
      # Addestrare e valutare il modello sui resampling
      rf_results<-fit_resamples(
        rf_workflow
        ,resamples=roll_rs
        ,metrics=metric_set(rmse,rsq)
        ,control=control_resamples(save_pred=TRUE)
      )
      
      # Risultati del backtesting
      metrics_rf<-rf_results%>%
        collect_metrics()
      
      # Addestrare il modello
      rf_fit<-rf_workflow%>%
        fit(data=forex_data)
      
      # Previsioni
      forecast_rf<-rf_fit%>%
        predict(new_data=tail(forex_data,5))
      
      gc()
    }
  }
  
  # xgb ----
  if(use_xgb){
    {
      # Specificare il modello
      xgb_spec<-boost_tree()%>%
        set_engine("xgboost")%>%
        set_mode("regression")
      
      # Creare il workflow
      xgb_workflow<-workflow()%>%
        add_recipe(recipe_formula)%>%
        add_model(xgb_spec)
      
      # Addestrare e valutare il modello sui resampling
      xgb_results<-fit_resamples(
        xgb_workflow
        ,resamples=roll_rs
        ,metrics=metric_set(rmse,rsq)
        ,control=control_resamples(save_pred=TRUE)
      )
      
      # Risultati del backtesting
      metrics_xgb<-xgb_results%>%
        collect_metrics()
      
      # Addestrare il modello
      xgb_fit<-xgb_workflow%>%
        fit(data=forex_data)
      
      # Previsioni
      forecast_xgb<-xgb_fit%>%
        predict(new_data=tail(forex_data,5))
      
      gc()
    }
  }
  
  # Confrontare le performance dei modelli
  models_results<-bind_rows(
    linear_results%>% mutate(model="lm")
    ,elastic_results%>% mutate(model="elastic")
    ,mars_results%>% mutate(model="mars")
    #,rf_results%>% mutate(model="rf")
    #,xgb_results%>% mutate(model="xgb")
  )
  
  models_res_grp<-models_results%>%
    group_by(model,.metrics)%>%
    summarize(mean=mean(.estimate),sd=sd(.estimate))
  
  # Combinare i Modelli in un Ensemble : Utilizzare lo stacking ensemble di tidymodels
  # Creare uno stack dei modelli
  ensemble_stack<-stacks()%>%
    add_candidates(linear_results)%>%
    add_candidates(elastic_results)%>%
    add_candidates(mars_results)
  
  # Eseguire il blending delle previsioni
  ensemble_stack<-blend_predictions(ensemble_stack)
  
  # Addestrare il modello ensemble
  ensemble_fit<-fit_members(ensemble_stack)
  
  # Previsioni con il modello ensemble
  forecast_ensemble<-predict(ensemble_fit,new_data=tail(forex_data,5))
  
  # Confrontare le performance dei modelli individuali e dell'ensemble
  collect_metrics(linear_results)%>%
    bind_rows(collect_metrics(elastic_results))%>%
    bind_rows(collect_metrics(mars_results))%>%
    bind_rows(collect_metrics(ensemble_fit%>%mutate(model="Ensemble")))
  
}



fnc_uniV_forecast<-function(FX_Selected="CADCHF"
                            ,FX_Periodicity="daily" # "daily" "weekly"
                            ,time_Delta=time_delta
                            ,pred_variable="open"
                            ,path_Output_Metrics=path_output_metrics){
  
  # FX_Selected="GBPUSD"; FX_Periodicity="daily"; time_Delta=time_delta; pred_variable="open"; path_Output_Metrics=path_output_metrics
  
  FX_symbol<-FX_Selected%>%
    as.character()%>%
    str_split_i("[.]",i=1)
  
  time_Filter<-(Sys.Date()%m-%time_Delta)
  
  FX_Metrics_List<-list.files(file.path(path_Output_Metrics,FX_Periodicity),pattern="^2.*Rds")
  FX_Metrics_File<-FX_Metrics_List[str_detect(FX_Metrics_List,FX_symbol)%>%which()]
  FX_Metrics_Data<-read_rds(file.path(path_Output_Metrics,FX_Periodicity,FX_Metrics_File))%>%
    filter(date>=time_Filter)
  forex_data<-FX_Metrics_Data%>%
    na.omit()%>%
    select(c("date",pred_variable))%>%
    as_tsibble(index=date)
  
  colnames(forex_data)<-c("date","pred_variable")
  
  # Divisione dei dati in set di addestramento e test 
  set.seed(123)
  data_split<-initial_time_split(forex_data,prop=0.8) 
  train_data<-training(data_split) 
  test_data<-testing(data_split)
  
  # Creare i resampling per il backtesting
  which(forex_data$date%>%year()=="2024")%>%length()
  roll_rs<-rolling_origin(forex_data,initial=262,assess=5,skip=0)
  nrow(roll_rs)
  roll_rs$splits[[1]]
  
  recipe_formula<-recipe(pred_variable~date,data=forex_data)
  
  # Generic model ----
  {
    # Specificare il modello
    GenericModel_spec<-arima_reg()%>%
      set_engine("auto_arima")
    
    # Creare il workflow
    GenericModel_workflow<-workflow()%>%
      add_recipe(recipe_formula)%>%
      add_model(GenericModel_spec)
    
    # Addestrare e valutare il modello sui resampling 
    plan(multisession)
    GenericModel_results<-fit_resamples(GenericModel_workflow
                                 ,resamples=roll_rs
                                 ,metrics=metric_set(rmse,rsq)
                                 ,control=control_resamples(save_pred=TRUE
                                                            ,parallel_over="everything")
    )
    plan(sequential)
    
    # Risultati del backtesting
    metrics_GenericModel<-GenericModel_results%>%
      collect_metrics()
    
    # Addestrare il modello
    GenericModel_fit<-GenericModel_workflow%>%
      fit(data=train_data)
    
    # Calibration with test data
    GenericModel_calib <- GenericModel_fit|>
      modeltime_calibrate(new_data=test_data)
    
    GenericModel_calib|>
      dplyr::slice(1)|>
      unnest(.calibration_data)
    
    # Evaluation
    # Out-of-Sample
    GenericModel_calib|>
      modeltime_accuracy()|>
      table_modeltime_accuracy(.interactive=TRUE
                               ,bordered=TRUE
                               ,resizable=TRUE)
    
    # In-Sample
    GenericModel_calib|>
      modeltime_accuracy(new_data=train_data|> drop_na())|>
      table_modeltime_accuracy(.interactive=TRUE
                               ,bordered=TRUE
                               ,resizable=TRUE)
    
    GenericModel_calib|>
      modeltime_forecast(
        new_data=test_data,
        actual_data=forex_data%>%last((test_data%>%nrow()*2)),
        conf_interval=.8
      )|>
      plot_modeltime_forecast(
        .conf_interval_show=TRUE,
        .conf_interval_alpha=.5,
        .conf_interval_fill="lightblue",
        .title="Subscriber Forecast"
      )
    
    # Refitting ----
    GenericModel_refit <- GenericModel_calib|>
      modeltime_refit(data=forex_data)
    
    forecast_GenericModel<-GenericModel_refit|>
      modeltime_forecast(h="5 days"
                         ,actual_data=forex_data
                         ,conf_interval=.8) 
    
    # Visualizzare le previsioni
    forecast_GenericModel%>%
      slice_tail(n=10)
    
    forecast_GenericModel|>
      plot_modeltime_forecast(.conf_interval_fill="lightblue")
  }
  
  
  # arima ----
  {
    # Specificare il modello
    arima_spec<-arima_reg()%>%
      set_engine("auto_arima")
    
    # Creare il workflow
    arima_workflow<-workflow()%>%
      add_recipe(recipe_formula)%>%
      add_model(arima_spec)
    
    # Addestrare e valutare il modello sui resampling 
    plan(multisession)
    arima_results<-fit_resamples(arima_workflow
                                 ,resamples=roll_rs
                                 ,metrics=metric_set(rmse,rsq)
                                 ,control=control_resamples(save_pred=TRUE
                                                            ,parallel_over="everything")
    )
    plan(sequential)
    
    # Risultati del backtesting
    metrics_arima<-arima_results%>%
      collect_metrics()
    
    # Addestrare il modello
    arima_fit<-arima_workflow%>%
      fit(data=train_data)
    
    # Calibration with test data
    arima_calib <- arima_fit|>
      modeltime_calibrate(new_data=test_data)
    
    arima_calib|>
      dplyr::slice(1)|>
      unnest(.calibration_data)
    
    # Evaluation
    # Out-of-Sample
    arima_calib|>
      modeltime_accuracy()|>
      table_modeltime_accuracy(.interactive=TRUE,bordered=TRUE,resizable=TRUE)
    
    # In-Sample
    arima_calib|>
      modeltime_accuracy(new_data=train_data|> drop_na())|>
      table_modeltime_accuracy(.interactive=TRUE,bordered=TRUE,resizable=TRUE)
    
    arima_calib|>
      modeltime_forecast(
        new_data=test_data,
        actual_data=forex_data%>%last((test_data%>%nrow()*2)),
        conf_interval=.8
      )|>
      plot_modeltime_forecast(
        .conf_interval_show=TRUE,
        .conf_interval_alpha=.5,
        .conf_interval_fill="lightblue",
        .title="Subscriber Forecast"
      )
    
    
    # Refitting ----
    
    arima_refit <- arima_calib|>
      modeltime_refit(data=forex_data)
    
    arima_refit|>
      modeltime_forecast(
        h="5 days",
        actual_data=forex_data,
        conf_interval=.8
      )|>
      plot_modeltime_forecast(.conf_interval_fill="lightblue")
    
    # Previsioni
    forecast_arima<-modeltime_table(arima_fit)%>%
      modeltime_forecast(h="5 days",actual_data=forex_data)
    
    # Visualizzare le previsioni
    forecast_arima%>%
      slice_tail(n=10)
  }
  
  # Prophet ----
  {
    # Specificare il modello
    prophet_spec<-prophet_reg()%>%
      set_engine("prophet")
    
    # Creare il workflow
    prophet_workflow<-workflow()%>%
      add_recipe(recipe_formula)%>%
      add_model(prophet_spec)
    
    # Addestrare e valutare il modello sui resampling 
    plan(multisession)
    prophet_results<-fit_resamples(prophet_workflow
                                   ,resamples=roll_rs
                                   ,metrics=metric_set(rmse,rsq)
                                   ,control=control_resamples(save_pred=TRUE
                                                              ,parallel_over="everything")
    )
    plan(sequential)
    
    # Risultati del backtesting
    metrics_prophet<-prophet_results%>%
      collect_metrics()
    
    # Addestrare il modello
    prophet_fit<-prophet_workflow%>%
      fit(data=forex_data)
    
    # Previsioni
    forecast_prophet<-modeltime_table(prophet_fit)%>%
      modeltime_forecast(h="5 days",actual_data=forex_data)
    
    # Visualizzare le previsioni
    forecast_prophet%>%
      slice_tail(n=10)
  }
  
  # arima_boost ----
  {
    # Specificare il modello
    arima_boost_spec<-arima_boost()%>%
      set_engine("auto_arima_xgboost")
    
    # Creare il workflow
    arima_boost_workflow<-workflow()%>%
      add_recipe(recipe_formula)%>%
      add_model(arima_boost_spec)
    
    # Addestrare e valutare il modello sui resampling 
    plan(multisession)
    arima_boost_results<-fit_resamples(arima_boost_workflow
                                       ,resamples=roll_rs
                                       ,metrics=metric_set(rmse,rsq)
                                       ,control=control_resamples(save_pred=TRUE
                                                                  ,parallel_over="everything")
    )
    plan(sequential)
    
    # Risultati del backtesting
    metrics_arima_boost<-arima_boost_results%>%
      collect_metrics()
    
    # Addestrare il modello
    arima_boost_fit<-arima_boost_workflow%>%
      fit(data=forex_data)
    
    # Previsioni
    forecast_arima_boost<-modeltime_table(arima_boost_fit)%>%
      modeltime_forecast(h="5 days",actual_data=forex_data)
    
    # Visualizzare le previsioni
    forecast_arima_boost%>%
      slice_tail(n=10)
  }
  
  # prophet_boost ----
  {
    # Specificare il modello
    prophet_boost_spec<-prophet_boost()%>%
      set_engine("prophet_xgboost")
    
    # Creare il workflow
    prophet_boost_workflow<-workflow()%>%
      add_recipe(recipe_formula)%>%
      add_model(prophet_boost_spec)
    
    # Addestrare e valutare il modello sui resampling 
    plan(multisession)
    prophet_boost_results<-fit_resamples(prophet_boost_workflow
                                         ,resamples=roll_rs
                                         ,metrics=metric_set(rmse,rsq)
                                         ,control=control_resamples(save_pred=TRUE
                                                                    ,parallel_over="everything")
    )
    plan(sequential)
    
    # Risultati del backtesting
    metrics_prophet_boost<-prophet_boost_results%>%
      collect_metrics()
    
    # Addestrare il modello
    prophet_boost_fit<-prophet_boost_workflow%>%
      fit(data=forex_data)
    
    # Previsioni
    forecast_prophet_boost<-modeltime_table(prophet_boost_fit)%>%
      modeltime_forecast(h="5 days",actual_data=forex_data)
    
    # Visualizzare le previsioni
    forecast_prophet_boost%>%
      slice_tail(n=10)
  }
  
  # Linear Regression ----
  {
    # Specificare il modello
    linear_spec<-linear_reg()%>%
      set_engine("lm")%>%
      set_mode("regression")
    
    # Creare il workflow
    linear_workflow<-workflow()%>%
      add_recipe(recipe_formula)%>%
      add_model(linear_spec)
    
    # Addestrare e valutare il modello sui resampling 
    plan(multisession)
    linear_results<-fit_resamples(linear_workflow
                                  ,resamples=roll_rs
                                  ,metrics=metric_set(rmse,rsq)
                                  ,control=control_resamples(save_pred=TRUE
                                                             ,parallel_over="everything")
    )
    plan(sequential)
    
    # Risultati del backtesting
    metrics_linear<-linear_results%>%
      collect_metrics()
    
    # Addestrare il modello
    linear_fit<-linear_workflow%>%
      fit(data=forex_data)
    
    # Previsioni
    forecast_linear<-modeltime_table(linear_fit)%>%
      modeltime_forecast(h="5 days",actual_data=forex_data)
    
    # Visualizzare le previsioni
    forecast_linear%>%
      slice_tail(n=10)
  }
  
  # Random Forest ----
  {
    # Specificare il modello
    rf_spec<-rand_forest()%>%
      set_engine("ranger")%>%
      set_mode("regression")
    
    # Creare il workflow
    rf_workflow<-workflow()%>%
      add_recipe(recipe_formula)%>%
      add_model(rf_spec)
    
    # Addestrare e valutare il modello sui resampling 
    plan(multisession)
    rf_results<-fit_resamples(rf_workflow
                              ,resamples=roll_rs
                              ,metrics=metric_set(rmse,rsq)
                              ,control=control_resamples(save_pred=TRUE
                                                         ,parallel_over="everything")
    )
    plan(sequential)
    
    # Risultati del backtesting
    metrics_rf<-rf_results%>%
      collect_metrics()
    
    # Addestrare il modello
    rf_fit<-rf_workflow%>%
      fit(data=forex_data)
    
    # Previsioni
    forecast_rf<-modeltime_table(rf_fit)%>%
      modeltime_forecast(h="5 days",actual_data=forex_data)
    
    # Visualizzare le previsioni
    forecast_rf%>%
      slice_tail(n=10)
  }
  
  # XGBoost ----
  {
    forex_data_xgb<-forex_data
    forex_data_xgb$date<-(1:nrow(forex_data))
    # Specificare il modello
    xgb_spec<-boost_tree()%>%
      set_engine("xgboost")%>%
      set_mode("regression")
    
    # Creare il workflow
    xgb_workflow<-workflow()%>%
      add_recipe(recipe(pred_variable~date,data=forex_data_xgb))%>%
      add_model(xgb_spec)
    
    # Addestrare e valutare il modello sui resampling 
    plan(multisession)
    xgb_results<-fit_resamples(xgb_workflow
                               ,resamples=roll_rs
                               ,metrics=metric_set(rmse,rsq)
                               ,control=control_resamples(save_pred=TRUE
                                                          ,parallel_over="everything")
    )
    plan(sequential)
    
    # Risultati del backtesting
    metrics_xgb<-xgb_results%>%
      collect_metrics()
    
    # Addestrare il modello
    xgb_fit<-xgb_workflow%>%
      fit(data=forex_data_xgb)
    
    # Previsioni
    forecast_xgb<-modeltime_table(xgb_fit)%>%
      modeltime_forecast(h="5 days",actual_data=forex_data_xgb)
    
    # Visualizzare le previsioni
    forecast_xgb%>%
      slice_tail(n=10)
  }
  
  # Ensemble ----
  {
    # Creare un ensemble dei modelli ARIMA,Prophet e Random Forest
    ensemble_table<-modeltime_table(
      arima_fit
      ,prophet_fit
      ,arima_boost_fit
      ,prophet_boost_fit
      ,linear_fit
      ,rf_fit
    )
    
    # Eseguire il blending delle previsioni
    ensemble_fit<-ensemble_table %>%
      ensemble_average(type="mean")
    
    # Previsioni con l'ensemble
    forecast_ensemble<-ensemble_fit %>%
      modeltime_forecast(h="5 days",actual_data=forex_data)
    
    # Visualizzare le previsioni
    forecast_ensemble
  }
  
  
  
  
  
}
