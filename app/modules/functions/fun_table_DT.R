#https://rstudio.github.io/DT/

css_styling_FTP_complete<-JS(
  "function(settings,json) {",
  "$(this.api().table().header()).css({'background-color': '#2668B2','color': '#fff'});",
  "$(this.api().table().rows().nodes()).each(function(index) {
    if (index % 2 === 0) {
      $(this).css({'background-color': '#000000','color': '#fff'});
    } else {
      $(this).css({'background-color': '#FF0000','color': '#fff'});
    }
  });",
  "$(this).find('td:nth-child(1)').css({'background-color': '#2668B2','color': '#fff'});",
  "$(this).find('td:nth-child(2)').css('background-color','#007FFF');",
  "$(this).find('td:nth-child(3)').css('background-color','#0055FF');",
  "$(this).find('td:nth-child(4)').css('background-color','#002BFF');",
  "}")

fun_DT_FX_summary<-function(FX_Periodicity="daily" # "daily" "weekly"
                            ,path_Output_Tables=path_output_tables
                            ,path_Output_Metrics=path_output_metrics
                            ,css_Styling_FTP_Complete=css_styling_FTP_complete
                            ,save_Widget=FALSE){
  
  # FX_Periodicity="daily";path_Output_Tables=path_output_tables;path_Output_Metrics=path_output_metrics;css_Styling_FTP_Complete=css_styling_FTP_complete
  
  path_Output_DT<-file.path(path_Output_Tables,"DT",FX_Periodicity)
  
  FX_Metrics_List<-list.files(file.path(path_Output_Metrics,FX_Periodicity),pattern="^2.*feather")
  
  FX_data_summary<-tibble(symbol=as.character(NA)
                          ,from=as.character(NA)
                          ,to=as.character(NA)
                          ,close=as.numeric(NA)
                          ,close_D=as.numeric(NA)
                          ,close_Perc=as.character(NA)
                          ,RSI=as.numeric(NA)
                          ,stoch=as.numeric(NA)
                          ,adx=as.numeric(NA)
                          ,volatility=as.numeric(NA)
  )
  
  for(FX_Metrics_File in FX_Metrics_List){
    # FX_selected="GBPUSD";
    # FX_Metrics_File<-FX_Metrics_List[str_detect(FX_Metrics_List,FX_selected)%>%which()]
    
    FX_Metrics_Data<-read_feather(file.path(path_Output_Metrics,FX_Periodicity,FX_Metrics_File))%>%
      #arrange(date%>%desc())%>%
      slice_tail(n=10)
    
    dates_from_to<-FX_Metrics_Data$date%>%last(2)
    
    (fx_close<-FX_Metrics_Data$close%>%last(1))
    last_two_observations<-FX_Metrics_Data$close%>%last(2)
    (fx_close_delta<-(last_two_observations[2]-last_two_observations[1]))%>%round(digits=4)
    delta_perc<-(((last_two_observations[2]/last_two_observations[1])-1)*100)%>%round(digits=2)
    (fx_close_delta_perc<-paste0(delta_perc,"%"))
    index_lm<-(1:3)
    fx_lm<-stats::lm(FX_Metrics_Data$close%>%last(3)~index_lm)
    (fx_close_slope<-coef(fx_lm)[2])
    
    (fx_rsi<-FX_Metrics_Data$RSI%>%last(1))
    fx_lm<-stats::lm(FX_Metrics_Data$RSI%>%last(3)~index_lm)
    (fx_rsi_slope<-coef(fx_lm)[2])
    
    (fx_stoch<-FX_Metrics_Data$stoch%>%last(1))
    fx_lm<-stats::lm(FX_Metrics_Data$stoch%>%last(3)~index_lm)
    (fx_stoch_slope<-coef(fx_lm)[2])
    
    (fx_adx<-FX_Metrics_Data$ADX%>%last(1))
    fx_lm<-stats::lm(FX_Metrics_Data$ADX%>%last(3)~index_lm)
    (fx_adx_slope<-coef(fx_lm)[2])
    
    (fx_volatility<-(FX_Metrics_Data$volatility%>%last(1))*100)
    fx_lm<-stats::lm(FX_Metrics_Data$volatility%>%last(3)~index_lm)
    (fx_volatility_slope<-(coef(fx_lm)[2])*100)
    
    FX_data_summary%<>%add_row(tibble_row(symbol=FX_Metrics_Data$symbol%>%unique()
                                          ,from=dates_from_to[1]%>%format( "%d/%m")
                                          ,to=dates_from_to[2]%>%format( "%d/%m")
                                          ,close=fx_close
                                          ,close_D=fx_close_delta
                                          ,close_Perc=fx_close_delta_perc
                                          ,RSI=fx_rsi
                                          ,stoch=fx_stoch
                                          ,adx=fx_adx
                                          ,volatility=fx_volatility
    ))%>%
      na.omit()%>%
      arrange(symbol)%>%
      distinct()
  }
  
  table_DT_FX_summary<-FX_data_summary%>%
    rename("FX"="symbol"
           ,"cls"="close","cls %"="close_Perc","cls D"="close_D"
           ,"volat"="volatility"
    )%>%
    formattable::formattable()%>%
    formattable::as.datatable(class='compact cell-border stripe'
                              ,extensions=c('Responsive','Buttons')
                              ,rownames=FALSE
                              ,options=list(autoWidth=T,paging=F,scrollX=F,searching=T,ordering=T
                                            ,columnDefs=list(list(className='dt-center'
                                                                  ,targets=0:(ncol(FX_data_summary)-1)))
                                            ,buttons=list('copy'
                                                          ,list(extend='collection'
                                                                ,buttons=c('csv','excel','pdf')
                                                                ,text='Download'))
                                            ,dom='tr'
                                            ,searchHighlight=FALSE
                                            ,initComplete=css_Styling_FTP_Complete
                              )
                              ,caption=htmltools::tags$caption(style='caption-side: top
                                                               ;text-align: center
                                                               ;color:black
                                                               ;font-size:100%;'
                                                               ,paste0('Summary table'))
    )%>%
    formatRound(columns=c("cls","cls D"),digits=4)%>%
    formatRound(columns=c("RSI"
                          ,"stoch"
                          ,"adx"
                          ,"volat"),digits=1)
  
  if(save_Widget){
    htmlwidgets::saveWidget(table_DT_FX_summary
                            ,file=file.path(path_Output_DT,FX_Periodicity
                                            ,paste0("table_",FX_Periodicity,"_FX_summary.html")))
  }
  
  return(table_DT_FX_summary)
}
#-------------------------------------------------------------------------------
fun_DT_trading_signals<-function(FX_Selected="CADCHF"
                                 ,FX_Periodicity="daily" # "daily"
                                 ,path_Output_Tables=path_output_tables
                                 ,path_Output_Signals=path_output_signals
                                 ,css_Styling_FTP_Complete=css_styling_FTP_complete
                                 ,save_Widget=FALSE){
  
  # FX_Periodicity="daily";FX_Selected="CADCHF";path_Output_Tables=path_output_tables;path_Output_Signals=path_output_signals;css_Styling_FTP_Complete=css_styling_FTP_complete
  
  path_Output_DT<-file.path(path_Output_Tables,"DT",FX_Periodicity)
  
  FX_Signals_List<-list.files(file.path(path_Output_Signals,FX_Periodicity),pattern="^.*feather")
  
  FX_Signals_File<-FX_Signals_List[str_detect(FX_Signals_List,FX_Selected)%>%which()]
  
  FX_Signals_Data<-read_feather(file.path(path_Output_Signals,FX_Periodicity,FX_Signals_File))%>%
    arrange(date%>%desc())%>%
    slice_head(n=100)
  
  colnames(FX_Signals_Data)<-colnames(FX_Signals_Data)%>%str_remove_all("Sig_")

  table_DT_FX_signals<-FX_Signals_Data%>%
    rename("FX"="symbol"
    )%>%
    formattable::formattable()%>%
    formattable::as.datatable(filter='none'
                              ,rownames=FALSE
                              ,options=list(autoWidth=TRUE
                                ,searching=FALSE)
                              ,caption=htmltools::tags$caption(style='caption-side: top
                                                               ;text-align: center
                                                               ;color:black
                                                               ;font-size:100%;'
                                                               ,paste0(FX_Selected,' signals table'))
    )%>%
    formatRound(columns=c("atr"),digits=3)
  
  if(save_Widget){
    htmlwidgets::saveWidget(table_DT_FX_signals
                            ,file=file.path(path_Output_DT,FX_Periodicity
                                            ,paste0("table_", FX_Symbol, "_",FX_Periodicity,"_FX_summary.html")))
  }
  
  return(table_DT_FX_signals)
}
