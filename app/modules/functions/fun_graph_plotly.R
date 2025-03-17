#' https://jtr13.github.io/cc19/technical-analysis-for-stocks-using-plotly.html

# layout parameters
{
  buttons_list<-list(
    list(count=7,label="1w",step="day",stepmode="backward")
    ,list(count=1,label="1m",step="month",stepmode="backward")
    ,list(count=3,label="3m",step="month",stepmode="backward")
    ,list(count=1,label="YTD",step="year",stepmode="todate")
    ,list(step="all")
  )
  
  yaxis_layout_list<-list(autorange=TRUE,fixedrange=FALSE)
  
}

# parameters 
{
  time_delta<-months(6)
}

# candlestick ----
fnc_plotly_FX_candlestick<-function(FX_Selected="CADCHF"
                                    ,FX_Periodicity="daily" # "daily" "weekly"
                                    ,time_Delta=time_delta
                                    ,path_Output_Graphs=path_output_graphs
                                    ,path_Output_Metrics=path_output_metrics
                                    ,buttons_List=buttons_list
                                    ,yaxis_Layout_List=yaxis_layout_list
                                    ,save_Widget=FALSE){
  
  # FX_Selected="CADCHF"; FX_Periodicity="daily"; time_Delta=time_delta; save_Widget=FALSE; path_Output_Graphs=path_output_graphs;path_Output_Metrics=path_output_metrics; buttons_List=buttons_list; yaxis_Layout_List=yaxis_layout_list
  
  FX_symbol<-FX_Selected%>%
    as.character()%>%
    str_split_i("[.]",i=1)
  
  path_Output_Plotly<-file.path(path_Output_Graphs,"plotly",FX_Periodicity)
  
  time_Filter<-(Sys.Date()%m-%time_Delta)
  
  FX_Metrics_List<-list.files(file.path(path_Output_Metrics,FX_Periodicity),pattern="^2.*feather")
  FX_Metrics_File<-FX_Metrics_List[str_detect(FX_Metrics_List,FX_symbol)%>%which()]
  FX_Metrics_Data<-read_feather(file.path(path_Output_Metrics,FX_Periodicity,FX_Metrics_File))%>%
    filter(date>=time_Filter)
  
  # layout parameters
  {
    xaxis_Candlestick_Layout_List<-list(title=paste0("From : ",min(FX_Metrics_Data$date)
                                                     ," To : ",max(FX_Metrics_Data$date))
                                        ,rangebreaks=list(list(bounds=list("sat","sun")))
                                        ,rangeslider=list(visible=TRUE)
                                        ,rangeselector=list(buttons=buttons_List))
  }
  
  # CANDLESTICK
  plot_candlestick<-plot_ly(FX_Metrics_Data)%>%
    add_trace(x=~date,open=~open,high=~high,low=~low,close=~close
              ,name='OHLC',type='candlestick'
              ,increasing=list(line=list(color='green',width=2)
                               ,fillcolor='rgba(0,0,0,0)') # Transparent
              ,decreasing=list(line=list(color='red',width=2)
                               ,fillcolor='rgba(0,0,0,0)') # Transparent
              ,legendgroup='one')%>%
    add_trace(x=~date,y=~close,name='close',type='scatter',mode='lines'
              ,line=list(color='white',width=1,dash='line')
              ,marker=NULL,legendgroup='two')%>%
    # BBands SMA
    add_trace(x=~date,y=~BB_up,name='BB_up',type='scatter',mode='lines'
              ,line=list(color='green',width=1.5,dash='line')
              ,legendgroup='three')%>%
    add_trace(x=~date,y=~BB_mavg,name='BB_mavg',type='scatter',mode='lines'
              ,line=list(color='navy',width=1.5,dash='line')
              ,legendgroup='three')%>%
    add_trace(x=~date,y=~BB_dn,name='BB_dn',type='scatter',mode='lines'
              ,line=list(color='red',width=1.5,dash='line')
              ,legendgroup='three')%>%
    layout(showlegend=T,title=FX_symbol
           ,xaxis=xaxis_Candlestick_Layout_List
           ,yaxis=list.append(yaxis_Layout_List,title="candlestick")
           ,paper_bgcolor='black',plot_bgcolor='black')
  
  if(save_Widget){
    htmlwidgets::saveWidget(plot_candlestick
                            ,file=file.path(path_Output_Plotly,FX_Periodicity
                                            ,paste0("plot_",FX_symbol,"_",FX_Periodicity,"_candlestick.html")))
  }
  
  return(plot_candlestick)
  gc()
}


# Volatility ----
fnc_plotly_FX_volatility<-function(FX_Selected="CADCHF"
                                   ,FX_Periodicity="daily" # "daily" "weekly"
                                   ,time_Delta=time_delta
                                   ,path_Output_Graphs=path_output_graphs
                                   ,path_Output_Metrics=path_output_metrics
                                   ,buttons_List=buttons_list
                                   ,yaxis_Layout_List=yaxis_layout_list
                                   ,save_Widget=FALSE){
  
  # FX_Selected="EURCHF"; FX_Periodicity="daily"; time_Delta=time_delta; save_Widget=FALSE; path_Output_Graphs=path_output_graphs;path_Output_Metrics=path_output_metrics; buttons_List=buttons_list; yaxis_Layout_List=yaxis_layout_list
  
  FX_symbol<-FX_Selected%>%
    as.character()%>%
    str_split_i("[.]",i=1)
  
  path_Output_Plotly<-file.path(path_Output_Graphs,"plotly",FX_Periodicity)
  
  time_Filter<-(Sys.Date()%m-%time_Delta)
  
  FX_Metrics_List<-list.files(file.path(path_Output_Metrics,FX_Periodicity),pattern="^2.*feather")
  FX_Metrics_File<-FX_Metrics_List[str_detect(FX_Metrics_List,FX_symbol)%>%which()]
  FX_Metrics_Data<-read_feather(file.path(path_Output_Metrics,FX_Periodicity,FX_Metrics_File))%>%
    filter(date>=time_Filter)
  
  # layout parameters
  {
    xaxis_Lines_Layout_List<-list(title=paste0("From : ",min(FX_Metrics_Data$date)
                                               ," To : ",max(FX_Metrics_Data$date))
                                  #,rangebreaks=list(list(bounds=list("sat","mon")))
                                  ,rangeslider=list(visible=TRUE)
                                  ,rangeselector=list(buttons=buttons_List))
    }
  
  # Volatility
  plot_Volatility<-plot_ly(FX_Metrics_Data)%>%
    add_trace(x=c(~min(date),~max(date)),y=c(0.1,0.1),name='low',type='scatter',mode='lines'
              ,line=list(color='green',width=1,dash='dot')
              ,marker=NULL,legendgroup='one')%>%
    add_trace(x=c(~min(date),~max(date)),y=c(0.2,0.2),name='medium',type='scatter',mode='lines'
              ,line=list(color='orange',width=1,dash='dot')
              ,marker=NULL,legendgroup='one')%>%
    add_trace(x=c(~min(date),~max(date)),y=c(0.4,0.4),name='high',type='scatter',mode='lines'
              ,line=list(color='red',width=1,dash='dot')
              ,marker=NULL,legendgroup='one')%>%
    add_trace(x=~date,y=~volatility,name='volatility',type='scatter',mode='lines'
              ,line=list(color='navy'),marker=NULL,legendgroup='two')%>%
    layout(showlegend=T,title=FX_symbol
           ,xaxis=xaxis_Lines_Layout_List
           ,yaxis=list.append(yaxis_Layout_List,title="ADX")
           ,paper_bgcolor='black',plot_bgcolor='white')
  
  if(save_Widget){
    htmlwidgets::saveWidget(plot_Volatility
                            ,file=file.path(path_Output_Plotly,FX_Periodicity
                                            ,paste0("plot_",FX_symbol,"_",FX_Periodicity,"_Volatility.html")))
  }
  
  return(plot_Volatility)
  gc()
}

# Trend-Following ----
fnc_plotly_FX_trendFollowing<-function(FX_Selected="CADCHF"
                                 ,FX_Periodicity="daily" # "daily" "weekly"
                                 ,time_Delta=time_delta
                                 ,path_Output_Graphs=path_output_graphs
                                 ,path_Output_Metrics=path_output_metrics
                                 ,buttons_List=buttons_list
                                 ,yaxis_Layout_List=yaxis_layout_list
                                 ,save_Widget=FALSE){
  
  # FX_Selected="CADCHF"; FX_Periodicity="daily"; time_Delta=time_delta; save_Widget=FALSE; path_Output_Graphs=path_output_graphs;path_Output_Metrics=path_output_metrics; buttons_List=buttons_list; yaxis_Layout_List=yaxis_layout_list
  
  FX_symbol<-FX_Selected%>%
    as.character()%>%
    str_split_i("[.]",i=1)
  
  path_Output_Plotly<-file.path(path_Output_Graphs,"plotly",FX_Periodicity)
  
  time_Filter<-(Sys.Date()%m-%time_Delta)
  
  FX_Metrics_List<-list.files(file.path(path_Output_Metrics,FX_Periodicity),pattern="^2.*feather")
  FX_Metrics_File<-FX_Metrics_List[str_detect(FX_Metrics_List,FX_symbol)%>%which()]
  FX_Metrics_Data<-read_feather(file.path(path_Output_Metrics,FX_Periodicity,FX_Metrics_File))%>%
    filter(date>=time_Filter)
  
  # layout parameters
  {
    xaxis_Lines_Layout_List<-list(title=paste0("From : ",min(FX_Metrics_Data$date)
                                               ," To : ",max(FX_Metrics_Data$date))
                                  #,rangebreaks=list(list(bounds=list("sat","mon")))
                                  ,rangeslider=list(visible=TRUE)
                                  ,rangeselector=list(buttons=buttons_List))
  }
  
  # SMA_fast > SMA_slow ~ "BUY"
  # SMA_fast < SMA_slow ~ "SELL"
  plot_trendFollowing<-plot_ly(FX_Metrics_Data)%>%
    add_trace(x=~date,y=~close,name='close',type='scatter',mode='lines'
              ,line=list(color='black',width=1,dash='line')
              ,marker=NULL,legendgroup='one')%>%
    add_trace(x=~date,y=~SMA_veloce,name='SMA fast',type='scatter',mode='lines'
              ,line=list(color='blue',width=2,dash='line')
              ,marker=NULL,legendgroup='two')%>%
    add_trace(x=~date,y=~SMA_lento,name='SMA slow',type='scatter',mode='lines'
              ,line=list(color='orange',width=2,dash='line')
              ,marker=NULL,legendgroup='two')%>%
    add_trace(x=~date,y=~EMA_veloce,name='EMA fast',type='scatter',mode='lines'
              ,line=list(color='green',width=2,dash='line')
              ,marker=NULL,legendgroup='three')%>%
    add_trace(x=~date,y=~EMA_lento,name='EMA slow',type='scatter',mode='lines'
              ,line=list(color='red',width=2,dash='line')
              ,marker=NULL,legendgroup='three')%>%
    layout(showlegend=T
           ,title=list(
             text=paste0(FX_symbol, "<br><sup>MA fast > MA slow ~ BUY / MA fast < MA slow ~ SELL</sup>")
             , x=0.5  # Centrare il titolo
           )
           ,xaxis=xaxis_Lines_Layout_List
           ,yaxis=list.append(yaxis_Layout_List,title="Moving Average")
           ,paper_bgcolor='white',plot_bgcolor='white')
  
  if(save_Widget){
    htmlwidgets::saveWidget(plot_trendFollowing
                            ,file=file.path(path_Output_Plotly,FX_Periodicity
                                            ,paste0("plot_",FX_symbol,"_",FX_Periodicity,"_trendFollowing.html")))
  }
  
  return(plot_trendFollowing)
  gc()
  
}

# Momentum ----
fnc_plotly_FX_momentum<-function(FX_Selected="CADCHF"
                            ,FX_Periodicity="daily" # "daily" "weekly"
                            ,time_Delta=time_delta
                            ,path_Output_Graphs=path_output_graphs
                            ,path_Output_Metrics=path_output_metrics
                            ,buttons_List=buttons_list
                            ,yaxis_Layout_List=yaxis_layout_list
                            ,save_Widget=FALSE){
  
  # FX_Selected="CADCHF"; FX_Periodicity="daily"; save_Widget=FALSE; path_Output_Graphs=path_output_graphs;path_Output_Metrics=path_output_metrics; buttons_List=buttons_list; yaxis_Layout_List=yaxis_layout_list
  
  FX_symbol<-FX_Selected%>%
    as.character()%>%
    str_split_i("[.]",i=1)
  
  path_Output_Plotly<-file.path(path_Output_Graphs,"plotly",FX_Periodicity)
  
  time_Filter<-(Sys.Date()%m-%time_Delta)
  
  FX_Metrics_List<-list.files(file.path(path_Output_Metrics,FX_Periodicity),pattern="^2.*feather")
  FX_Metrics_File<-FX_Metrics_List[str_detect(FX_Metrics_List,FX_symbol)%>%which()]
  FX_Metrics_Data<-read_feather(file.path(path_Output_Metrics,FX_Periodicity,FX_Metrics_File))%>%
    filter(date>=time_Filter)
  
  # layout parameters
  {
    xaxis_Lines_Layout_List<-list(title=paste0("From : ",min(FX_Metrics_Data$date)
                                               ," To : ",max(FX_Metrics_Data$date))
                                  #,rangebreaks=list(list(bounds=list("sat","mon")))
                                  ,rangeslider=list(visible=TRUE)
                                  ,rangeselector=list(buttons=buttons_List))
  }
  
  plot_momentum<-plot_ly(FX_Metrics_Data)%>%
    # RSI
    add_trace(x=c(~min(date),~max(date)),y=c(70,70),name='overbought',type='scatter',mode='lines'
              ,line=list(color='green',width=1,dash='dot')
              ,marker=NULL,legendgroup='one')%>%
    add_trace(x=c(~min(date),~max(date)),y=c(50,50),name='shift',type='scatter',mode='lines'
              ,line=list(color='orange',width=1,dash='dot')
              ,marker=NULL,legendgroup='one')%>%
    add_trace(x=c(~min(date),~max(date)),y=c(30,30),name='oversold',type='scatter',mode='lines'
              ,line=list(color='red',width=1,dash='dot')
              ,marker=NULL,legendgroup='one')%>%
    add_trace(x=~date,y=~RSI,name='RSI',type='scatter',mode='lines'
              ,line=list(color='black'),marker=NULL,legendgroup='one')%>%
    # Stochastic
    add_trace(x=c(~min(date),~max(date)),y=c(80,80),name='overbought',type='scatter',mode='lines'
              ,line=list(color='green',width=1,dash='dot')
              ,marker=NULL,legendgroup='two')%>%
    add_trace(x=c(~min(date),~max(date)),y=c(50,50),name='shift',type='scatter',mode='lines'
              ,line=list(color='orange',width=1,dash='dot')
              ,marker=NULL,legendgroup='two')%>%
    add_trace(x=c(~min(date),~max(date)),y=c(20,20),name='oversold',type='scatter',mode='lines'
              ,line=list(color='red',width=1,dash='dot')
              ,marker=NULL,legendgroup='two')%>%
    add_trace(x=~date,y=~fastK*100,name='fastK',type='scatter',mode='lines'
              ,line=list(color='coral'),marker=NULL,legendgroup='two')%>%
    add_trace(x=~date,y=~fastD*100,name='fastD',type='scatter',mode='lines'
              ,line=list(color='purple'),marker=NULL,legendgroup='two')%>%
    add_trace(x=~date,y=~stoch*100,name='stoch',type='scatter',mode='lines'
              ,line=list(color='blue'),marker=NULL,legendgroup='two')%>%
    layout(showlegend=T,title=FX_symbol
           ,xaxis=xaxis_Lines_Layout_List
           ,yaxis=list.append(yaxis_Layout_List,title="RSI")
           ,paper_bgcolor='black',plot_bgcolor='white')
  
  if(save_Widget){
    htmlwidgets::saveWidget(plot_momentum
                            ,file=file.path(path_Output_Plotly,FX_Periodicity
                                            ,paste0("plot_",FX_symbol,"_",FX_Periodicity,"_momentum.html")))
  }
  
  return(plot_momentum)
  gc()
}

# MACD : Moving Average Convergence Divergence ----
fnc_plotly_FX_MACD<-function(FX_Selected="CADCHF"
                             ,FX_Periodicity="daily" # "daily" "weekly"
                             ,time_Delta=time_delta
                             ,path_Output_Graphs=path_output_graphs
                             ,path_Output_Metrics=path_output_metrics
                             ,buttons_List=buttons_list
                             ,yaxis_Layout_List=yaxis_layout_list
                             ,save_Widget=FALSE){
  
  # FX_Selected="EURCHF"; FX_Periodicity="daily"; save_Widget=FALSE; path_Output_Graphs=path_output_graphs;path_Output_Metrics=path_output_metrics; buttons_List=buttons_list; yaxis_Layout_List=yaxis_layout_list
  
  FX_symbol<-FX_Selected%>%
    as.character()%>%
    str_split_i("[.]",i=1)
  
  path_Output_Plotly<-file.path(path_Output_Graphs,"plotly",FX_Periodicity)
  
  time_Filter<-(Sys.Date()%m-%time_Delta)
  
  FX_Metrics_List<-list.files(file.path(path_Output_Metrics,FX_Periodicity),pattern="^2.*feather")
  FX_Metrics_File<-FX_Metrics_List[str_detect(FX_Metrics_List,FX_symbol)%>%which()]
  FX_Metrics_Data<-read_feather(file.path(path_Output_Metrics,FX_Periodicity,FX_Metrics_File))%>%
    filter(date>=time_Filter)
  
  # layout parameters
  {
    xaxis_Lines_Layout_List<-list(title=paste0("From : ",min(FX_Metrics_Data$date)
                                               ," To : ",max(FX_Metrics_Data$date))
                                  #,rangebreaks=list(list(bounds=list("sat","mon")))
                                  ,rangeslider=list(visible=TRUE)
                                  ,rangeselector=list(buttons=buttons_List))
  }
  
  # MACD
  plot_MACD<-plot_ly(FX_Metrics_Data)%>%
    add_trace(x=~date,y=~macd_histo,name='histo',type='bar'
              ,marker=list(color='red'),marker=NULL,legendgroup='one')%>%
    add_trace(x=~date,y=~macd,name='macd',type='scatter',mode='lines'
              ,line=list(color='black'),marker=NULL,legendgroup='one')%>%
    add_trace(x=~date,y=~macd_signal,name='signal',type='scatter',mode='lines'
              ,line=list(color='navy'),marker=NULL,legendgroup='one')%>%
    layout(showlegend=T,title=FX_symbol
           ,xaxis=xaxis_Lines_Layout_List
           ,yaxis=list.append(yaxis_Layout_List,title="MACD")
           ,paper_bgcolor='black',plot_bgcolor='white')
  
  if(save_Widget){
    htmlwidgets::saveWidget(plot_MACD
                            ,file=file.path(path_Output_Plotly,FX_Periodicity
                                            ,paste0("plot_",FX_symbol,"_",FX_Periodicity,"_MACD.html")))
  }
  
  return(plot_MACD)
  gc()
}
