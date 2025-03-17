
#' NOT IN function
`%notin%` <- Negate(`%in%`)

#' ichimoku
fun_ichimoku<-function(data){
  
  data<-data%>%
    #' Ichimoku
    mutate(nine_period_high=rollapplyr(high,9,max,fill=NA)
           ,nine_period_low=rollapplyr(low,9,min,fill=NA)
           # Tenkan-sen (Conversion Line): (9-period high+9-period low)/2))
           ,tenkan_sen=(nine_period_high+nine_period_low)/2
           ,period26_high=rollapplyr(high,26,max,fill=NA)
           ,period26_low=rollapplyr(low,26,min,fill=NA)
           # Kijun-sen (Base Line): (26-period high+26-period low)/2))
           ,kijun_sen=(period26_high+period26_low)/2
           # Senkou Span A (Leading Span A): (Conversion Line+Base Line)/2))
           ,senkou_span_a=lag((tenkan_sen+kijun_sen)/2,26)
           ,period52_high=rollapplyr(high,52,max,fill=NA)
           ,period52_low=rollapplyr(low,52,min,fill=NA)
           # Senkou Span B (Leading Span B): (52-period high+52-period low)/2))
           ,senkou_span_b=lag((period52_high+period52_low)/2,26)
           # The most current closing price plotted 26 time periods behind (optional)
           ,chikou_span=lag(close,26))
  
  return(data)
}
