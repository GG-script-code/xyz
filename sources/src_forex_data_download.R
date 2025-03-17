
# tq_get ----
fnc_tqGet_FX_Data<-function(FX_List=FX_list,path_Input=path_input_tqGet
                            ,last_Week_Delete=(Sys.Date()-7)
                            ,yesterday_Date=(Sys.Date()-1)){
  # FX_List=FX_list; path_Input=path_input_tqGet; last_Week_Delete=(Sys.Date()-7); yesterday_Date=(Sys.Date()-1)
  
  while(wday(yesterday_Date,week_start=1)%in%c(6,7)){
    yesterday_Date<-yesterday_Date%m-%days(1)
  }
  
  if(wday(yesterday_Date,week_start=1)%notin%c(6,7)){
    
    for(Periodicity in c("daily")){
      # Periodicity<-"daily"
      
      FX_Data_List<-list.files(file.path(path_Input,Periodicity),pattern=".feather$")
      
      pb<-progress_bar$new(total=length(FX_List))
      
      for(FX_Symbol in names(FX_List)){
        # FX_Symbol<-"CADCHF"
        
        if(paste0(FX_Symbol,"_",Periodicity,".feather")%in%FX_Data_List){
          
          FX_Data<-read_feather(file.path(path_Input,Periodicity
                                          ,paste0(FX_Symbol,"_",Periodicity,".feather")))%>%
            drop_na()%>%
            filter(date<last_Week_Delete)%>%
            select(c("symbol","date","open","high","low","close","adjusted"))
          
          if(yesterday_Date>=(max(FX_Data$date)+1)){
            
            FX_Symbol_Yahoo<-paste0(FX_Symbol,"=X")
            FX_Data_Get<-tq_get(FX_Symbol_Yahoo
                                ,get="stock.prices"
                                ,from=(max(FX_Data$date))
                                ,to=yesterday_Date
                                ,periodicity=Periodicity)%>%
              mutate(symbol=FX_Symbol)%>%
              select(c("symbol","date","open","high","low","close","adjusted"))
            
            FX_Data<-bind_rows(FX_Data,FX_Data_Get)%>%
              distinct()%>%
              drop_na()%>%
              arrange(date)
            
          }
          
        }else{
          
          FX_Data<-tibble(symbol=as.character(NA)
                          ,date=as.Date(NA)
                          ,open=as.numeric(NA)
                          ,high=as.numeric(NA)
                          ,low=as.numeric(NA)
                          ,close=as.numeric(NA)
                          ,adjusted=as.numeric(NA)
          )
          
          from_Date<-ymd("2000-01-01")
          
          FX_Symbol_Yahoo<-paste0(FX_Symbol,"=X")
          FX_Data_Get<-tq_get(FX_Symbol_Yahoo
                              ,get="stock.prices"
                              ,from=from_Date
                              ,to=yesterday_Date
                              ,periodicity=Periodicity)%>%
            mutate(symbol=FX_Symbol)%>%
            select(c("symbol","date","open","high","low","close","adjusted"))
          
          FX_Data<-bind_rows(FX_Data,FX_Data_Get)%>%
            distinct()%>%
            drop_na()%>%
            arrange(date)
          
        }
        
        write_feather(FX_Data,file.path(path_Input
                                        , Periodicity
                                        , paste0(FX_Symbol,"_",Periodicity,".feather")))
        
        pb$tick()
      }
    }
    
  }else{
    
    print("The FOREX market is closed during Saturday/Sunday!")
    
  }
  
  return("FX symbols tqGet updated!")
  
}

# forexsb ----
fnc_forexSb_FX_Data<-function(FX_List=FX_list,path_Input=path_input_forexSb
                            ,last_Week_Delete=(Sys.Date()-7)){
  # FX_List=FX_list; path_Input=path_input_forexSb; last_Week_Delete=(Sys.Date()-7)
  
  for(Periodicity in c("H1")){
    # Periodicity<-"H1"
    
    FX_Data_List<-list.files(file.path(path_Input,Periodicity),pattern=".feather$")
    
    if(Periodicity=="H1"){
      pattern_input<-"60.csv$"
    }
    
    FX_Input_List<-list.files(file.path(path_Input),pattern=pattern_input)
    
    pb<-progress_bar$new(total=length(FX_List))
    
    for(FX_Symbol in names(FX_List)){
      # FX_Symbol<-"CADCHF"
      
      FX_Input_file<-FX_Input_List[FX_Input_List%>%str_detect(FX_Symbol)]
      
      FX_Input_Data<-read_delim(file.path(path_Input,FX_Input_file), delim="\t")
      colnames(FX_Input_Data)<-c("date", "open", "high", "low", "close", "volume")
      
      FX_Input_Data<-tibble(symbol=FX_Symbol
             ,date=FX_Input_Data$date
             ,open=FX_Input_Data$open
             ,high=FX_Input_Data$high
             ,low=FX_Input_Data$low
             ,close=FX_Input_Data$close
             ,volume=FX_Input_Data$volume)%>%
        distinct()%>%
        drop_na()
      
      if(paste0(FX_Symbol,"_",Periodicity,".feather")%in%FX_Data_List){
        
        FX_Data<-read_feather(file.path(path_Input,Periodicity
                                        ,paste0(FX_Symbol,"_",Periodicity,".feather")))%>%
          drop_na()%>%
          filter(date<last_Week_Delete)%>%
          select(c("symbol","date","open","high","low","close","volume"))
        
        if(max(FX_Input_Data$date)>=max(FX_Data$date)){
          
          FX_Data<-bind_rows(FX_Data,FX_Input_Data)%>%
            distinct()%>%
            drop_na()%>%
            arrange(date)
          
        }
        
      }else{
        
        FX_Data<-FX_Input_Data%>%
          arrange(date)
        
      }
      
      write_feather(FX_Data,file.path(path_Input
                                      , Periodicity
                                      , paste0(FX_Symbol,"_",Periodicity,".feather")))
      
      pb$tick()
    }
  }
  
  return("FX symbols forexsb updated!")
  
}
