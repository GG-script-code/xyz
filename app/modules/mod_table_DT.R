
if(!require(DT)){
  install.packages("DT")
  library(DT)
}
if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}
if(!require(shinydashboardPlus)){
  install.packages("shinydashboardPlus")
  library(shinydashboardPlus)
}

source(file.path(path_functions, "fun_table_DT.R"))

table_DT_UI<-function(id){
  ns<-NS(id, "table_DT_output")

  DT::DTOutput(ns, width="auto", height="auto", fill=TRUE)

}

table_DT_server<-function(id, FX_Periodicity, FX_Selected, tbl_Type="FX_summary"){
  moduleServer(id, function(input, output, session){

    ns<-session$ns

    output$table_DT_output<-DT::renderDT(server=FALSE,{

      if(FX_Periodicity=="daily"){
        
        if(tbl_Type=="FX_summary"){
          fun_DT_FX_summary(FX_Periodicity="daily")
          
        }else if(tbl_Type=="FX_signals"){
          fun_DT_trading_signals(FX_Selected=FX_Selected()
                                  , FX_Periodicity="daily")
        }
        
      }
    })
  })
}
