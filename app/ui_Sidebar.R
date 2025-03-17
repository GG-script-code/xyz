#' https://rstudio.github.io/shinydashboard/structure.html#sidebar

slcInp_FXrate_list<-rlist::list.flatten(FX_list)


myConditionalPanel_Graphs_UI<-function(id,input_Sidebar="FX_D1_tab"
                                       ,select_Input_ID="slcInp_FXrate_d1"
                                       ,action_Button_ID="buttInp_getFXgraphs_d1"
                                       ,title_HTML="Forex daily data"){
  #ns<-NS(id)
  return(
    
    conditionalPanel(
      paste0("input.sidebarid == '",input_Sidebar,"'")
      ,HTML(paste0("&nbsp;&nbsp;<b>",title_HTML,"</b>"))%>%h4()
      ,selectizeInput(inputId=select_Input_ID
                      ,label="Select FOREX:"
                      ,choices=list(`FOREX`=slcInp_FXrate_list)
                      ,multiple=F,selected=NULL
                      ,width="250px")
      ,HTML("&nbsp;&nbsp;<b>Click to get the results</b>")%>%h5()
      ,actionButton(inputId=action_Button_ID
                    ,label="Compute",icon=icon("laptop-code")
                    ,style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                    ,width="200px",disabled=FALSE)
      #,ns=NS(id)
    )
    
  )
  
}

sidebar<-shinydashboardPlus::dashboardSidebar(
  disable=FALSE
  ,width=300
  ,sidebarMenu(
    id="sidebarid"
    #---------------------------------------------------------------------------
    ,menuItem(text="FOREX academy"
              ,tabName="FX_academy_tab"
              ,icon=icon("book-open-reader")
    )
    #---------------------------------------------------------------------------
    ,menuItem(text="FOREX summary"
              ,tabName="FX_summary_tab"
              ,icon=icon("pen-ruler")
    )
    #---------------------------------------------------------------------------
    ,menuItem(text="FOREX D1 data"
              ,tabName="FX_D1_tab"
              ,icon=icon("chart-line")
    )
    ,myConditionalPanel_Graphs_UI("FX_D1_tab",input_Sidebar="FX_D1_tab"
                                  ,select_Input_ID="slcInp_FXrate_d1"
                                  ,action_Button_ID="buttInp_getFXgraphs_d1"
                                  ,title_HTML="Forex daily data")
    #---------------------------------------------------------------------------
  )
)
