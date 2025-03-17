
source(file.path(path_functions,"fun_graph_ggplot.R"))

#-------------------------------------------------------------------------------

graph_ggplot_UI<-function(id){
  ns<-NS(id,"graph_ggplot_output")
  
  shiny::plotOutput(ns,width="100%",height="500px")
}

graph_ggplot_server<-function(id,FX_Periodicity,grp_Type="FX_correlation"){
  moduleServer(id,function(input,output,session){
    
    ns<-session$ns
    
    output$graph_ggplot_output<-shiny::renderPlot({
      
      if(FX_Periodicity=="daily"){
        
        if(grp_Type=="FX_correlation"){
          fnc_ggplot_FX_correlation(FX_Periodicity="daily")
        }
        
      }
    })
  })
}



