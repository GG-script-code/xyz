
# Correlation Matrix ----
fnc_ggplot_FX_correlation<-function(FX_Periodicity="daily" # "daily" "weekly"
                                    ,path_Output_Metrics=path_output_metrics){
  # FX_Periodicity="weekly";path_Output_Metrics=path_output_metrics
  
  FX_Corr_List<-list.files(file.path(path_Output_Metrics,FX_Periodicity),pattern="^corr_spearman.*Rds")
  FX_Corr_File<-FX_Corr_List[1]
  FX_Corr_Data<-read_rds(file.path(path_Output_Metrics,FX_Periodicity,FX_Corr_File))
  
  yearFrac<-fmdates::year_frac(FX_Corr_Data$from_to_dates[1]
                               ,FX_Corr_Data$from_to_dates[2]
                               ,day_basis="act/365")%>%
    round(digits=2)
  
  # reduce the size of correlation matrix
  melted_corr_mat<-melt(FX_Corr_Data$corr)
  
  # Correlation
  plot_Correlation<-ggplot(data=melted_corr_mat,aes(reorder(Var2,-desc(as.character(Var2))),
                                                    reorder(Var1,-desc(as.character(Var1)))
                                                    ,fill=value))+
    geom_tile(color="white")+
    geom_text(aes(Var2, Var1, label=value), 
              color="black", size=4)+
    scale_fill_gradient2(low="blue",high="red",mid="white",
                         midpoint=0,limit=c(-1,1),space="Lab",
                         name="Spearman\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x=element_text(angle=45,vjust=1,
                                   size=12,hjust=1))+
    labs(title=paste0(FX_Periodicity%>%toupper(), " correlation from : ", FX_Corr_Data$from_to_dates[1]
                      ," to : ", FX_Corr_Data$from_to_dates[2]
                      , " (", yearFrac, " years)")
         , x="", y="")+
    coord_fixed()
  
  return(plot_Correlation)
  gc()
}







