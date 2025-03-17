
body<-dashboardBody(
  
  tabItems(
    # FX_academy_tab------------------------------------------------------------
    #' https://www.textfixer.com/html/convert-text-html.php 
    tabItem(tabName="FX_academy_tab"
            ,fluidRow(
              box(
                title="Academy"
                ,footer=""
                #,icon=shiny::icon("question")
                ,status="primary"
                ,solidHeader=TRUE
                ,background=NULL
                ,width=12
                ,height=NULL
                ,collapsible=TRUE
                ,collapsed=FALSE
                ,tabBox(
                  width=NULL
                  ,tabPanel(title=HTML("<b>Divergenze</b>")%>%
                              h4(align="center")
                            ,HTML(HTML_Scripts$Divergenze))
                  ,tabPanel(title=HTML("<b>RSI</b>")%>%
                              h4(align="center")
                            ,HTML(HTML_Scripts$RSI))
                  ,tabPanel(title=HTML("<b>Ichimoku</b>")%>%
                              h4(align="center")
                            ,HTML(HTML_Scripts$Ichimoku))
                  ,tabPanel(title=HTML("<b>MACD</b>")%>%
                              h4(align="center")
                            ,HTML(HTML_Scripts$MACD))
                  ,tabPanel(title=HTML("<b>BBollinger</b>")%>%
                              h4(align="center")
                            ,HTML(HTML_Scripts$BBollinger))
                  ,tabPanel(title=HTML("<b>Stochastic</b>")%>%
                              h4(align="center")
                            ,HTML(HTML_Scripts$StochasticOscillator))
                  ,tabPanel(title=HTML("<b>YZ Volatility</b>")%>%
                              h4(align="center")
                            ,HTML(HTML_Scripts$YZ_Volatility))
                )
              )
            )
    )
    # FX_summary_tab------------------------------------------------------------
    ,tabItem(tabName="FX_summary_tab"
            ,fluidRow(
              box(
                title="Summary"
                ,footer=""
                #,icon=shiny::icon("question")
                ,status="primary"
                ,solidHeader=TRUE
                ,background=NULL
                ,width=12
                ,height=NULL
                ,collapsible=TRUE
                ,collapsed=FALSE
                ,tabBox(
                  width=NULL
                  ,tabPanel(title=HTML("<b>D1 summary</b>")%>%
                              h4(align="center")
                            ,table_DT_UI("tbl_DT_FX_summary_d1")
                            ,width=12,status="info")
                  ,tabPanel(title=HTML("<b>Economic Calendar</b>")%>%
                              h4(align="center")
                            ,table_DT_UI("")
                            ,width=12,status="info")
                )
              )
              ,box(
                title="Correlation"
                ,footer=""
                #,icon=shiny::icon("question")
                ,status="primary"
                ,solidHeader=TRUE
                ,background=NULL
                ,width=12
                ,height=NULL
                ,collapsible=TRUE
                ,collapsed=FALSE
                ,tabBox(
                  width=NULL
                  ,tabPanel(title=HTML("<b>D1 correlation</b>")%>%
                              h4(align="center")
                            ,graph_ggplot_UI("grp_ggplot_FX_correlation_d1")
                            ,width=12,status="info")
                )
              )
            )
    )
    # FX_daily_tab--------------------------------------------------------------
    ,tabItem(
      tabName="FX_D1_tab"
      ,fluidRow(
        box(
          title="FX D1 signals"
          ,footer=""
          #,icon=shiny::icon("question")
          ,status="primary"
          ,solidHeader=TRUE
          ,background=NULL
          ,width=12
          ,height=NULL
          ,collapsible=TRUE
          ,collapsed=FALSE
          ,tabBox(
            width=NULL
            ,tabPanel(title=HTML("<b>FX signals</b>")%>%
                        h4(align="center")
                      ,table_DT_UI("tbl_DT_FX_signals_d1")
                      ,width=12,status="info")
          )
        )
        ,box(
          title="FX D1 graphs"
          ,footer=""
          ,status="primary"
          ,solidHeader=TRUE
          ,background=NULL
          ,width=12
          ,height=NULL
          ,collapsible=TRUE
          ,collapsed=FALSE
          ,tabBox(width=NULL
                  ,tabPanel(title=HTML("<b>D1 time series</b>")%>%
                              h4(align="center")
                            ,graph_plotly_UI("grp_plotly_FX_candlestick_d1")
                            ,width=12,status="info")
                  ,tabPanel(title=HTML("<b>D1 Volatility</b>")%>%
                              h4(align="center")
                            ,graph_plotly_UI("grp_plotly_FX_volatility_d1")
                            ,width=12,status="info")
                  ,tabPanel(title=HTML("<b>D1 Trend Following</b>")%>%
                              h4(align="center")
                            ,graph_plotly_UI("grp_plotly_FX_trendFollowing_d1")
                            ,width=12,status="info")
                  ,tabPanel(title=HTML("<b>D1 Momentum</b>")%>%
                              h4(align="center")
                            ,graph_plotly_UI("grp_plotly_FX_momentum_d1")
                            ,width=12,status="info")
                  ,tabPanel(title=HTML("<b>D1 MACD</b>")%>%
                              h4(align="center")
                            ,graph_plotly_UI("grp_plotly_FX_macd_d1")
                            ,width=12,status="info")
          )
        )
      )
    )
    #---------------------------------------------------------------------------
  )
)
