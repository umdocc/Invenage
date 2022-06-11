# functions to create the ui for po_report tab (por)
if('slr' %in% hidden_tab){
  slr_tab <- tabPanel(uielem$sale_log_report)
}else{
  slr_tab <- tabPanel(uielem$sale_log_report,
    fluidRow(
      box(width=2,
          htmlOutput("slr_pxk_num"),
          htmlOutput("slr_pxk_stt"),
          p()
      )
      ,
      box(width=10,
        DT::dataTableOutput("slr_data")
      )
    )
  )
}