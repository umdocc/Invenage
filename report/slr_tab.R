# functions to create the ui for po_report tab (por)
if('slr' %in% hidden_tab){
  slr_tab <- tabPanel(uielem$sale_log_report)
}else{
  slr_tab <- tabPanel(uielem$sale_log_report,
    fluidRow(
      box(width=3,
          p(),
          htmlOutput("slr_pxk_num"),
          p()
      )
      ,
      box(width=9,
        DT::dataTableOutput("slr_data")
      )
    )
  )
}