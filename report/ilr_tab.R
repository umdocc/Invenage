# functions to create the ui for po_report tab (por)
if('ilr' %in% hidden_tab){
  ilr_tab <- tabPanel(uielem$import_log_report)
}else{
  ilr_tab <- tabPanel(uielem$import_log_report,
    fluidRow(
      box(width=3, height = 800,
        p()
      )
    )
  )
}