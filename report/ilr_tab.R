# functions to create the ui for po_report tab (por)
if('ilr' %in% hidden_tab){
  ilr_tab <- tabPanel(uielem$import_log_report)
}else{
  ilr_tab <- tabPanel(
    uielem$import_log_report,
    fluidRow(
      htmlOutput("ilr_prod_filter"),
      DT::dataTableOutput("ilr_data"),
      actionButton(
        inputId = "ilr_print_report",
        label = uielem$print_report)
      )
  )
}