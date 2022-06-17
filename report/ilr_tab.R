# functions to create the ui for po_report tab (por)
if('ilr' %in% hidden_tab){
  ilr_tab <- tabPanel(uielem$import_log_report)
}else{
  ilr_tab <- tabPanel(
    uielem$import_log_report,
    fluidRow(
      htmlOutput("ilr_prod_filter"),
      DT::dataTableOutput("ilr_data"),
      actionButton("ilr_print_report", uielem$print_report),
      p(),
      h4(uielem$edit_data),
      div(style="display: inline-block;vertical-align:middle;width: 150px",
          htmlOutput("ilr_lineid")),
      div(style="display: inline-block;vertical-align:middle;width: 150px",
          htmlOutput("ilr_pxk_line_col")),
      div(style="display: inline-block;vertical-align:middle;width: 150px",
          htmlOutput("ilr_line_col_content")),
      p(),
      div(style="display: inline-block;vertical-align:middle;width: 150px",
          actionButton("ilr_del_line",uielem$del_line)),
      p(),
      div(style="display: inline-block;vertical-align:middle;width: 150px",
          actionButton("ilr_edit_line",uielem$edit_data)),
      p()
      )
  )
}