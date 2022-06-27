# functions to create the ui for po_report tab (por)
if('mil' %in% hidden_tab){
  mil_tab <- tabPanel(uielem$manage_import_log)
}else{
  mil_tab <- tabPanel(
    uielem$manage_import_log,
    fluidRow(
      htmlOutput("mil_prod_filter"),
      DT::dataTableOutput("mil_data"),
      actionButton("mil_print_report", uielem$print_report),
      p(),
      h4(uielem$edit_data),
      div(style="display: inline-block;vertical-align:middle;width: 150px",
          htmlOutput("mil_lineid")),
      div(style="display: inline-block;vertical-align:middle;width: 150px",
          htmlOutput("mil_line_col")),
      div(style="display: inline-block;vertical-align:middle;width: 150px",
          htmlOutput("mil_line_col_content")),
      div(style="display: inline-block;vertical-align:middle;width: 150px",
          htmlOutput("mil_confirm_code")),
      p(),
      div(style="display: inline-block;vertical-align:middle",
          actionButton("mil_del_line",uielem$del_line)),
      div(style="display: inline-block;vertical-align:middle",
          htmlOutput("mil_del_line_explan")),
      p(),
      div(style="display: inline-block;vertical-align:middle",
          actionButton("mil_edit_line",uielem$edit_data)),
      div(style="display: inline-block;vertical-align:middle",
          htmlOutput("mil_edit_line_explan")),
      p()
      )
  )
}