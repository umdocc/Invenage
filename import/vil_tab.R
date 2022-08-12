# functions to create the ui for po_report tab (por)
if('vii' %in% hidden_tab){
  vii_tab <- tabPanel(uielem$vendor_import_invoice)
}else{
  vii_tab <- tabPanel(
    uielem$vendor_import_invoice,
    fluidRow(
      htmlOutput("vii_vendor"),
      # DT::dataTableOutput("vii_data"),
      actionButton("vii_add_data", uielem$add_data),
      htmlOutput("vii_invoice_num"),
      htmlOutput("vii_amount"),
      htmlOutput("vii_invoice_cdn"),
      # h4(uielem$edit_data),
      # div(style="display: inline-block;vertical-align:middle;width: 150px",
      #     htmlOutput("mil_lineid")),
      # div(style="display: inline-block;vertical-align:middle;width: 150px",
      #     htmlOutput("mil_line_col")),
      # div(style="display: inline-block;vertical-align:middle;width: 150px",
      #     htmlOutput("mil_line_col_content")),
      # div(style="display: inline-block;vertical-align:middle;width: 150px",
      #     htmlOutput("mil_confirm_code")),
      # p(),
      # div(style="display: inline-block;vertical-align:middle",
      #     actionButton("mil_del_line",uielem$del_line)),
      # div(style="display: inline-block;vertical-align:middle",
      #     htmlOutput("mil_del_line_explan")),
      # p(),
      # div(style="display: inline-block;vertical-align:middle",
      #     actionButton("mil_edit_line",uielem$edit_data)),
      # div(style="display: inline-block;vertical-align:middle",
      #     htmlOutput("mil_edit_line_explan")),
      p()
      )
  )
}