# functions to create the ui for po_report tab (por)
if('slr' %in% hidden_tab){
  slr_tab <- tabPanel(uielem$sale_log_report)
}else{
  slr_tab <- tabPanel(
    uielem$sale_log_report,
    div(style="display: inline-block;vertical-align:top;width: 300px",
        htmlOutput("slr_prod_name")),
    div(style="display: inline-block;vertical-align:top;width: 240px",
        htmlOutput("slr_customer")),
    div(style="display: inline-block;vertical-align:top;width: 210px",
      htmlOutput("slr_pxk_num")),
    div(style="display: inline-block;vertical-align:bottom; \
        position:absolute;right:15px",
        actionButton("slr_reload", uielem$reload)),
    DT::dataTableOutput("slr_data"),
    div(style="display: inline-block;vertical-align:bottom; \
                        position:absolute;right:15px",
        actionButton("slr_print_report", uielem$print_report)),
    p(),
    h4(uielem$edit_data),
    div(style="display: inline-block;vertical-align:top;width: 150px",
        htmlOutput("slr_pxk_lineid")),
    div(style="display: inline-block;vertical-align:top;width: 150px",
        htmlOutput("slr_pxk_line_col")),
    div(style="display: inline-block;vertical-align:top;width: 150px",
        htmlOutput("slr_pxk_line_col_content")),
    p(),
    div(style="display: inline-block;vertical-align:top;width: 150px",
        actionButton("slr_del_line",uielem$del_line)),
    p(),
    div(style="display: inline-block;vertical-align:top;width: 150px",
        actionButton("slr_edit_line",uielem$edit_data)),
    p()
  )
  
}