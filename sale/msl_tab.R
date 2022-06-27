# functions to create the ui for po_report tab (por)
if('msl' %in% hidden_tab){
  msl_tab <- tabPanel(uielem$manage_sale_log)
}else{
  msl_tab <- tabPanel(
    uielem$manage_sale_log,
    div(style="display: inline-block;vertical-align:top;width: 300px",
        htmlOutput("msl_prod_name")),
    div(style="display: inline-block;vertical-align:top;width: 240px",
        htmlOutput("msl_customer")),
    div(style="display: inline-block;vertical-align:top;width: 210px",
      htmlOutput("msl_pxk_num")),
    div(style="display: inline-block;vertical-align:bottom; \
        position:absolute;right:15px",
        actionButton("msl_reload", uielem$reload)),
    DT::dataTableOutput("msl_data"),
    div(style="display: inline-block;vertical-align:bottom; \
                        position:absolute;right:15px",
        actionButton("msl_print_report", uielem$print_report)),
    p(),
    h4(uielem$edit_data),
    div(style="display: inline-block;vertical-align:top;width: 150px",
        htmlOutput("msl_pxk_lineid")),
    div(style="display: inline-block;vertical-align:top;width: 150px",
        htmlOutput("msl_pxk_line_col")),
    div(style="display: inline-block;vertical-align:top;width: 270px",
        htmlOutput("msl_pxk_line_col_content")),
    div(style="display: inline-block;vertical-align:top;width: 150px",
        htmlOutput("msl_confirm_code")),
    p(),
    div(style="display: inline-block;vertical-align:bottom",
        actionButton("msl_del_line",uielem$del_line)),
    div(style="display: inline-block;vertical-align:bottom",
        htmlOutput("msl_del_line_explan")),
    p(),
    div(style="display: inline-block;vertical-align:bottom",
        actionButton("msl_edit_line",uielem$edit_data)),
    div(style="display: inline-block;vertical-align:bottom",
        htmlOutput("msl_edit_line_explan")),
    p()
  )
  
}