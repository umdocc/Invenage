# functions to create the ui for po_report tab (por)
if('slr' %in% hidden_tab){
  slr_tab <- tabPanel(uielem$sale_log_report)
}else{
  slr_tab <- tabPanel(
    uielem$sale_log_report,
    htmlOutput("slr_pxk_num"),
    htmlOutput("slr_customer"),
    htmlOutput("slr_prod_name"),
    DT::dataTableOutput("slr_data"),
    p(),
    div(style="display: inline-block;vertical-align:top",
        HTML(paste("<font size='+1'>",uielem$del_stt,'</font>'))),
    div(style="display: inline-block;vertical-align:top;width: 90px",
        htmlOutput("slr_pxk_stt")),
    div(style="display: inline-block;vertical-align:top;width: 150px",
        actionButton("slr_del_stt",uielem$delete))
  )
  
}