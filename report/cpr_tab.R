# customer_price_report
if('cpr' %in% hidden_tab){
  cpr_tab <- tabPanel(uielem$customer_price_report)
}else{
  cpr_tab <- tabPanel(
    uielem$customer_price_report,
    fluidRow(
        htmlOutput("cpr_customer"),
        actionButton("cpr_create_report",uielem$print_report)
    )
  )
}
