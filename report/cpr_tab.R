
# customer_price_report
if('cpr' %in% hidden_tab){
  cpr_tab <- tabPanel(uielem$customer_price_report)
}else{
  cpr_tab <- tabPanel(
    uielem$customer_price_report,
    fluidRow(
      box(
        width=3, height = 800,
        p(),
        htmlOutput("cpr_customer"),
        p()
      ), # end left side
      box(
        width=9, height = 800,
        p()
      ) # end right side
    )
  )
}
