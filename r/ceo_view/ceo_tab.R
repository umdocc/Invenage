# ----------------------------- update_customer_tab ----------------------------
if ('vendor_import_performance' %in% hidden_tab){
  vendor_import_performance_tab <- tabPanel(
    get_actual('vendor_import_performance'))
}else{
  vendor_import_performance_tab <- tabPanel(
    get_actual('vendor_import_performance'),
    fluidRow(
      useShinyalert(),  # Set up shinyalert
      box(width = 3, style = "background-color:#f5f5f5;",
          p('this is the sidebar')
          # htmlOutput("ip_vendor_name"),
      ),
      box(width = 9, style = "background-color:#f5f5f5;",
          # DT::dataTableOutput("import_performance_summary")
      )
    )
  )
}
