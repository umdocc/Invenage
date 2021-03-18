# ----------------------------- update_customer_tab ----------------------------
if ('import_performance' %in% hidden_tab){
  import_performance_tab <- tabPanel(
    get_actual('import_performance'))
}else{
  import_performance_tab <- tabPanel(
    get_actual('import_performance'),
    fluidRow(
      useShinyalert(),  # Set up shinyalert
      box(width = 3, style = "background-color:#f5f5f5;",
          # htmlOutput("ip_vendor_name"),
      ),
      box(width = 9, height = 800, style = "background-color:#f5f5f5;",
          # DT::dataTableOutput("import_performance_summary")
      )
    )
  )
}
