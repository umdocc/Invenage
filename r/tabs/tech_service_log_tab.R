# ----------------------------- update_customer_tab ----------------------------
if ('tech_service_log' %in% hidden_tab){
  tech_service_log_tab <- tabPanel(
    get_actual('tech_service_log'))
}else{
  tech_service_log_tab <- tabPanel(
    get_actual('tech_service_log'),
    fluidRow(
      useShinyalert(),  # Set up shinyalert
      box(width = 3, height = 400, style = "background-color:#f5f5f5;",
          htmlOutput("tsl_engineer_name"),
          htmlOutput("tsl_customer_name"),
          htmlOutput("tsl_analyser_name"),
          htmlOutput("tsl_analyser_lot"),
          htmlOutput("tsl_job_desc"),
          htmlOutput("tsl_job_detail"),
      ),
      box(width = 9, height = 800, style = "background-color:#f5f5f5;",
          DT::dataTableOutput("tsl_out_tbl")
      )
    )
  )
}