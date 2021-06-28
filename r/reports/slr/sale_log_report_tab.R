# inv_out tab ui and functions
# ---------------------------- shiny ui object --------------------------------
if('sale_log_report' %in% hidden_tab){
  sale_log_report_tab <- tabPanel(uielem$sale_log)
}else{
  sale_log_report_tab <- tabPanel(
  theme = shinytheme("united"), uielem$sale_log,
  fluidRow(
    box(
      width=3, height = 800,
      h3(uielem$sale_log),
      dateInput(inputId = "slr_from_date",
                label = uielem$from_date,
                value = Sys.Date(),
                format = config$display_date_format),
      dateInput(inputId = "slr_to_date",
                label = uielem$to_date,
                value = Sys.Date(),
                format = config$display_date_format),
      actionButton(inputId = "print_sale_log_report",
                   label = uielem$printReport),
      p()
    ),
    box(
      width=9, height = 800,
      h3(uielem$sale_log),
      DT::dataTableOutput("sale_log_report_tbl"),
      p() #space
    )
  )
)
}