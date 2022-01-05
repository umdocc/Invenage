# inv_out tab ui and functions
# ---------------------------- shiny ui object --------------------------------
if('ilr' %in% hidden_tab){
  ilr_tab <- tabPanel(uielem$import_log_report)
}else{
  ilr_tab <- tabPanel(
  theme = shinytheme("united"), uielem$import_log_report,
  fluidRow(
    box(
      width=3, height = 800,
      h3(uielem$import_log),
      dateInput(inputId = "ilr_from_date",
                label = uielem$from_date,
                value = Sys.Date(),
                format = config$display_date_format),
      dateInput(inputId = "ilr_to_date",
                label = uielem$to_date,
                value = Sys.Date(),
                format = config$display_date_format),
      actionButton(inputId = "print_import_log_report",
                   label = uielem$printReport),
      p()
    ),
    box(
      width=9, height = 800,
      h3(uielem$import_log),
      # DT::dataTableOutput("import_log_report_tbl"),
      p() #space
    )
  )
)
}