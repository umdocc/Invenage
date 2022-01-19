# functions to create the ui for po_report tab (por)
if('pir' %in% hidden_tab){
  pir_tab <- tabPanel(uielem$po_inventory_report)
}else{
  pir_tab <- tabPanel(uielem$po_inventory_report,
  fluidRow(
    box(
      width=3, height = 800,
      p(),
      h3(uielem$po_inventory),
      selectInput(inputId = 'pir_vendor',
                  label = uielem$vendor,
                  choices = db_read_query(
                    "select vendor from vendor_info")$vendor),
      dateInput(inputId = "pir_to_date",
                label = uielem$to_date,
                value = Sys.Date(),
                format = config$display_date_format),
      radioButtons(inputId = 'pir_report_type',
                   label = uielem$option,
                   choices = list(uielem$value_report,
                                  uielem$po_report,
                                  uielem$separate_lot,
                                  uielem$expiry_first)
                  ),
      actionButton(inputId = "pir_print_report", 
                   label = uielem$print_report),
      p(),
    ),
    box(
      width=9, height = 800,
      DT::dataTableOutput("pir_data"),
      p() #space
    )
  )
)
}
