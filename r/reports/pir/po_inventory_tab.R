# functions to create the ui for po_report tab (por)
if('po_inventory' %in% hidden_tab){
  po_inventory_tab <- tabPanel(uielem$po_inventory)
}else{
  po_inventory_tab <- tabPanel(uielem$po_inventory,
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
                   label = '',
                   choices = list(uielem$value_report,
                                  uielem$po_report,
                                  uielem$separate_lot,
                                  uielem$expiry_first)
                  ),
      actionButton(inputId = "print_inventory_report", 
                   label = uielem$printReport),
      p()
    )
  )
)
}
