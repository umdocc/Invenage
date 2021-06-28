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
      checkboxInput(inputId = 'pir_po_report',
                    label = uielem$po_report,
                    value = F),
      checkboxInput(inputId = 'pir_separate_lot',
                    label = uielem$cir_separate_lot,
                    value = F),
      checkboxInput(inputId = 'pir_expiry_first',
                    label = uielem$cir_expiry_first,
                    value = F),
      checkboxInput(inputId = 'pir_value_report',
                    label = uielem$cir_value_report,
                    value = F),
      actionButton(inputId = "print_inventory_report", 
                   label = uielem$printReport),
      p()
    )
  )
)
}
