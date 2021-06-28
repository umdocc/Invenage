# functions to create the ui for po_report tab (por)
if('po_inventory' %in% hidden_tab){
  po_inventory_tab <- tabPanel(uielem$po_inventory)
}else{
  po_inventory_tab <- tabPanel(uielem$po_inventory,
  fluidRow(
    box(
      width=3, height = 800,
      p(),
      h3(uielem$po_report),
      selectInput(inputId = 'por_vendor',
                  label = get_actual('vendor'),
                  choices = db_read_query(
                    "select vendor from vendor_info 
                    where import_from=1")$vendor),
      actionButton(inputId = "print_po_report", 
                   label = uielem$printReport),
      p()
    ),
    box(
      width=3, height = 800,
      p(),
      h3(uielem$current_inventory_report),
      selectInput(inputId = 'cir_vendor',
                  label = get_actual('vendor'),
                  choices = db_read_query(
                    "select vendor from vendor_info")$vendor),
      dateInput(inputId = "cir_to_date",
                label = uielem$to_date,
                value = Sys.Date(),
                format = config$display_date_format),
      checkboxInput(inputId = 'cir_separate_lot',
                    label = uielem$cir_separate_lot,
                    value = F),
      checkboxInput(inputId = 'cir_expiry_first',
                    label = uielem$cir_expiry_first,
                    value = F),
      checkboxInput(inputId = 'cir_value_report',
                    label = uielem$cir_value_report,
                    value = F),
      actionButton(inputId = "print_inventory_report", 
                   label = uielem$printReport),
      p()
    ),
    box(
      width=3, height = 800, 
      h3(uielem$sale_log),
      p()
    ),
    box(
      width=3, height = 800, 
      h3(uielem$import_log),
      p()
    )
  )
)
}
