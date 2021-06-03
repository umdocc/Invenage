# functions to create the ui for po_report tab (por)
if('po_inventory' %in% hidden_tab){
  po_inventory_tab <- tabPanel(uielem$po_inventory)
}else{
  po_inventory_tab <- tabPanel(uielem$po_inventory,
  fluidRow(
    box(
      width=3, height = 800,
      p(),
      h3(get_actual('po_report')),
      selectInput(
        inputId = 'por_vendor',
        label = get_actual('vendor'),
        choices = db_read_query(
          "select vendor from vendor_info where import_from=1")$vendor
        ),
      actionButton(
        "print_po_report", get_actual('printReport')
        ),
      p()
    ),
    box(
      width=3, height = 800,
      p(),
      h3(get_actual('current_inventory_report')),
      selectInput(
        inputId = 'cir_vendor',
        label = get_actual('vendor'),
        choices = db_read_query(
          "select vendor from vendor_info")$vendor
      ),
      dateInput(inputId = "cir_to_date",
                label = get_actual("to_date"),
                value = Sys.Date(),
                format = config$display_date_format
      ),
      checkboxInput(inputId = 'cir_separate_lot',
                    label = get_actual("cir_separate_lot"),
                    value = F
      ),
      actionButton(
        "print_inventory_report", get_actual('printReport')
      ),
      p()
    ),
    box(
      width=3, height = 800, style = 
      verbatimTextOutput("\n"),
      h3(get_actual('sale_log')),
      p()
    ),
    box(
      width=3, height = 800, 
      verbatimTextOutput("\n"),
      h3(get_actual('import_log')),
      p()
    )
  )
)
}
