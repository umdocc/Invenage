# functions to create the ui for po_report tab (por)
if('po_inventory' %in% hidden_tab){
  po_inventory_tab <- tabPanel(ui_elem$actual[ui_elem$label=='po_inventory'])
}else{
  po_inventory_tab <- tabPanel(
  ui_elem$actual[ui_elem$label=='po_inventory'],
  fluidRow(
    style = "background-color:#f5f5f5;",
    box(
      width=3, height = 800,
      h2(get_actual('po_report')),
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
      # width=9, height = 800,
      # DT::dataTableOutput('lu_report_tbl')
    )
  )
)
}
