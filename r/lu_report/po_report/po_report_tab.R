# functions to create the ui for po_report tab (por)
if('po_report' %in% hidden_tab){
  po_report_tab <- tabPanel(ui_elem$actual[ui_elem$label=='po_report'])
}else{
  po_report_tab <- tabPanel(
  ui_elem$actual[ui_elem$label=='po_report'],
  fluidRow(
    style = "background-color:#f5f5f5;",
    box(
      width=3, height = 800,
      selectInput(
        inputId = 'por_vendor_select',
        label = get_actual('vendor'),
        choices = db_read_query(
          "select vendor from vendor_info where import_from=1")$vendor
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
