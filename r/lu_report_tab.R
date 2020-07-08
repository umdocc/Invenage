# functions to create the ui for report tab
lu_report_tab <- tabPanel(
  ui_elem$actual[ui_elem$label=='lu_report'],
  fluidRow(
    style = "background-color:#f5f5f5;",
    box(
      width=3, height = 800,
      selectInput(
        inputId = 'lu_report_group_selector',
        label = ui_elem$actual[ui_elem$label=='choose_group'],
        choices = report_group),     
      htmlOutput('lu_report_tbl_selector'),
      actionButton(
        "print_lu_report",
        ui_elem$actual[ui_elem$label=='printReport'])
    ),
    box(
      width=9, height = 800,
      DT::dataTableOutput('lu_report_tbl')
    )
  )
)

