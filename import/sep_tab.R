# ------------------------------ ui object -------------------------------------
# if hidden, create blank, otherwise create the tab
if ('sep' %in% hidden_tab){
  sep_tab <- tabPanel(
    theme = shinytheme(config$app_theme), uielem$sync_excel_po)
}else{
  sep_tab <- tabPanel(
    theme = shinytheme(config$app_theme), uielem$sync_excel_po,
    fluidRow(
      useShinyalert(),  # Set up shinyalert
      box(
        width=3,
        p(), #space
        fileInput("sep_po_file", uielem$update_actual_unit_cost, 
                  accept = ".xlsx"),
        actionButton("sep_add_po", 
                     label = uielem$add_po),
        actionButton("sep_add_unit_cost", 
                     label = uielem$update_actual_unit_cost)
      ),
      box(
        width = 9,
        p(),
        DT::dataTableOutput("sep_po_data"),
        p()
      )# end box2
    )# end fluidRow
  ) # end of ui object
}