# ------------------------------ ui object -------------------------------------
# if hidden, create blank, otherwise create the tab
if ('sep' %in% hidden_tab){
  sep_tab <- tabPanel(
    theme = shinytheme(config$app_theme), uielem$sync_excel_po)
}else{
  sep_tab <- tabPanel(
    theme = shinytheme(config$app_theme), uielem$sync_excel_po,
    fluidRow(
      box(
        width=3,
        p(), #space
        htmlOutput('sep_po_name'),
        actionButton("sep_sync_po", label = uielem$sync_excel_po),
        p()
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