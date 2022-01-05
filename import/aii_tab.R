# ------------------------------ ui object -------------------------------------
# if hidden, create blank, otherwise create the tab
if ('aii' %in% hidden_tab){
  aii_tab <- tabPanel(
    theme = shinytheme(config$app_theme), uielem$add_import_item)
}else{
  aii_tab <- tabPanel(
    theme = shinytheme(config$app_theme), uielem$add_import_item,
    fluidRow(
      box(
        width=3,
        p(), #space
        htmlOutput('aii_prod_name'),
        div(style="display: inline-block;vertical-align:top;width: 135px",
            htmlOutput('aii_vendor')),
        div(style="display: inline-block;vertical-align:top;width: 135px",
            htmlOutput('aii_invoice_num')),
        div(style="display: inline-block;vertical-align:top;width: 135px",
            htmlOutput("aii_qty")),
        div(style="display: inline-block;vertical-align:top;width: 135px",
            htmlOutput("aii_unit")),
        div(style="display: inline-block;vertical-align:top;width: 135px",
            htmlOutput("aii_lot")),
        div(style="display: inline-block;vertical-align:top;width: 135px",
            htmlOutput("aii_exp_date")),
        div(style="display: inline-block;vertical-align:top;width: 200px",
            htmlOutput("aii_unit_cost")),
        div(style="display: inline-block;vertical-align:top;width: 70px",
            htmlOutput("aii_vat_percent")),
        div(style="display: inline-block;vertical-align:top;width: 100px",
            htmlOutput('aii_invoice_warehouse')),
        div(style="display: inline-block;vertical-align:top;width: 170px",
            textInput("aii_note",uielem$note)),
        actionButton("aii_add_entry", label = uielem$add_import_item),
        p()
      ),
      box(
        width = 9,
        p(),
        DT::dataTableOutput("aii_import_data"),
        p()
      )# end box2
    )# end fluidRow
  ) # end of ui object
}