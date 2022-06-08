# ------------------------------ ui object -------------------------------------
# if hidden, create blank, otherwise create the tab
if ('mdn' %in% hidden_tab){
  mdn_tab <- tabPanel(
    theme = shinytheme(config$app_theme), uielem$manage_delivery_note)
}else{
  mdn_tab <- tabPanel(
  theme = shinytheme(config$app_theme), uielem$manage_delivery_note,
  fluidRow(
    box(
      width=3,
      p(), #space
      htmlOutput('mdn_pxk_num'),
      actionButton("print_mdn_pxk", label = uielem$print_delivery_note),
      p()
    )
    # ,
    # box(
    #   width = 9,
    #   p(),
    #   htmlOutput("mdn_pxk_data"),
    #   DT::dataTableOutput("mdn_pxk_data"),
    #   p()
    # )# end inv_out box2
  )# end inv_out fluidRow
) # end of ui object
}
