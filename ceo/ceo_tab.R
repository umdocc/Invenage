# ------------------------------ ui object -------------------------------------
# if hidden, create blank, otherwise create the tab
if ('ceo' %in% hidden_tab){
  ceo_tab <- tabPanel(
    theme = shinytheme(config$app_theme), uielem$ceo)
}else{
  ceo_tab <- tabPanel(
    theme = shinytheme(config$app_theme), uielem$ceo,
    fluidRow(
      box(
        width=3,
        p(), #space
        p()
      ),
      box(
        width = 9,
        p(),
        p()
      )# end box2
    )# end fluidRow
  ) # end of ui object
}