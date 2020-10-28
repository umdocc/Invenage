# the tab to look at all number summary
# the get_sales_data return a data frame based on filter requirements

if('manager' %in% hidden_tab){
  manager_tab <- tabPanel(ui_elem$actual[ui_elem$label=='manager'])
}else{
  manager_tab <- tabPanel(
    theme = shinytheme("united"), 
    ui_elem$actual[ui_elem$label=='manager'],
    fluidRow(
      box(
        width=3, height = 800,
        h3('temp')
      ),
      box(
        width=9, height = 800,
        h3('temp')
      )
    )
  )
}