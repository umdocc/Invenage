if ('po_man' %in% hidden_tab){
  po_man_tab <- tabPanel(ui_elem$actual[ui_elem$label=='po_man'])
}else{
  po_man_tab <- tabPanel(
    ui_elem$actual[ui_elem$label=='po_man'],
    fluidRow(
      useShinyalert(),  # Set up shinyalert
      # add_product box
      box(width = 3, height = 400,
          htmlOutput('po_man_po_list')
      ),
      box(width = 9, height = 400,
          DT::dataTableOutput("po_man_po_detail"),
      )
    )
  )
}






