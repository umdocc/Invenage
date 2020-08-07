if ('write_po_price' %in% hidden_tab){
  write_po_price_tab <- tabPanel(ui_elem$actual[ui_elem$label=='write_po_price'])
}else{
  write_po_price_tab <- tabPanel(
    ui_elem$actual[ui_elem$label=='write_po_price'],
    fluidRow(
      useShinyalert(),  # Set up shinyalert
      # add_product box
      box(width = 3, height = 400,
          h3('tmp')
      )
    )
  )
}





