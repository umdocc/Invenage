# import price renderers
# render the current import price, with option to create new price
# default will look in update_import_price tab for the selected product
render_prod_import_price <- function(input,iid,tab='update_import_price'){
  renderUI({
    # depend on the tab, will output choices, selected and allow_add
    if (tab=='update_import_price'){
      # get the product code
      sel_prod_code <- product_info$prod_code[
        product_info$search_str==input$uip_prod_name]
      # get the current import price
      current_import_price <- import_price$import_price[import_price]
    }
    
    
    selectizeInput(
      inputId = iid, label = ui_elem$actual[ui_elem$label=='tender_name'],
      choices = tender_choices, selected =  default_tender,
      options = list(create=T)
    )
  })
}