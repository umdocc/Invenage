# import price renderers
# render the current import price, with option to create new price
# default will look in update_import_price tab for the selected product
render_import_price <- function(
  input,iid,tab='update_import_price',allow_add=T){renderUI({
    # depend on the tab, will output choices, selected and allow_add
    if (tab=='update_import_price'){
      # get the product code
      sel_prod_code <- product_info$prod_code[
        product_info$search_str==input$uip_prod_name]
      # get the vendor id
      sel_vendor_id <- vendor_info$vendor_id[
        vendor_info$vendor==input$uip_vendor]
      # get the current import price
      current_import_price <- import_price$import_price[
        import_price$prod_code==sel_prod_code&
          import_price$vendor_id==sel_vendor_id]
      selected_price <- current_import_price[1]
    }
    selectizeInput(
      inputId = iid, label = ui_elem$actual[ui_elem$label=='import_price'],
      choices = current_import_price, selected =  selected_price,
      options = list(create=allow_add)
    )
  })
}