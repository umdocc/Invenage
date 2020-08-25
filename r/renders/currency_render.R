render_currency <- function(input,iid,allow_add=T){renderUI({
  if(iid=='uip_currency'){
    # get the product code
    sel_prod_code <- product_info$prod_code[
      product_info$search_str==input$uip_prod_name]
    # get the vendor id
    sel_vendor_id <- vendor_info$vendor_id[
      vendor_info$vendor==input$uip_vendor]
    # get the currency
    currrency_code <- import_price$currency_code[
      import_price$vendor_id==sel_vendor_id&
        import_price$prod_code==sel_prod_code][1]
    # build the list based on data
    if (!is.na(currrency_code)){
      box_choices <- currency$currency[currency$currency_code==currrency_code]
    }else{
      box_choices <- currency$currency
    }
    box_selected <- box_choices[1]
    box_label = get_actual('currency')
  }
  
  selectizeInput(inputId = iid,
                 label = box_label,
                 choices = box_choices, 
                 selected = box_selected,
                 options = list(create=allow_add))
})}