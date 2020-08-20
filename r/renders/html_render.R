# renders for html text

render_currency <- function(
  input,iid,tab='update_import_price',allow_add=F){renderUI({
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
    currency <- currency$currency[currency$currency_code==currrency_code]
    HTML(paste('<br/>',paste(currency,collapse = ' ')))
  })
}