
render_vendor_list <- function(input, iid, ui_label, allow_add = T, tab='update_prod'){
  renderUI({
    
    if (tab=='update_prod'){
      current_vendor <- product_info$vendor[
        product_info$prod_code == input$add_prod_code]
      if (length(current_vendor)==0){
        current_vendor <- vendor_info$vendor
      }
      selected_vendor <- current_vendor[1]
    }
    if (tab=='invoice_update'){
      current_vendor <- vendor_info$vendor[vendor_info$local==0]
      selected_vendor <- current_vendor[1]
    }
    if (tab=='inv_in'){
      current_vendor <- vendor_info$vendor
      current_prod_code <- product_info$prod_code[
        product_info$search_str == input$in_prodname_select]
      last_vendor_id <- import_log[
        import_log$prod_code==current_prod_code,]
      sel_vendor_id <- last_vendor_id$vendor_id[
        last_vendor_id$delivery_date==max(last_vendor_id$delivery_date)]
      sel_vendor_id <- sel_vendor_id[!is.na(sel_vendor_id)]
      if (length(sel_vendor_id)==0){
        sel_vendor_id <- 1
      }
      selected_vendor <- vendor_info$vendor[vendor_info$vendor_id==sel_vendor_id]
    }
    if (tab=='update_import_price'){
      sel_prod_code <- product_info$prod_code[
        product_info$search_str==input$uip_prod_name]
      avai_vendor_id <- import_price[
        import_price$prod_code==sel_prod_code,]
      # if filter yield result, use it, otherwise default to full list
      if (nrow(avai_vendor_id)>0){
        current_vendor <- merge(
          avai_vendor_id,vendor_info %>% select(vendor_id,vendor),all.x=T)
        current_vendor <- current_vendor$vendor
      }else{
        current_vendor <- vendor_info$vendor
      }
      selected_vendor <- current_vendor[1]
    }
    
    selectizeInput(
      inputId = iid, label = ui_label,
      choices = current_vendor, 
      selected = selected_vendor, 
      options = list(create = allow_add))
  }) 
}
