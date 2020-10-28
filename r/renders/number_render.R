# render all ui that use a number as input

render_min_order <- function(input,iid,allow_add=T){renderUI({
  # calculate the choice based on iid
  if (iid == 'uip_min_order'){
    current_prod_code <- product_info$prod_code[
      product_info$search_str==input$uip_prod_name]
    current_vendor_id <- vendor_info$vendor_id[
      vendor_info$vendor == input$uip_vendor]
    box_choice <- import_price$min_order[
      import_price$prod_code==current_prod_code&
        import_price$vendor_id==current_vendor_id]
    print(box_choice)
    if (length(box_choice)==0){
      box_choice <- 1
    }

    box_selected <- box_choice[1]
    
    box_label <- get_actual('min_qty')
  }
  
  selectizeInput(
    inputId = iid, label = box_label,
    choices = box_choice, 
    selected = box_selected, 
    options = list(create = allow_add))
})}