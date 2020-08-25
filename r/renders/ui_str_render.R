# these  are functions that render the strings in UI for information only

# render the information string when adding packaging
# in the update_prod tab
render_add_pkg_str <- function(input){renderUI({
  ordering_unit <- get_ordering_unit(packaging)
  cur_prod_code <- product_info$prod_code[
    product_info$search_str == input$add_pkg_prod_name]
  cur_order_unit <- ordering_unit$unit[ordering_unit$prod_code==cur_prod_code]
  output <- paste0(
    ui_elem$actual[ui_elem$label=='add_pkg'],'  ',input$add_unitspp,
    input$add_pkg_unit,'/',cur_order_unit,' ',
    ui_elem$actual[ui_elem$label=='for_prod'],' ', input$add_pkg_prod_name)
  HTML(output)
}) }

# render product info default to inv_out but things can change
render_prod_info <- function(input,type='inv_out'){renderUI({
  inventory <- update_inventory(config_dict)
  if (type=='inv_out'){
    current_select <- product_info[
      product_info$search_str==input$prod_name_select,]
  }
  total_available <-inventory[
    inventory$prod_code == current_select$prod_code &
      inventory$lot == input$lot_select,
    'remaining_qty']
  
  # also get all other available lot
  alllot_available <- inventory[
    inventory$prod_code == current_select$prod_code &
      inventory$lot != input$lot_select,]
  alllot_available$exp_date[is.na(alllot_available$exp_date)] <- 'nodate'
  
  if(length(total_available)==0){total_available <- 0}
  current_exp_date <- inventory[
    inventory$prod_code == current_select$prod_code &
      inventory$lot == input$lot_select, 'exp_date']
  packaging_str <- packaging[
    packaging$prod_code == current_select$prod_code &
      packaging$unit == input$unit_selector,]
  ordering_unit <- get_ordering_unit(packaging)
  current_order_unit <- ordering_unit$unit[
    ordering_unit$prod_code==current_select$prod_code]
  current_selected_unit <- packaging[
    packaging$unit==input$unit_selector & 
      packaging$prod_code==current_select$prod_code,]
  packaging_str <- paste0(packaging_str$units_per_pack[1],
                          packaging_str$unit[1],'/',current_order_unit)
  alllot_str <- ''
  alllot_available <- merge(alllot_available,ordering_unit,all.x=T)
  if (nrow(alllot_available)>0){
    for(i in 1:nrow(alllot_available)){
      alllot_str <- paste0(
        alllot_str,alllot_available$lot[i],' - ',alllot_available$exp_date[i],
        ' - ',alllot_available$remaining_qty[i],'',alllot_available$unit[i],
        '<br/>')
    }
  }
  # print(alllot_available)
  # print(current_select$prod_code)
  product_info_str <- paste(
    "Information:", '<br/>',
    ui_elem$actual[ui_elem$label=='prod_code'],':',
    current_select$prod_code, '<br/>',
    ui_elem$actual[ui_elem$label=='vendor'],':',
    current_select$vendor,'<br/>',
    "REF: ",current_select$ref_smn,'<br/>',
    ui_elem$actual[ui_elem$label=='exp_date'],':',
    current_exp_date, '<br/>',
    ui_elem$actual[ui_elem$label=='total_available'],':',
    round(total_available*current_selected_unit$units_per_pack,digits=0),
    current_selected_unit$unit,'(',
    round(total_available,digits=1), current_order_unit, ')<br/>',
    ui_elem$actual[ui_elem$label=='packaging_str'],
    ':',packaging_str, '<br/>',get_actual('other_lot'),':<br/>',alllot_str)
  HTML(product_info_str)
}) }



