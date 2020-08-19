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