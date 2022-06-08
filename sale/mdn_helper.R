mdn_load_ui <- function(input,output,ui_list){
  if ('mdn_pxk_num' %in% ui_list){
    output$mdn_pxk_num <- render_mdn_pxk_num(input)
  }
  return(output)
}

render_mdn_pxk_num <- function(input){renderUI({
  pxk_list <- unique(sale_log$pxk_num)
  selectizeInput(
    inputId = "mdn_pxk_num", label = uielem$pxk_num,
    choices = pxk_list, selected = pxk_list[1],
    options = list(create = F))
})
}
