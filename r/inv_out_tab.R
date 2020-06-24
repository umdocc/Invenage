render_current_pxk_infostr <- function(config_dict){renderUI({
  pxk_num <- get_current_pxk(config_dict) # get the current pxk_num
  current_pxk_str <- get_pxk_info_str(pxk_num)
  HTML(current_pxk_str)
}) }

# render table for the invout tab
render_invout_pxktable <- function(){DT::renderDataTable({
  current_pxk <- get_current_pxk(config_dict)
  output <- render_selected_pxk(current_pxk,config_dict)
  
  DT::datatable(output, options = list(pageLength = 10),rownames=F)
})
}