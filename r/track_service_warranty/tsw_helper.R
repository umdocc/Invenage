# helper provide additional resources and non-render function for the tab
# -------------------------------- Resources -----------------------------------
# load additional tables
reload_tbl(config_dict,c('tsl_analyser_list','tsl_service_type'))


# -------------------------------- UI Loader -----------------------------------
# function to reload additional ui
tsw_load_ui <- function(input,output,ui_list){
  if ("tsw_product_name" %in% ui_list){
    output$tsw_product_name <- render_product_list('tsw_product_name')
  }
  return(output)
}

