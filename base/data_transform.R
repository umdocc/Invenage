# the convert_config function will do 2 things: convert config_dict from long 
# to wide format and convert characters into correct format 
# defined in the 'type' column
convert_config <- function(config_dict){
  
  # convert to wide format
  config <- config_dict %>% select(name,value)
  config <- spread(config,name,value)
  
  # convert int variable
  var_list <- config_dict$name[config_dict$type=='int']
  if(length(var_list)>0){
    for (var_name in var_list){
      config[[var_name]] <- as.integer(config[[var_name]])
    }
  }
  
  return(config)
}