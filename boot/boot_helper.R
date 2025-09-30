# --------------------- config and uielem --------------------------------

# create config_dict and build the paths inside, require app_path
# if the db_config flag is true, it will read config from db, merge with local,
# and prioritise local config in case of duplicates
load_local_config <- function(local_config_path){

  if (file.exists(local_config_path)){
    config_dict <- read.table(local_config_path, sep = "\t", header = T,
                              stringsAsFactors = F)
  }else{
    stop(paste('config file not found in',local_config_path))
  }
  
  config_dict$source_rank <- 1 # local=1;db=2
  
  # add comment column if not yet in config_dict, as db dict have comment
  if (!('comment' %in% names(config_dict))){
    config_dict$comment <- ''
  }
  
  return(config_dict)
}

build_config_path <- function(config_dict){
  
  # build paths in config_dict
  config_dict$value[config_dict$type=='relative'] <-
    file.path(app_path,config_dict$value[config_dict$type=='relative'])
  
  # fix windows styling
  config_dict$value[config_dict$type=='relative'|config_dict$type=='abs'] <-
    gsub('\\\\','/',config_dict$value[
      config_dict$type=='relative'|config_dict$type=='abs'])
  
  return(config_dict)
}

load_db_config <- function(local_config){
  admin_id <- as.integer(
    local_config$value[local_config$name=='admin_id'])
  
  db_config <- dbGetQuery(conn,paste0('select * from config where admin_id=',
                                      admin_id,' or admin_id=0'))
  
  # if there is duplicated items in db_config, use the one with admin_id
  db_config <- db_config %>% arrange(desc(admin_id))
  db_config <- db_config[!duplicated(db_config$name),]
  
  #remove admin_id and finalise
  db_config$admin_id <- NULL
  db_config$source_rank <- 2
  # db_config <- build_config_dict_path(db_config)
  
  return(db_config)
}

#create a wide format config and format the data type
create_config <- function(local_config,db_config){
  
  # bind the two config, sort by source_rank, then remove duplicates
  config_dict <- rbind(local_config,db_config)
  config_dict <- config_dict[order(config_dict$source_rank),]
  config_dict <- config_dict[!duplicated(config_dict$name),]
  
  #build the path
  config_dict <- build_config_path(config_dict)
  
  # convert to wide format
  config <- config_dict %>% select(name,value)
  config <- spread(config,name,value)
  
  # convert boolean variable
  var_list <- config_dict$name[config_dict$type=='boolean']
  if(length(var_list)>0){
    for (var_name in var_list){
      config[[var_name]] <- (config[[var_name]]==1|
                               grepl("T",config[[var_name]]))
    }
  }
  
  return(config)
}

# create ui_elem
create_uielem <- function(config){
  
  query <- paste0(
    "select * from uielem where app_lang='",config$app_lang,"'")
  uielem <- dbGetQuery(conn,query)
  
  uielem <- spread(uielem %>% select(label,actual),label,actual)
  
  return(uielem)
}

# 
# split a long string separated by ';' to recover the list of strings
split_semi <- function(input_str){
  
  if(grepl(';',input_str)){
  output_str <- unlist(strsplit(input_str,';'))
  }else{
    output_str <- input_str
    }
  
  return(output_str)
}
