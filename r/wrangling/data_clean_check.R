# clean data frame from common mistakes, defensive programming

# generic clean for all data_df in invenage
clean_data <- function(data_df){
  if('unit' %in% names(data_df)){
    data_df[['unit']] <- tolower(data_df[['unit']])
  }
  return(data_df)
}

# check data_df for common mistake
check_data <- function(data_df,error_display='shinyalert'){
  error_label <- ''
  data_df <- clean_data(data_df)
  # check unit column for consistency
  if('unit' %in% names(data_df)){
    tmp <- data_df[is.na(data_df[['unit']])|data_df[['unit']]=='',]
    if (nrow(tmp)>0){
      error_label <- 'invalid_unit'
      if(error_display=='shinyalert'){
        show_alert('input_error','invalid_unit','error')
      }
    }
  }
  return(error_label)
}

# check before writing new row to database
dup_check <- function(append_df,tbl_name){
  error_label <- ''
  col_to_check <- unlist(
    strsplit(
      config_dict$value[config_dict$name == paste0('dup_check_',tbl_name)],
      split = ';'))
  if(is.null(col_to_check)){
    stop('col_to_check is null, check config_dict')
  }
  db_data <- get(tbl_name)
  append_df <- check_exist(append_df,db_data,col_to_check)
  if (any(append_df$exist)){
    error_label <- 'previously_entered'
    show_alert('error',error_label,'error')
  }
  return(error_label)
}