# various functions to check data requirements
check_required_col <- function(data_df,required_col){
  for (col_name in required_col){
    if(!(col_name %in% names(data_df))){
      stop(paste(col_name, 'column not found'))
    }
  }
}