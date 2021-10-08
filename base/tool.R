# function to replace a pattern, default replacement to "", use with mapply
rep_vector <- function(pattern_col,target_col,replacement=""){
  gsub(pattern_col,replacement,target_col)
}

# check if a list of columns exist on a data frame
col_exist <- function(data_frame,col_list){
  col_exist <- F
  
  return(colname_not_exist)
}

# function to add import license data to a data frame of products
add_import_lic_info <- function(product_data, import_lic_data){
  required_col <- c("ref_smn","vendor_id")
}

# translate column
translate_tbl_column <- function(input_df,ui_elem=uielem){
  for (i in 1:length(input_df)){
    if (names(input_df)[i] %in% names(ui_elem)){
      names(input_df)[i] = ui_elem[names(input_df)[i]]
    }
  }
  return(input_df)
}