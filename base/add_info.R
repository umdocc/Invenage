# functions to add info to raw data by using database
add_product_info <- function(input_df,add_col=c("comm_name","ref_smn")){
  
  # check data frame for consistency
  if(any(is.na(input_df$prod_code))){
    stop("data frame contains null product code")
  }
  
  filter_col <- c("prod_code", add_col)
  input_df <- merge(input_df, product_info[,filter_col], all.x=T)
  
  return(input_df)
}

add_customer_info <- function(input_df,add_col=c("customer_name")){
  
  # check data frame for consistency
  if(any(is.na(input_df$customer_id))){
    stop("data frame contains null customer id")
  }
  
  filter_col <- c("customer_id", add_col)
  input_df <- merge(input_df, customer_info[,filter_col], all.x=T)
  
  return(input_df)
}