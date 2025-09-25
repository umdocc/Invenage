# ---------------- open/read/exec queries --------------------
db_open <- function(db_name = config$sql_db_name){
  conn <- dbConnect(
    drv = RMariaDB::MariaDB(),
    username = config$sql_usr,
    password = config$sql_pswd,
    host = config$sql_host,
    port = 3306, dbname = db_name)
  
  return(conn)
}

# return result from a query, require db_open
db_read_query <- function(query){
  conn <- db_open()
  data_out <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  
  return(data_out)
}

# execute a query, require db_open
db_exec_query <- function(query){
  conn <- db_open()
  dbExecute(conn, query)
  dbDisconnect(conn)
}

db_append_tbl <- function(tbl_name,data_df){
  conn <- db_open()
  dbWriteTable(conn,tbl_name,data_df, append=T, overwrite=F)
  dbDisconnect(conn)
}

# ------------- read tables ---------------
# read the product list, but return a search_str for display
# item_list can be customised
db_get_prodlist <- function(
  search_str_structure){
  product_list <- product_info
  
  #clean up
  product_list$prod_search_str <- paste(
    product_list$ref_smn, product_list$comm_name, product_list$packaging_str)
  product_list$prod_search_str <- trimws(product_list$prod_search_str)
  product_list <- product_list %>% select(prod_search_str,prod_code)
  
  return(product_list)
}

# load table with some customisation
gbl_load_tbl <- function(table_list=c("uielem")){
  
  conn <- db_open()
  
  for (tbl_name in table_list){
    # if complex table process separately, otherwise load entire tbl
    if(tbl_name %in% c("sale_log", "payment_type", "product_type", "price_list")){
      
      if(tbl_name=="sale_log"){
        data_tbl <- dbGetQuery(conn,"select * from sale_log inner join pxk_info
                           using (pxk_num)")
        assign("sale_log",data_tbl,envir=globalenv())
      }
      
      if(tbl_name=="price_list"){
        data_tbl <- dbGetQuery(conn,"select * from price_list where active=1")
        assign("price_list",data_tbl,envir=globalenv())
      }
      
      if(tbl_name %in% c("payment_type", "product_type")){
        data_tbl <- dbGetQuery(conn,
        paste0("select * from ",tbl_name," inner join uielem
                           on ",tbl_name,".label = uielem.label"))
        assign(tbl_name,data_tbl,envir=globalenv())
      }
      
    }else{
    data_tbl <- dbGetQuery(conn,paste("select * from",tbl_name))
    assign(tbl_name,data_tbl,envir=globalenv())
    }
  }

  dbDisconnect(conn)
  
  }