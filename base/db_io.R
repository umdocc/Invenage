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

# ------------- read tables ---------------
# read the product list, but return a search_str for display
# item_list can be customised
db_get_prodlist <- function(
  search_str_structure){
  product_list <- db_read_query(paste0(
    "select concat(",search_str_structure,") as prod_search_str, prod_code
    from product_info"))
  
  #clean up
  product_list$prod_search_str <- trimws(product_list$prod_search_str)
  return(product_list)
}

# function to load raw db table into
db_load_simple_tbl <- function(table_list=c("packaging","product_info")){
  conn <- db_open()

  for (tbl_name in table_list){
    data_tbl <- dbGetQuery(conn,paste("select * from",tbl_name))
    assign(tbl_name,data_tbl,envir=globalenv())
  }
  
  dbDisconnect(conn)
}

db_load_complex_tbl <- function(table_list=c("sale_log")){
  conn <- db_open()
  if("sale_log" %in% table_list){
    data_tbl <- dbGetQuery(conn,"select * from sale_log inner join pxk_info
                           on sale_log.pxk_num = pxk_info.pxk_num")
    assign("sale_log",data_tbl,envir=globalenv())
    
  }
  dbDisconnect(conn)
}