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
db_load_tbl <- function(table_list=c("packaging","product_info")){
  conn <- db_open()
  if("packaging" %in% table_list){
    data_tbl <- dbGetQuery(conn,"select * from packaging")
    assign("packaging",data_tbl,envir=globalenv())
  }
  if("product_info" %in% table_list){
    data_tbl <- dbGetQuery(conn,"select * from product_info")
    assign("product_info",data_tbl,envir=globalenv())
  }
  if("import_log" %in% table_list){
    data_tbl <- dbGetQuery(conn,"select * from import_log")
    assign("import_log",data_tbl,envir=globalenv())
  }
  
  dbDisconnect(conn)
}