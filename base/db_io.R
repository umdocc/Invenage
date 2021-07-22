# open the connection
db_open <- function(config){
  conn <- dbConnect(
    drv = RMariaDB::MariaDB(),
    username = config$sql_usr,
    password = config$sql_pswd,
    host = config$sql_host,
    port = 3306, dbname = config$sql_db_name)
  
  return(conn)
}

# return result from a query, require db_open
db_read_query <- function(query){
  conn <- db_open(config)
  data_out <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  
  return(data_out)
}

# execute a quqery, require db_open
db_exec_query <- function(query){
  conn <- db_open(config)
  dbExecQuery(conn,query)
  dbDisconnect(conn)
}

# read the product list, but return a search_str for display
# item_list can be customised
db_get_prodlist <- function(
  search_str_structure){
  product_list <- db_read_query(paste0(
    "select concat(",search_str_structure,") as prod_search_str, prod_code
    from product_info"))
  return(product_list)
}