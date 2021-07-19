db_open <- function(config){
  conn <- dbConnect(
    drv = RMariaDB::MariaDB(),
    username = config$sql_usr,
    password = config$sql_pswd,
    host = config$sql_host,
    port = 3306, dbname = config$sql_db_name)
  
  return(conn)
}

db_read_query <- function(query){
  conn <- db_open(config)
  data_out <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  
  return(data_out)
}