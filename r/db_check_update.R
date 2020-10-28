# function to update po_info
update_po_info <- function(config_dict){
  
  result <- add_po_to_db() # add new po to db, result is logical for error_free
  
  # if(result){
  #   # get a list of incomplete po and check their data
  #   incomplete_po <- po_info$po_name[po_info$completed==0]
  #   for (po_name in incomplete_po){
  #     # print(po_name)
  #     mark_po_complete(po_name)
  #   }
  # }
}

add_po_to_db <- function(){
  error_free <- T
  # compare with remote database
  local_po <- get_local_po_list(config_dict)
  if(is.data.frame(local_po)){
    remote_po <- po_info
    
    # update with new local_po
    new_po <- check_exist(local_po,remote_po,'po_name')
    new_po <- new_po[!new_po$exist,]
    # write new data to database
    if (nrow(new_po)>0){
      new_po$completed <- 0
      new_po$finalised <- 0
      new_po$note <- ''
      new_po$exist <- NULL
      append_po <- new_po %>% select(po_name,completed,finalised,note)
      append_tbl_rld(config_dict,'po_info',append_po)
    }
  }else{
    warning("path to local po not found! nothing will be updated")
    error_free <- F
  }
  return(error_free)
}

# check a po for full data then mark it as completed
mark_po_complete <- function(po_name){
  po_data <- read_po_data(po_name)
  #filter on cancelled items
  po_data <- po_data[!is.na(po_data$qty) & po_data$num_qty>0,]
  if (nrow(po_data)>0){
    db_data <- db_read_query(
      paste0("select * from import_log where po_name='",po_name,"'"))
    po_data <- check_exist(
      po_data,db_data,c("prod_code","qty","lot"))
    # all data in po exist in db?
    all_db <- all(po_data$exist)
    #all lot completed?
    lot_complete <- all(!(db_data$lot==''|is.na(db_data$lot)))
    # all unit cost completed?
    unit_cost_complete <- all(!is.na(as.numeric(db_data$actual_unit_cost)))
    if(lot_complete & unit_cost_complete & all_db){
      query <- paste0(
        "update po_info set completed = 1 where po_name='",po_name,"'")
      db_exec_query(query)
    }
  }
}

# create list of local po
get_local_po_list <-  function(config_dict){
  po_path <- config_dict$value[config_dict$name=='po_path']
  po_search_str <- config_dict$value[config_dict$name=='po_file_include']
  
  # R regex fix, for scanning PO
  po_search_str <- gsub('\\.','\\\\.',po_search_str)
  po_list <- get_file_info(po_path)
  if(is.data.frame(po_list)){
    po_list <- po_list[grepl(po_search_str,po_list$file_name),]
    
    # remove locked excel files, pdf files
    po_list <- po_list[!grepl('\\$',po_list$file_name),]
    po_list <- po_list[!grepl('pdf',po_list$file_name),]
    po_list$po_name <- gsub('\\.xlsx|\\.xls','',po_list$file_name)
  }
  return(po_list)
}