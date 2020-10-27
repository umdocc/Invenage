

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
get_local_po_list <-  function(){
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