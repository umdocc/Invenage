sep_load_ui <- function(input,output,ui_list){
  if ('sep_po_name' %in% ui_list){
    output$sep_po_name <- render_sep_po_name(input)
  }
  if ('sep_po_data' %in% ui_list){
    output$sep_po_data <- render_sep_po_data(input)
  }
  return(output)
}

sep_add_po2db <- function(input,output){
  # reset error free before proceeding
  gbl_write_var("error_free",T)
  req(input$sep_po_file)
  po_filepath <- input$sep_po_file$datapath
  
  # read metadata of po
  po_meta <- sep_read_po_meta(po_filepath)
  po_data <- sep_read_po_data(po_filepath, po_meta)
  
  gbl_write_var("po_data",po_data)
  gbl_write_var("po_meta",po_meta)
  
  
  if(error_free){

    po_data <- merge(po_data,ordering_unit, all.x=T)
    
    if(any(is.na(po_data$unit))){
      gbl_write_var("error_free", F)
      show_error("unit_notfound",
                 po_data$prod_code[is.na(po_data$unit)])
    }
  }
  
  append_log <- sep_check_db_exist(po_meta$po_name, po_data)
  
  if(nrow(append_log)>0){
    # add po_name to write to db
    print("adding items from PO to database")
    print(append_log)
    append_log$po_name <- po_meta$po_name
    
    
    # add data and append to db
    append_log$delivery_date <- Sys.Date()
    append_log$vendor_id <- get_vid_from_po_name(po_meta$po_name)
    keep_col <- c("prod_code", "unit", "qty", "lot", "po_name", "exp_date",
                  "delivery_date", "vendor_id", "note")
    append_log <- append_log[,keep_col]
    if(error_free){
      db_append_tbl("import_log",append_log)
      
      # reload data and other UIs
      gbl_load_tbl("import_log")
      gbl_update_inventory()
      show_success("add_success")
    }else{
      show_error("data_exist")
    }
    
  }else{
    show_error("data_exist")
  }
}

# read, check and clean po_data
sep_read_po_data <- function(po_filepath, po_meta){
  
  if(error_free){
    po_data <- read.xlsx(po_filepath, startRow = po_meta$start_row)
    required_cols <- unlist(strsplit(config$po_data_excel_colnames,";"))
    check_required_col(required_cols, po_data)
  }
  
  # proceed if previous check not return error_free
  if(error_free){

    # keep only required col and clean up
    po_data <- po_data[,required_cols]
    names(po_data) <- unlist(strsplit(config$po_data_colnames,";"))
    po_data <- sep_clean_po_data(po_data)

    # add prod_code and check for invalid product
    po_data <- sep_add_po_prod_code(po_data)
    
  }
  return(po_data)
}

sep_add_po_prod_code <- function(po_data){
  po_data <- merge(po_data,product_info %>%
                     select(ref_smn,vendor_id,prod_code),
                   all.x=T)
  
  # check the po for invalid prod_code and notify the user
  invalid_ref <- po_data$ref_smn[is.na(po_data$prod_code)]
  if(length(invalid_ref)>0){
    show_error("ref_notfound",invalid_ref)
    gbl_write_var("error_free",F)
  }
  
  return(po_data)
}
# 
sep_clean_po_data <- function(po_data){
  
  po_data$ref_smn <- trimws(po_data$ref_smn) #trim ref_smn ws
  po_data <- po_data[!is.na(po_data$ref_smn),] # remove rows with empty ref
  po_data <- po_data[po_data$qty >0,] # keep only rows with qty > 0
  #keep only rows with valid lot
  po_data <- po_data[(!is.na(po_data$lot)) & (po_data$lot!=''),]
  # remove the "'" character in lot/date
  po_data$lot <- gsub("'","",po_data$lot)
  po_data$exp_date <- gsub("'","",po_data$exp_date)
  
  return(po_data)
}

sep_check_db_exist <- function(po_name, po_data){

  db_data <- db_read_query(paste0(
    "select prod_code, qty, lot from import_log where po_name='",
    po_name,"'")) %>% mutate(in_db=T)
  po_data <- merge(po_data,db_data,all.x=T)
  append_log <- po_data[is.na(po_data$in_db),]
  append_log$in_db <- NULL

  return(append_log)
}

get_vid_from_po_name <- function(po_name){
  vendor_id <- NA
  exw_vendor_list <- vendor_info[!is.na(vendor_info$name_on_po),]
  
  # make the search case insensitive
  exw_vendor_list$name_on_po <- tolower(exw_vendor_list$name_on_po)
  po_name <- tolower(po_name)
  
  for (i in 1:nrow(exw_vendor_list)){
    if(grepl(exw_vendor_list$name_on_po[i],po_name)){
      vendor_id <- exw_vendor_list$vendor_id[i]
    }
  }
  # if we cannot find anything set error_free to F
  if (is.na(vendor_id)){
    show_error("vendorid_notfound")
    gbl_write_var("error_free",F)
  }
  return(vendor_id)

}

# check po file for including all required data
sep_read_po_meta <- function(po_filepath){
  
  po_data <- read.xlsx(po_filepath,skipEmptyRows = F, colNames = F)
  po_name_coord <- as.numeric(split_semi(config$po_name_coord))
  po_meta <- data.frame(po_name=po_data[po_name_coord[1], po_name_coord[2]])
  
  po_meta$start_row <- as.numeric(split_semi(config$po_data_start_coord))[1]
  
  # check if po name was read correctly
  if(!grepl(".PO.",po_meta$po_name)){
    show_error("po_name_notfound")
  }
  
  return(po_meta)
}

# 
sep_update_unit_cost <- function(input, keep_old_data=T){
  
  req(input$sep_po_file)
  po_filepath <- input$sep_po_file$datapath
  
  po_meta <- sep_read_po_meta(po_filepath)
  po_data <- sep_read_po_data(po_filepath, po_meta)

  # get the unit cost to write
  unit_cost_data <- po_data %>% filter(!is.na(actual_unit_cost))
  unit_cost_data <- merge(
    unit_cost_data, product_info %>% select(prod_code, ref_smn)) %>%
    select(prod_code, actual_unit_cost, lot, qty)
  
  # retrieve entries from database that match po_name
  # if overwrite_old is TRUE, then we dont need to filter na
  db_data <- import_log[import_log$po_name==po_meta$po_name,]
  if(keep_old_data){
    db_data <- db_data %>% filter(is.na(actual_unit_cost))
    }
  db_data <- db_data %>% select(prod_code,lot,qty)
  
  
  append_unit_cost <- merge(unit_cost_data, db_data)
  
  #construct queries
  for (i in 1:nrow(append_unit_cost)){
    query <- paste0("update import_log set actual_unit_cost=",
                    append_unit_cost$actual_unit_cost[i]," where prod_code='",
                    append_unit_cost$prod_code[i],"' and lot='",
                    append_unit_cost$lot[i],"' and qty=",
                    append_unit_cost$qty[i]," and po_name='",
                    po_meta$po_name,"'")
    db_exec_query(query)
  }
  # reload data and other UIs
  gbl_load_tbl("import_log")
  gbl_update_inventory()
  show_success("add_success")
}
# 
# write_po_unit_cost <- function(po_name, unit_cost_data=NULL, 
#                                write_mode="full"
#                            ){
#   
#   po_data <- db_read_query(
#     paste0("select * from import_log where po_name='",po_name,"'"))
#   po_data <- merge(po_data, product_info %>% select(prod_code,ref_smn))
#   
#   if(write_mode=="missing"){
#     po_data <- po_data[is.na(po_data$actual_unit_cost),]
#   }
#   po_data$actual_unit_cost <- NULL
#   
#   po_data <- merge(po_data,unit_cost_data %>% select(ref_smn,actual_unit_cost),
#                          all.x=T)
#   po_data <- po_data[!is.na(po_data$actual_unit_cost),]
# }

