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
  req(input$sep_po_file)
  po_filepath <- input$sep_po_file$datapath
  po_data <- sep_read_po_data(po_filepath)
  
  
  if(error_free){

    po_data <- merge(po_data,ordering_unit, all.x=T)
    
    if(any(is.na(po_data$unit))){
      gbl_write_var("error_free", F)
      show_error("unit_notfound",
                 po_data$prod_code[is.na(po_data$unit)])
    }
  }
  po_name <-unique(po_data$po_name) # compatibility
  append_log <- sep_check_db_exist(po_name, po_data)
  # print(append_log)
  if(nrow(append_log)>0){
    
    # add data and append to db
    append_log$delivery_date <- Sys.Date()
    append_log$vendor_id <- get_vid_from_po_name(po_name)
    keep_col <- c("prod_code", "unit", "qty", "lot", "po_name", "exp_date",
                  "delivery_date", "vendor_id", "note")
    append_log <- append_log[,keep_col]
    if(error_free){
      db_append_tbl("import_log",append_log)
      
      # reload other UIs
      gbl_load_tbl("import_log")
      gbl_update_inventory()
      show_success("add_success")
    }
    
  }
}

# read, check and clean po_data
sep_read_po_data <- function(po_filepath){

  # read in data and check required columns
  po_data <- read.xlsx(po_filepath,skipEmptyRows = F)
  name_pos <- which(po_data[,1]=="Name")
  po_name <- po_data[name_pos, 2]
  start_pos <- which(po_data[,4]=="REF")
  po_data <- read.xlsx(po_filepath, startRow = start_pos)
  required_cols <- unlist(strsplit(config$po_data_excel_colnames,";"))
  check_required_col(required_cols, po_data)

  # proceed if previous check not return error_free
  if(error_free){

    # keep only required col and clean up
    po_data <- po_data[,required_cols]
    names(po_data) <- unlist(strsplit(config$po_data_colnames,";"))
    po_data <- sep_clean_po_data(po_data)

    # add prod_code and check for invalid product
    po_data <- sep_add_po_prod_code(po_data)
    
    # add po_name
    if(grepl(".PO.",po_name)){
    po_data$po_name <- po_name
    }else{
      show_error(type = "po_name_notfound","")
    }
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
  for (i in 1:nrow(exw_vendor_list)){
    if(grepl(exw_vendor_list$name_on_po[i],po_name)){
      vendor_id <- exw_vendor_list$vendor_id[i]
    }
  }
  # if we cannot find anything set error_free to F
  if (is.na(vendor_id)){
    gbl_write_var("error_free",F)
  }
  return(vendor_id)

}

# 
# sep_update_unit_cost <- function(input,output){
#   po_name <- input$sep_po_name
#   sep_file_upload <- input$sep_file
#   
#   unit_cost_data <- read.xlsx(sep_file_upload$datapath)
#   print(sep_file_upload$datapath)
#   print(unit_cost_data)
#   # will need to check file format and columns etc later
#   unit_cost_data <- unit_cost_data %>% select(ref_smn,actual_unit_cost)
#   
#   # write the unit cost to both the po and the database
#   # write_po_unit_cost(po_name, unit_cost_data)
#   
#   return(output)
# }
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

