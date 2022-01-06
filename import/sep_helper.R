sep_load_ui <- function(input,output,ui_list){
  if ('sep_po_name' %in% ui_list){
    output$sep_po_name <- render_sep_po_name(input)
  }
  if ('sep_po_data' %in% ui_list){
    output$sep_po_data <- render_sep_po_data(input)
  }
  return(output)
}

sep_get_local_po_list <- function(po_path){
  po_filelist <- data.frame(
    full_path = list.files(path = po_path, full.names = T, recursive = T))
  if(nrow(po_filelist)>0){
    po_filelist$po_name <- basename(po_filelist$full_path)
    po_filelist <- po_filelist[
      grepl(config$po_id_string,po_filelist$po_name) & 
        grepl(config$po_file_format,po_filelist$po_name),]
    po_filelist$po_name <- gsub(config$po_file_format, "", po_filelist$po_name)
  }else{
    po_filelist <- setNames(
      data.frame(matrix(ncol = 3, nrow = 0)), c("full_path", "po_name"))
  }
  return(po_filelist)
}

sep_sync_po2db <- function(input){
  
  # collect variables
  po_name <- input$sep_po_name
  out_msg <- '' #init the output message
  po_data_coord <-
    as.numeric(unlist(strsplit(config$po_data_start_coord,split=";")))
  po_data_colnames <- unlist(strsplit(config$po_data_colnames,split=";"))
  
  # read the po data
  full_path <- local_po_list$full_path[local_po_list$po_name==po_name]
  po_data <- sep_read_po_data(full_path)
  
  if(!is.logical(po_data)){
    
    po_data$po_name <- input$sep_po_name
    # add unit
    ordering_unit <- get_ordering_unit(packaging)
    ordering_unit <- ordering_unit[!duplicated(ordering_unit$prod_code),]
    po_data <- merge(po_data,ordering_unit, all.x=T)
    
    # create list of rows to append to db and write
    append_log <- sep_check_db_exist(po_data)
    if(nrow(append_log)>0){
      db_append_tbl("import_log",append_log)
    }
  }
}



# read, check and clean po_data
sep_read_po_data <- function(full_path){
  
  # read in data and check required columns
  po_data <- read.xlsx(full_path,startRow = po_data_coord[1])
  po_data <- sep_check_required_cols(po_data)
  
  # proceed if previous check not return a logical (FALSE) value
  if(!is.logical(po_data)){
    
    # keep only required col and clean up
    po_data <- po_data[,required_cols]
    names(po_data) <- unlist(strsplit(config$po_data_colnames,";"))
    po_data <- sep_clean_po_data(po_data)
    
    # add prod_code and check for invalid product
    po_data <- sep_add_po_prod_code(po_data)
    
    return(po_data)
  }
}

sep_check_required_cols <- function(po_data){
  
  # check the data for required columns, show error with missing columns
  error_free <- T
  required_cols <- unlist(strsplit(config$po_data_excel_colnames,split=";"))
  for (col_name in required_cols){
    if(!(col_name %in% names(po_data))&error_free){
      alert_col_notfound(col_name)
      error_free <- F
    }
  }
  
  if(error_free){
    return(po_data)
  }else{
    return(FALSE)
  }
}

sep_add_po_prod_code <- function(po_data){
  error_free <- T
  po_data <- merge(po_data,product_info %>%
                     select(ref_smn,vendor_id,prod_code),
                   all.x=T)
  
  # check the po for invalid prod_code and notify the user
  invalid_ref <- po_data$ref_smn[is.na(po_data$prod_code),]
  if(length(invalid_ref)>0){
    alert_ref_notfound(invalid_ref)
    errorr_free <- F
  }
  if(error_free){
    return(po_data)
  }else{
    return(FALSE)
  }
}

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

sep_check_db_exist <- function(po_data,po_name){
  
  db_data <- db_read_query(paste0(
    "select prod_code, qty, lot from import_log where po_name='",
    po_name,"'")) %>% mutate(in_db=T)
  po_data <- merge(po_data,db_data,all.x=T)
  append_log <- po_data[is.na(po_data$in_db),]
  
  return(append_log)
}

#       # writing to database
#       if (nrow(po_data)>0){
#         print('writing to import_log'); print(po_data)
#         db_write(config_dict,'import_log',po_data)
#         out_msg <- paste0(
#           out_msg, '\n',ui_elem$actual[ui_elem$label=='add_lotdate_success'])
#       }
#       
#       #update price
#       # keep only rows with price to prevent writing NA in database
#       po_price <- po_price[!is.na(po_price$actual_unit_cost),]
#       if (nrow(po_price)>0){
#         print('updating price(s)'); print(po_price)
#         conn <- db_open(config_dict)
#         for (i in 1:nrow(po_price)){
#           query <- paste0('update import_log set actual_unit_cost = ',
#                           po_price$actual_unit_cost[i],
#                           ' where po_name like ','"',po_price$po_name[i],'"',
#                           ' AND qty = ',po_price$qty[i], 
#                           ' AND lot like ','"',po_price$lot[i],'"',
#                           ' AND prod_code like ','"',po_price$prod_code[i],'"')
#           dbExecute(conn,query)
#         }
#         dbDisconnect(conn)
#         out_msg <- paste0(
#           out_msg, '\n',ui_elem$actual[ui_elem$label=='update_cost_success'])
#       }
#       if (out_msg==''){
#         out_msg <- ui_elem$actual[ui_elem$label=='nothing_to_update']
#       }
#     }
#     # reload import_log
#     reload_tbl(config_dict,'import_log')
#     
#     # show an alert
#     big_msg <- ui_elem$actual[ui_elem$label=='done']
#     shinyalert(title = big_msg, text = out_msg, type = "success")
#   }
#   
#   
#   else{
#     if(length(full_path)==0){
#       show_alert("error","po_file_not_found","error")}
#     if(length(full_path)>1){
#       show_alert("error","multiple_po_file_found","error")}
#   }

sep_add_po_import_price <- function(input,output){
  po_name <- input$sep_po_list
  vendor_id <- guess_vendor_id(po_name,mode='filepath')
  if (length(vendor_id)==1){
    write_po_price(po_name)
    show_alert("success","add_import_price_success","success")
  }else{
    show_alert("error","unknown_vendor","error")
  }
}