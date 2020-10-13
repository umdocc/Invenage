# function to edit pxk using pxk_num and 'cell' value
edit_db_pxk <- function(cell,pxk_num){
  
  # read config variables: list of column in pxk and edit-allowed columns
  allowed_colname <- split_semi(config$pxkm_allow_edit_colname)
  render_colname <- split_semi(config$pxk_render_colnames)

  # get the stt of edited row
  #since shiny editable cell only display relative to current table
  # we need to first rebuild the table
  updated_pxk <- render_selected_pxk(pxk_num,config_dict)
  # reverse the column name
  updated_pxk <- rev_trans_tbl_column(updated_pxk, ui_elem)
  edited_stt <- updated_pxk$stt[cell$row] # get the stt
  
  # read the edited row from database
  conn = db_open(config_dict)
  tmp <- dbGetQuery(
    conn,paste('select * from sale_log where pxk_num =',pxk_num," and stt =",
               edited_stt))
  dbDisconnect(conn)
  current_unit_price <- as.numeric(tmp$unit_price)
  
  # if returned data has more than 1 row, it indicate a duplication
  # unfortunately we need to stop the app and investigate the db
  if (nrow(tmp)>1){stop("duplicated stt found in db, shut down!")}
  
  # dt cell has offset 1, so cell$col=2 point to the 3rd col in the table
  edited_colname <- pxk_render_colnames[cell$col+1]

  # allow editing certain fields only
  if (edited_colname %in% allowed_colname){
    tmp[,edited_colname] <- as.character(cell$value)
  }
  
  # check data
  tmp$unit <- tolower(tmp$unit) # clean the unit before checking
  tmp$qty <- as.numeric(tmp$qty)
  tmp$unit_price <- as.numeric(tmp$unit_price)
  
  errorsFree=T
  if (edited_colname=='unit'){ #check the unit
    test <- merge(tmp,packaging)
    if(length(test$units_per_pack[test$stt==edited_stt])==0){
      errorsFree = F
      # do something to alert user here
      show_alert("error","invalid_unit","error")
    }
  }
  
  # check qty and unit_price
  if (is.na(tmp$qty)){
    errorsFree = F
    # do something to alert user here
    show_alert("error","number_only_col","error")
  }
  if (is.na(tmp$unit_price)&!is.na(current_unit_price)){
    errorsFree = F
    # do something to alert user here
    show_alert("error","number_only_col","error")
  }
  
  if (errorsFree){
    if(edited_colname %in% c('unit','note')){
      updated_value <- paste0("'",cell$value,"'")
    }else{
      updated_value <- cell$value
    }
    db_exec_query(
      paste0("update sale_log set ",edited_colname,"=",updated_value,
             " where id=",tmp$id))
  }
  return(errorsFree)
}