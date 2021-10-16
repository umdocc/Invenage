update_db_pxk <- function(pxk_num,edited_stt,col_name,new_value){
  error_free <- T
  
  # read config variables: list of column in pxk and edit-allowed columns
  allowed_colname <- split_semi(config$pxkm_allow_edit_colname)
  
  if(col_name %in% allowed_colname){
    
    # read the edited row from database
    tmp <- db_read_query(
      paste('select * from sale_log where pxk_num =',pxk_num," and stt =",
            edited_stt))
    # extract unit_price for checking later on
    current_unit_price <- as.numeric(tmp$unit_price)
    
    # if returned data has more than 1 row, it indicate a duplication
    # unfortunately we need to stop the app and investigate the db
    if (nrow(tmp)>1){stop("duplicated stt found in db, shut down!")}
    
    # if error_free is set to false during check, show alert then do nothing
    
    if(col_name=='unit'){
      new_value <- tolower(new_value)
      prod_code <- tmp$prod_code
      test_tbl <- db_read_query(
        paste0("select * from packaging where prod_code='",prod_code,"'",
               " and unit='",new_value,"'"))
      if(nrow(test_tbl)==0){
        error_free <- F
        show_alert("error","invalid_unit","error")
      }
    }
    if(col_name=='qty'){
      new_value <- as.numeric(new_value)
      if(is.na(new_value)){
        error_free <- F
        show_alert("error","number_only_col","error")
      }
    }
    if(col_name=='unit_price'){
      new_value <- as.numeric(new_value)
      if(is.na(new_value)&!is.na(current_unit_price)){
        error_free <- F
        show_alert("error","number_only_col","error")
      }
    }
    
    # if all is good, write to database
    if (error_free){
      
      # slightly different query for strings and numbers
      if(col_name %in% c('unit','note','lot')){
        update_query <- paste0(
          "update sale_log set ",col_name,"='",new_value,"' where id=",
          tmp$id)
      }else{
        update_query <- paste0(
          "update sale_log set ",col_name,"=",new_value," where id=",
          tmp$id)
      }
      
      #exec
      db_exec_query(update_query)
    }
    
  }else{
    show_alert("error","edit_not_allowed","error")
  }
  return(error_free)
}

# function to edit pxk using pxk_num and 'cell' value
edit_db_pxk <- function(cell,pxk_num){
  
  # list of colnames in pxk
  pxk_render_colnames <- split_semi(config$pxk_render_colnames)

  # get the stt of edited row
  # since shiny editable cell only display relative to current table
  # we need to first rebuild the table, then extract the stt
  updated_pxk <- render_selected_pxk(pxk_num,config_dict)
  # reverse the column name
  updated_pxk <- rev_trans_tbl_column(updated_pxk, ui_elem)
  edited_stt <- updated_pxk$stt[cell$row] # get the stt
  
  # dt cell has offset 1, so cell$col=2 point to the 3rd col in the table
  edited_colname <- pxk_render_colnames[cell$col+1]
  # get the new_value
  new_value <- as.character(cell$value)
  
  # update the database
  error_free <- update_db_pxk(pxk_num,edited_stt,edited_colname,new_value)
  
  return(error_free)
}