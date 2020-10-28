# the edit_dt function is used to edit all display dataTables in the ui
edit_dt <- function(input,tbl_name){
  
  # the purpose of each if clause is to generate the dt_edit_query
  # based on the table edited
  if(tbl_name=='admin_activity_log'){
    admin_id <- staff_info$admin_id[staff_info$admin_name==input$admin_name]
    sync_table <- read_activity_log(admin_id)
    dt_edit_info <- input$admin_activity_log_cell_edit
    db_tbl_2edit <- 'staff_activity_log'
    dt_edit_query <- gen_dt_edit_query(sync_table,dt_edit_info,db_tbl_2edit)
  }
  db_exec_query(dt_edit_query)
  reload_tbl(config_dict,db_tbl_2edit)
}

gen_dt_edit_query <- function(sync_table,dt_edit_info,db_tbl_2edit){
  
  # get the data and construct the query
  row_2edit <- sync_table[dt_edit_info$row[1],]
  colname_2edit <- names(sync_table)[dt_edit_info$col[1]+1]
  new_value <- dt_edit_info$value
  dt_edit_query <- paste0("update ",db_tbl_2edit," set ",colname_2edit,"='",
                  new_value,"' where stt=",row_2edit$stt[1],
                  " and activity_date='",row_2edit$activity_date[1],"'")
  return(dt_edit_query)
}