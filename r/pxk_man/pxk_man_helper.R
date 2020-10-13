# function to edit pxk using pxk_num and 'cell' value
edit_db_pxk <- function(cell,pxk_num){
  #use render_selected_pxk maintain consistency with ui
  updated_pxk <- render_selected_pxk(pxk_num,config_dict)
  # reverse the column name
  updated_pxk <- rev_trans_tbl_column(updated_pxk, ui_elem)
  edited_stt <- updated_pxk$stt[cell$row] # get the edit row stt
  
  # read the pxk from database
  conn = db_open(config_dict)
  tmp <- dbGetQuery(
    conn,paste('select * from sale_log where pxk_num =',pxk_num))
  dbDisconnect(conn)

  # allow editing certain fields only
  allow_edit_pxk_colnum <- as.numeric(
    unlist(strsplit(config$allow_pxk_edit_col,split=';'))) 
  if (cell$col %in% (allow_edit_pxk_colnum-1) ){ # dt cell has offset of 1
    tmp[tmp$stt==edited_stt,
        pxk_render_colnames[cell$col+1]] <- as.character(cell$value)
  }
  # check data
  tmp$unit <- tolower(tmp$unit) # clean the unit first
  
  # check the edited colname
  # print(pxk_render_colnames[cell$col+1])
  
  errorsFree=T
  if (pxk_render_colnames[cell$col+1]=='unit'){ #check the unit
    test <- merge(tmp,packaging)
    if(length(test$units_per_pack[test$stt==edited_stt])==0){
      errorsFree = F
      # do something to alert user here
    }
  }
  if (errorsFree){
    conn = db_open(config_dict)
    dbExecute(
      conn, paste('delete from sale_log where pxk_num =', pxk_num))
    # dbWriteTable(conn,'sale_log',tmp,append=T)
    dbDisconnect(conn)
    append_tbl_rld(config_dict,'sale_log',tmp)
  }
  return(errorsFree)
}