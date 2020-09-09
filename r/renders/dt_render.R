# display the data table
render_dt <- function(
  input,tbl_name = 'admin_activity_log',allow_edit=T){DT::renderDataTable({
    if(tbl_name=='admin_activity_log'){
      # read, translate and render
      admin_id <- staff_info$admin_id[staff_info$admin_name==input$admin_name]
      out_table <- read_activity_log(admin_id, mode='display')
      numofrow <- as.integer(config_dict$value[
        config_dict$name=='admin_activity_log_numofrow'][1])
    }
    
    if(tbl_name=='vendor_info_tbl'){
      # out_table is just vendor_info, with some cosmetic fix
      out_table <- vendor_info %>% arrange(desc(vendor_id))
      numofrow <- as.integer(config$vendor_info_numofrow)
      
      # translate local table data
      trans_str <- unlist(strsplit(config$local_noyes_str,split=';'))
      out_table$local <- trans_str[out_table$local+1]
      #translate orig_vendor data
      trans_str <- unlist(strsplit(config$orig_vendor_noyes_str,split=';'))
      out_table$orig_vendor <- trans_str[out_table$orig_vendor+1]
      out_table$group_type <- paste(out_table$orig_vendor,out_table$local)
      out_table <- out_table %>% select(vendor,vendor_code,group_type)
      
    }
    
    out_table <- translate_tbl_column(out_table,ui_elem)
    DT::datatable(out_table, options = list(pageLength = numofrow),rownames=F,
                editable = allow_edit)
})
}

# function to read activity log, default for rendering
# this is so that it can sync with editable
# if mode = 'display', optimise table for display purpose
read_activity_log <- function(admin_id, mode='display'){
  current_staff_log <- staff_activity_log[
    staff_activity_log$admin_id==as.numeric(admin_id),]
  current_staff_log <- merge(
    current_staff_log,staff_info %>% select(admin_id,admin_name))
  # remove id, translate and render
  current_staff_log$id <- NULL
  current_staff_log$admin_id <- NULL
  
  # select column,sort
  current_staff_log <- current_staff_log %>% 
    arrange(desc(activity_date),desc(stt))
  display_col <- unlist(strsplit(config_dict$value[
    config_dict$name=='admin_activity_log_displaycol'],split=';'))
  if(mode=='display'){
    current_staff_log <- current_staff_log[,display_col]
  }
  
  return(current_staff_log)
}