# display the data table
render_dt <- function(
  input,tbl_name = 'admin_activity_log',allow_edit=T){DT::renderDataTable({
    if(tbl_name=='admin_activity_log'){
      # read, translate and render
      admin_id <- staff_info$admin_id[staff_info$admin_name==input$admin_name]
      out_table <- read_activity_log(admin_id, mode='display')
    }
    out_table <- translate_tbl_column(out_table,ui_elem)
    DT::datatable(out_table, options = list(pageLength = 10),rownames=F,
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
    config_dict$name=='display_admin_activity_log'],split=';'))
  if(mode=='display'){
    current_staff_log <- current_staff_log[,display_col]
  }
  
  return(current_staff_log)
}