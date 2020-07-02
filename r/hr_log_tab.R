# uui renderer for hr_log tab

# # -------------------------- ui renderers --------------------------------------
render_admin_name <- function(){renderUI({
  admin_name <- staff_info$admin_name[
    staff_info$admin_id==as.integer(
      config_dict$value[config_dict$name=='admin_id'])]
  selectInput('admin_name','Admin Name',choices = admin_name,selected = admin_name)
}) }

render_hour_logged <- function(){renderUI({
  selectizeInput('hour_logged','Hour',choices = c(),selected = admin_name)
}) }