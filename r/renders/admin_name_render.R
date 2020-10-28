render_admin_name <- function(iid="admin_name"){renderUI({
  admin_name <- staff_info$admin_name[
    staff_info$admin_id==as.integer(
      config_dict$value[config_dict$name=='admin_id'])]
  
  if(staff_info$full_hrl[staff_info$admin_id==admin_id][1]==1){
    admin_choice <- staff_info$admin_name
  }else{
    admin_choice <- admin_name
  }
  if(iid=="tsl_engineer_name"){
    staff_type <- staff_info$admin_group[staff_info$admin_name==admin_name]
    if(staff_type=="engineer"|staff_type=="admin1"){
      admin_choice <- staff_info$admin_name[staff_info$admin_group=='engineer']
    }
  }
  
  selectInput('admin_name',
              label = get_actual('admin_name'),
              choices = admin_choice,
              selected = admin_name)
}) }