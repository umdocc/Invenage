render_pxk_list <- function(input,config_dict,iid){renderUI({
  conn <- db_open(config_dict)
  pxk_num_list <- dbGetQuery(conn,'select pxk_num from pxk_info')
  dbDisconnect(conn)
  selectizeInput( inputId = iid,
                  label = ui_elem$actual[ui_elem$label=='select_pxk'],
                  choices = pxk_num_list)
}) }