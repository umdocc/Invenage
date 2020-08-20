render_customer_list <- function(
  iid, type='inv_out',input,allow_add=T){renderUI({
    
  cust_choices <- get_cust_list(config_dict,type) # default list
  clabel <- ui_elem$actual[ui_elem$label=='customer_name'] # default label
  
  # set default customer/customer list based on type
  if (type=='add_customer'){
    default_customer <- cust_choices[1]
  }
  if (type=='inv_out'){
    default_customer <- cust_choices[1]
    allow_add <- F
  }
  if (type=='customer_change'){
    man_selected_pxk <- input$man_pxk_list
    conn <- db_open(config_dict)
    man_selected_pxk_info <- dbGetQuery(
      conn, paste('select * from pxk_info where pxk_num =', man_selected_pxk))
    dbDisconnect(conn)
    man_selected_pxk_info <- merge(man_selected_pxk_info, customer_info)
    # print(man_selected_pxk_info)
    default_customer <- man_selected_pxk_info$customer_name[1]
    allow_add <- F
  }
  selectizeInput(
    inputId = iid, label = clabel, choices = cust_choices, 
    selected = default_customer, options = list(create = allow_add))
}) }