# ----------------------------- update_customer_tab ----------------------------
if ('tech_service_log' %in% hidden_tab){
  tech_service_log_tab <- tabPanel(
    get_actual('tech_service_log'))
}else{
  tech_service_log_tab <- tabPanel(
    get_actual('tech_service_log'),
    fluidRow(
      useShinyalert(),  # Set up shinyalert
      box(width = 3, height = 400, style = "background-color:#f5f5f5;",
          dateInput("tsl_service_date",label=get_actual("entry_date")),
          htmlOutput("tsl_engineer_name"),
          htmlOutput("tsl_customer_name"),
          htmlOutput("tsl_analyser_name"),
          htmlOutput("tsl_analyser_lot"),
          htmlOutput("tsl_service_type"),
          textAreaInput("tsl_service_detail", label=get_actual("detail"), 
                        rows = 10, 
                        placeholder =  get_actual("tsl_service_detail_guide")),
          actionButton("tsl_add_entry",
                       label=get_actual("enter_data"))
      ),
      box(width = 9, height = 800, style = "background-color:#f5f5f5;",
          DT::dataTableOutput("tsl_service_log")
      )
    )
  )
}

# ------------------------------ Sidebar UI ------------------------------------
tsl_render_engineer_name <- function(){renderUI({
  engineer_list <- db_read_query(
    "select * from staff_info where admin_group='engineer'")$admin_name
  
  selectInput(inputId = 'tsl_engineer_name',
              label = get_actual('admin_name'),
              choices = engineer_list,
              selected = engineer_list[1])
}) }

tsl_render_customer_name <- function(){renderUI({
  customer_name_list <- db_read_query(
    'select distinct customer_info.customer_name from tsl_analyser_list 
    inner join customer_info
    on tsl_analyser_list.customer_id=customer_info.customer_id')$customer_name
  selectizeInput(
    inputId = "tsl_customer_name",
    label = get_actual("customer_name"),
    choices = customer_name_list,
    selected = customer_name_list[1]
    )
})}

tsl_render_analyser_name <- function(input,output){renderUI({

  # get the customer_id from selected customer
  customer_name <- input$tsl_customer_name
  customer_id <- db_read_query(paste0(
    "select distinct tsl_analyser_list.customer_id from tsl_analyser_list 
    inner join customer_info on
    tsl_analyser_list.customer_id = customer_info.customer_id
    where customer_info.customer_name ='",customer_name,"'"))$customer_id
  
  # get the list of analyser matching the customer
  # print(customer_id)
  analyser_list <- db_read_query(paste0(
    "select distinct tsl_analyser_list.prod_code, product_info.comm_name
    from tsl_analyser_list inner join product_info 
    on tsl_analyser_list.prod_code = product_info.prod_code
    where tsl_analyser_list.customer_id =",customer_id))$comm_name
  
  # render the UI
  selectizeInput(
    inputId = "tsl_analyser_name",
    label = get_actual("analyser_name"),
    choices = analyser_list,
    selected = analyser_list[1]
  )
})}

tsl_render_analyser_lot <- function(input,output){renderUI({

  # get the customer_id from selected customer
  customer_id <- db_read_query(paste0(
    "select distinct tsl_analyser_list.customer_id from tsl_analyser_list 
    inner join customer_info on
    tsl_analyser_list.customer_id = customer_info.customer_id
    where customer_info.customer_name ='",
    input$tsl_customer_name,"'"))$customer_id
  
  # analyser lot
  analyser_prodcode <- db_read_query(paste0(
    "select product_info.prod_code from product_info where comm_name='",
    input$tsl_analyser_name,"'"))$prod_code

  analyser_lot <- db_read_query(paste0(
    "select tsl_analyser_list.lot from tsl_analyser_list 
    where customer_id=",customer_id," and prod_code='",
    analyser_prodcode,"'"))$lot
  
  # render the UI
  selectizeInput(
    inputId = "tsl_analyser_lot",
    label = get_actual("lot"),
    choices = analyser_lot,
    selected = analyser_lot[1]
  )
})}

tsl_render_service_type <- function(input,output){renderUI({
  # get list of service type from db and merge with localisation
  service_type <- data.frame(
    label = unlist(strsplit(config$tsl_input_service_type,";")))
  service_type <- merge(service_type,ui_elem)$actual
  
  radioButtons(
    inputId = "tsl_service_type", label=NULL,
    choices = service_type, selected = service_type[1],
    inline = T)
})}

# --------------------------- Output Table UI ----------------------------------

tsl_render_service_log <- function(){DT::renderDataTable({
    # read and clean the table
    out_tbl <- db_read_query(
      "select staff_info.admin_name, customer_info.customer_name,
      product_info.comm_name, tsl_service_log.lot,
      tsl_service_log.service_date as entry_date,
      tsl_service_log.service_detail as detail,
      localisation.actual as service_type
      from tsl_service_log inner join staff_info
      on tsl_service_log.admin_id = staff_info.admin_id 
      inner join customer_info
      on tsl_service_log.customer_id = customer_info.customer_id
      inner join product_info
      on tsl_service_log.prod_code = product_info.prod_code
      inner join tsl_service_type
      on tsl_service_log.service_type = tsl_service_type.service_type
      inner join localisation
      on tsl_service_type.label = localisation.label
      order by tsl_service_log.id asc")
    out_tbl <- translate_tbl_column(out_tbl,ui_elem)
    DT::datatable(out_tbl, 
                  options = list(pageLength = config$tsl_service_log_numofrow),
                  rownames=F, editable = F)
  })
}
