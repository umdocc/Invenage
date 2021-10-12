# ------------------------------ ui object -------------------------------------
# if hidden, create blank, otherwise create the tab
if ('cdn' %in% hidden_tab){
  cdn_tab <- tabPanel(
    theme = shinytheme(config$app_theme), uielem$create_delivery_note)
}else{
  cdn_tab <- tabPanel(
  theme = shinytheme(config$app_theme), uielem$create_delivery_note,
  fluidRow(
    box(
      width=3,
      p(), #space
      htmlOutput('cdn_customer'),
      htmlOutput('cdn_prod_name'),
      div(style="display: inline-block;vertical-align:top;width: 90px",
          htmlOutput("cdn_qty")),
      div(style="display: inline-block;vertical-align:top;width: 90px",
          htmlOutput("cdn_unit")),
      # div(style="display: inline-block;vertical-align:top;width: 90px",
      #     htmlOutput("warehouse_selector")),
      div(style="display: inline-block;vertical-align:top;width: 110px",
          htmlOutput("cdn_warehouse")),
      # div(style="display: inline-block;vertical-align:top;width: 160px",
      #     htmlOutput("payment_selector")),
      # div(style="display: inline-block;vertical-align:top;width: 130px",
      #     htmlOutput("unit_price")),
      # div(style="display: inline-block;vertical-align:top;
      #     padding-top:20px;padding-left:10px;width: 140px",
      #     checkboxInput("promo_price",label = get_actual("promo_price"))),
      # htmlOutput("tender_name"),
      # textInput('pxk_note', ui_elem$actual[ui_elem$label=='note']),
      # htmlOutput("prod_info_str"),
      # actionButton("inventory_out", label = get_actual('inv_out')),
      # htmlOutput("sys_msg")
      p()
    )
    # box(
    #   width = 9,
    #   # style = "background-color:#c2e6ff;",
    #   p(),
    #   htmlOutput("current_pxk_info"),
    #   p(),
    #   DT::dataTableOutput("current_pxk_tbl"),
    #   h4(), #space
    #   div(
    #     style="display: inline-block;vertical-align:top;",
    #     h5(ui_elem$actual[ui_elem$label=='del_selected_stt'])
    #   ),
    #   div(style="display: inline-block;vertical-align:top; \
    #                     width: 5px;",HTML("<br>")
    #   ),
    #   div(style="display: inline-block;vertical-align:top; \
    #                     width: 100px;",
    #       htmlOutput('invout_stt_list')
    #   ),
    #   div(
    #     style="display: inline-block;vertical-align:top;width: 150px;",
    #     actionButton(
    #       "del_invout_stt", ui_elem$actual[ui_elem$label=='delete_stt'])
    #   ),
    #   div(style="display: inline-block;vertical-align:top; \
    #                     position:absolute;right:15px",
    #       actionButton(
    #         "complete_form",
    #         ui_elem$actual[ui_elem$label=='complete_form'])
    #   )
    # )# end inv_out box2
  )# end inv_out fluidRow
) # end of ui object
}
# ---------------------------- render functions --------------------------------  
render_current_pxk_infostr <- function(config_dict){renderUI({
  pxk_num <- get_current_pxk(config_dict) # get the current pxk_num
  current_pxk_str <- get_pxk_info_str(pxk_num)
  HTML(current_pxk_str)
}) }

# render table for the invout tab
render_invout_pxktable <- function(){DT::renderDataTable({
  current_pxk <- get_current_pxk(config_dict)
  output <- render_selected_pxk(current_pxk,config_dict)
  
  DT::datatable(output, options = list(pageLength = 10),rownames=F)
})
}

# -------------------------------- button handler ------------------------------
# inv_out complete_form button
complete_current_pxk <- function(input,output){
  finalised_pxk_num <- get_current_pxk(config_dict)
  
  # update completed field in databse
  conn <- db_open(config_dict)
  query <- paste0("update pxk_info set completed = 1
                    where pxk_num = ",finalised_pxk_num)
  dbExecute(conn,query)
  read_tbl(conn,'pxk_info')
  dbDisconnect(conn)
  
  # create the excel for current pxk, (open file by defaut)
  dest_path <- create_pxk_file(finalised_pxk_num, open_file=T)
  
  # reload the ui
  output <- reload_ui(
    input,output, split_semi(config$io_complete_form_ui_reload))
  
}

io_del_inv_out_stt <- function(input,output){ 
  current_pxk <- get_current_pxk(config_dict)
  current_stt_list <- get_pxk_entry_num(current_pxk,config_dict)
  # if there is record in current pxk, allow delete
  if (length(current_stt_list)>0){
    stt_to_proc <- as.character(input$invout_stt_list)
    delete_pxk(current_pxk,stt_to_proc,config_dict)
  }
  
  # reload the ui
  output <- reload_ui(input,output,
                      split_semi(config$io_del_inv_out_stt_ui_reload))
}