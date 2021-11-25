# ------------------------------ ui object -------------------------------------
# if hidden, create blank, otherwise create the tab
if ('aii' %in% hidden_tab){
  aii_tab <- tabPanel(
    theme = shinytheme(config$app_theme), uielem$add_import_item)
}else{
  aii_tab <- tabPanel(
    theme = shinytheme(config$app_theme), uielem$add_import_item,
    fluidRow(
      box(
        width=3,
        p(), #space
        htmlOutput('aii_prod_name'),
        div(style="display: inline-block;vertical-align:top;width: 135px",
            htmlOutput('aii_vendor')),
        div(style="display: inline-block;vertical-align:top;width: 135px",
            htmlOutput('aii_invoice_num')),
        div(style="display: inline-block;vertical-align:top;width: 135px",
            htmlOutput("aii_qty")),
        div(style="display: inline-block;vertical-align:top;width: 135px",
            htmlOutput("aii_unit")),
        div(style="display: inline-block;vertical-align:top;width: 135px",
            htmlOutput("aii_lot")),
        div(style="display: inline-block;vertical-align:top;width: 135px",
            htmlOutput("aii_exp_date")),
        div(style="display: inline-block;vertical-align:top;width: 200px",
            htmlOutput("aii_unit_cost")),
        div(style="display: inline-block;vertical-align:top;width: 70px",
            htmlOutput("aii_vat_percent")),
        div(style="display: inline-block;vertical-align:top;width: 100px",
            htmlOutput('aii_invoice_warehouse')),
        div(style="display: inline-block;vertical-align:top;width: 170px",
            textInput("aii_note",uielem$note)),
        actionButton("aii_add_entry", label = uielem$add_import_item),
        p()
      ),
      box(
        width = 9,
        p(),
        DT::dataTableOutput("aii_import_data"),
        # 
        # h4(), #space
        # div(
        #   style="display: inline-block;vertical-align:top;",
        #   h5(uielem$del_line)
        # ),
        # div(style="display: inline-block;vertical-align:top; \
        #                   width: 5px;",HTML("<br>")
        # ),
        # div(style="display: inline-block;vertical-align:top; \
        #                   width: 100px;",
        #     htmlOutput('invout_stt_list')
        # ),
        # div(
        #   style="display: inline-block;vertical-align:top;width: 150px;",
        #   actionButton(
        #     "del_invout_stt", uielem$delete)
        # ),
        # div(style="display: inline-block;vertical-align:top; \
        #                   position:absolute;right:15px",
        #     actionButton(
        #       "cdn_complete_pxk",
        #       uielem$complete)
        # ),
        p()
      )# end inv_out box2
    )# end inv_out fluidRow
  ) # end of ui object
}
# # ---------------------------- render functions --------------------------------  
# render_current_pxk_infostr <- function(config_dict){renderUI({
#   pxk_num <- get_current_pxk(config_dict) # get the current pxk_num
#   current_pxk_str <- get_pxk_info_str(pxk_num)
#   HTML(current_pxk_str)
# }) }
# 
# # render table for the invout tab
# render_invout_pxktable <- function(){DT::renderDataTable({
#   current_pxk <- get_current_pxk(config_dict)
#   output <- render_selected_pxk(current_pxk,config_dict)
#   
#   DT::datatable(output, options = list(pageLength = 10),rownames=F)
# })
# }
# 
# # -------------------------------- button handler ------------------------------
# # inv_out complete_form button
# complete_current_pxk <- function(input,output){
#   finalised_pxk_num <- get_current_pxk(config_dict)
#   
#   # update completed field in databse
#   conn <- db_open(config_dict)
#   query <- paste0("update pxk_info set completed = 1
#                     where pxk_num = ",finalised_pxk_num)
#   dbExecute(conn,query)
#   read_tbl(conn,'pxk_info')
#   dbDisconnect(conn)
#   
#   # create the excel for current pxk, (open file by defaut)
#   dest_path <- create_pxk_file(finalised_pxk_num, open_file=T)
#   
#   # reload the ui
#   output <- reload_ui(
#     input,output, split_semi(config$io_complete_form_ui_reload))
#   
# }
# 
# io_del_inv_out_stt <- function(input,output){ 
#   current_pxk <- get_current_pxk(config_dict)
#   current_stt_list <- get_pxk_entry_num(current_pxk,config_dict)
#   # if there is record in current pxk, allow delete
#   if (length(current_stt_list)>0){
#     stt_to_proc <- as.character(input$invout_stt_list)
#     delete_pxk(current_pxk,stt_to_proc,config_dict)
#   }
#   
#   # reload the ui
#   output <- reload_ui(input,output,
#                       split_semi(config$io_del_inv_out_stt_ui_reload))
# }