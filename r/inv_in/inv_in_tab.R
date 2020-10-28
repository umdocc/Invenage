# inv_out tab ui and functions
# ---------------------------- shiny ui object --------------------------------
if('inv_in' %in% hidden_tab){
  inv_in_tab <- tabPanel(ui_elem$actual[ui_elem$label=='inv_in'])
}else{
inv_in_tab <- tabPanel(
  theme = shinytheme("united"), ui_elem$actual[ui_elem$label=='inv_in'],
  fluidRow(
    box(
      width=3, height = 800,
      p(), # space
      h3(ui_elem$actual[ui_elem$label=='inv_in']),
      htmlOutput('in_prodname_select'),
      div(style="display: inline-block;vertical-align:top;width: 135px",
          htmlOutput('in_vendor')),
      div(style="display: inline-block;vertical-align:top;width: 135px",
          htmlOutput('in_invoice_num')),
      div(style="display: inline-block;vertical-align:top;width: 110px",
          selectizeInput(
            inputId = 'in_qty', label = ui_elem$actual[ui_elem$label=='qty'],
            choices = 1:1000, options = list(create = TRUE))),
      div(style="display: inline-block;vertical-align:top;width: 110px",
          htmlOutput('in_unit')),
      div(style="display: inline-block;vertical-align:top; \
                        width: 5px;",HTML("<br>")),
      div(style="display: inline-block;vertical-align:top; \
                        width: 110px",
          textInput('in_lot',label=ui_elem$actual[ui_elem$label=='lot'])),
      div(style="display: inline-block;vertical-align:top; \
                        width: 110px",
          textInput('in_expdate',
                    label=ui_elem$actual[ui_elem$label=='exp_date'])),
      div(style="display: inline-block;vertical-align:top;width: 5px;",
          HTML("<br>")),
      div(style="display: inline-block;vertical-align:top;width: 140px",
          htmlOutput('in_actual_unit_cost')),

      div(style="display: inline-block;vertical-align:top;width: 140px",
          htmlOutput('in_vat_percent')),
      div(style="display: inline-block;vertical-align:top;width: 140px",
          htmlOutput('in_warehouse')),
      div(style="display: inline-block;vertical-align:top;width: 140px",
          htmlOutput('in_note')),
      actionButton("inv_in",
                   ui_elem$actual[ui_elem$label=='inv_in'])
    ),
    box(
      width=9, height = 800,
      h3(ui_elem$actual[ui_elem$label=='recent_import']),
      DT::dataTableOutput("latest_import_tbl"),
      p() #space
      
    )
  )
)
}

# render the import_log for ui
render_import_log <- function(){
  DT::renderDataTable({
    # output_tbl <-merge(
    #   import_log, product_info %>% select(prod_code,comm_name), all.x=T) 
    # # replace warehouse_id with in_warehouse_id
    # output_tbl$warehouse_id <- output_tbl$in_warehouse_id
    # output_tbl$in_warehouse_id <- NULL
    # # merge info into note
    # output_tbl <- merge(
    #   output_tbl, vendor_info %>% select(vendor,vendor_id), all.x=T)
    # output_tbl$qty <- paste(output_tbl$qty,output_tbl$unit)
    # output_tbl <- merge(
    #   output_tbl,warehouse_info %>% select(warehouse,warehouse_id), 
    #   all.x=T)
    # output_tbl$merged_note <- paste(output_tbl$vendor,output_tbl$warehouse,
    #                                 output_tbl$note,
    #                                 sep=";")
    # output_tbl$note <- output_tbl$merged_note
    # output_tbl <- output_tbl[order(output_tbl$id, decreasing = T),]
    # output_tbl <- output_tbl %>% 
    #   rename(vat_percent=in_vat_percent, invoice_num =  in_invoice_num) %>%
    #   select(
    #     comm_name, qty, lot, exp_date, actual_unit_cost, 
    #     invoice_num, vat_percent, note)
  
    # output_tbl <- translate_tbl_column(output_tbl, ui_elem)
    output_tbl <- import_log
    DT::datatable(output_tbl, options = list(pageLength = 10),rownames=F)
  })
}

# render display data
render_output_tbl <- function(input, table_name='vendor_invoice'){
  DT::renderDataTable({
  if (table_name=='vendor_invoice'){
    output_tbl <- vendor_invoice
    output_tbl <- merge(output_tbl,vendor_info %>% select(vendor_id,vendor))
    output_tbl <- output_tbl[order(output_tbl$id, decreasing = T),]
    output_tbl <- output_tbl %>% 
      select(vendor, invoice_num, invoice_date, invoice_amount, 
             invoice_cd_num, po_name)
  }
  if (table_name=='po_detail'){
    po_name <- input$po_man_po_list
    # print(po_name)
    output_tbl <- read_po_data(po_name)
    output_tbl <- output_tbl %>%
      select(name, ref_smn, qty, lot, exp_date, actual_unit_cost)
  }
  output_tbl <- translate_tbl_column(output_tbl, ui_elem)
  DT::datatable(output_tbl, options = list(pageLength = 10),rownames=F)
})
}

render_in_invoice_num <- function(iid,ui_label){renderUI({
  selectizeInput(iid,label=ui_label,choices=NULL,options = list(create=T))
  })
}

render_vat_percent <- function(input,iid,ui_label,tab='inv_in'){renderUI({
  current_prod_code <- product_info$prod_code[
    product_info$search_str==input$in_prodname_select]
  current_vendor_id <- vendor_info$vendor_id[
    vendor_info$vendor==input$in_vendor]

  # set up vat choices and the latest vat
  vat_choices <- c(0,5,7,10)
  last_vat <- import_log[import_log$prod_code==current_prod_code &
    import_log$vendor_id==current_vendor_id,]
  
  if(nrow(last_vat)>0){
    last_vat <- last_vat$in_vat_percent[
      last_vat$delivery_date==max(last_vat$delivery_date)]
  }else{
    last_vat <- vat_choices[1] # if nothing default to first
  }
  
  selectizeInput(iid,label=ui_label,choices=vat_choices,
                 selected=last_vat,
                 options = list(create=T))
  })
}