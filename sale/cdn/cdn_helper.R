cdn_load_ui <- function(input,output,ui_list){
  if ('cdn_customer' %in% ui_list){
    output$cdn_customer <- render_cdn_customer(input) # customer
  }
  if ('cdn_prod_name' %in% ui_list){
    output$cdn_prod_name <- render_cdn_prod_name(input)
  }
  
  return(output)
}

render_cdn_customer <- function(input){renderUI({
    
    cust_choices <- db_read_query(
      "select customer_name from customer_info")$customer_name

    selectizeInput(
      inputId = "cdn_customer", label = uielem$customer_name, 
      choices = cust_choices, selected = cust_choices[1], 
      options = list(create = F))
  })
}

render_cdn_prod_name <- function(input){renderUI({
  
  prod_choices <- db_read_query(
    "select comm_name from product_info where active=1")$comm_name
  
  selectizeInput(
    inputId = "cdn_prod_name", label = uielem$comm_name, 
    choices = prod_choices, selected = prod_choices[1], 
    options = list(create = F))
})
}

# # function to check if an inv_out entry should be allowed before writing to db
# check_inv_out <- function(append_sale_log, config_dict){
#   inv_out_ok <- T
#   curent_prodcode <- as.character(append_sale_log$prod_code[1])
#   current_lot <- as.character(append_sale_log$lot[1])
#   
#   # fix r problem of using factor for everything before convert to pack
#   append_sale_log$qty <- as.numeric(as.character(append_sale_log$qty))
#   tmp <- convert_to_pack(append_sale_log,packaging,'qty','pack_qty')
#   sale_qty <- round(tmp$pack_qty,digits=4)
#   inventory <- update_inventory(config_dict)
#   inv_remain <- inventory$remaining_qty[inventory$prod_code == curent_prodcode &
#                                           inventory$lot == current_lot][1]
#   if (is.na(inv_remain)){inv_remain <- 0}
#   inv_remain <- round(inv_remain,digits = 4)
#   # check for errors, display message if something is wrong
#   if (inv_remain<sale_qty){
#     show_alert('error','inv_exceed','error')
#     inv_out_ok <- F }
#   
#   return(inv_out_ok)
# }
# 
# # the get_avail_lot function get a list of available lot, it returns a vector
# # if sortType ='fifo', the earliest exp_date will be on top
# get_avail_lot <- function(current_prod_code,config_dict,sort_type='fifo'){
#   inventory <- update_inventory(config_dict)
#   if (sort_type == 'fifo'){
#     avail_lot <- inventory[inventory$prod_code==current_prod_code,]
#     avail_lot <- avail_lot[order(avail_lot$intexp_date,
#                                  na.last = F, # put NA lot first
#                                  decreasing = F),] #lowest exp_date first
#   }
#   avail_lot <- avail_lot$lot
#   return(avail_lot)
# }
# 
# # function to render pxk information for get_pxk_info_str
# get_pxk_info <- function(pxk_num,translate=TRUE){
#   # read info for pxk_num
#   conn <- db_open(config_dict)
#   query <- paste('select * from pxk_info where pxk_num =',pxk_num)
#   current_pxk_info <- dbGetQuery(conn, query)
#   dbDisconnect(conn)
#   if (nrow(current_pxk_info)==0){
#     current_pxk_info[1,1] <- 1
#     current_pxk_info$pxk_num[1] <- pxk_num
#   }
#   # recover information
#   current_pxk_info <- merge(current_pxk_info,customer_info,all.x=T)
#   current_pxk_info <- merge(current_pxk_info,payment_type,all.x=T) %>% 
#     rename(payment_type = actual)
#   
#   # translate completed column
#   current_pxk_info$label <- ifelse(
#     is.na(current_pxk_info$completed), 'new', ifelse(
#       current_pxk_info$completed==1, 'completed', 'in_progress'))
#   current_pxk_info <- merge(
#     current_pxk_info, ui_elem %>% select(label,actual))
#   current_pxk_info <- current_pxk_info %>% rename(status = actual)
#   # select relevant column and format output
#   current_pxk_info <- current_pxk_info %>% 
#     select(pxk_num,customer_name, payment_type, status)
#   current_pxk_info$pxk_num <- as.character(current_pxk_info$pxk_num)
#   # translate the output
#   if (translate){
#     current_pxk_info <- translate_tbl_column(current_pxk_info,ui_elem)
#   }
#   return(current_pxk_info)
# }
# 
# # an ui line that display pxk information
# get_pxk_info_str <- function(pxk_num){
#   man_pxk_info <- get_pxk_info(pxk_num)
#   pxk_info_str <- '<font size=+1>'
#   for (i in 1:length(man_pxk_info)){
#     pxk_info_str <- paste0(
#       pxk_info_str, names(man_pxk_info)[i],':', em(man_pxk_info[1,i]),
#       '&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp')
#   }
#   pxk_info_str <- paste(pxk_info_str,'<font><br/>')
#   return(pxk_info_str)
# }
# 
# 
# get_current_pxk <- function(cofig_dict){
#   admin_id <- config_dict$value[config_dict$name=='admin_id']
#   if (length(admin_id)!=1){
#     stop('Critical Error! admin_id not found')
#   }
#   conn <- db_open(config_dict)
#   pxk_num_list <- dbGetQuery(conn,'select pxk_num from pxk_info')
#   current_pxk <- dbGetQuery(
#     conn,paste0("select pxk_num from pxk_info where completed = 0 and ",
#                 "admin_id = ",admin_id))
#   dbDisconnect(conn)
#   if (nrow(current_pxk)>0){
#     newPXK = current_pxk$pxk_num[1]
#   }else{
#     currentDate <- strftime(Sys.time(),'%d%m%y')
#     i <- 1;newPXKNum <- F
#     while (!newPXKNum){
#       tmp_num <- as.numeric(paste0(admin_id, currentDate,
#                                    sprintf("%02d",i)))
#       if (length(pxk_num_list[pxk_num_list$pxk_num==tmp_num,'pxk_num'])==0){
#         newPXK <- tmp_num
#         newPXKNum <- T
#       }else{
#         i <- i+1
#       }
#     }
#   }
#   return(newPXK)
# }
# 
# 
# cdn_add_entry <- function(input,output){
#   # custom display message
#   output$sys_msg <- render_sys_message('please wait....')
#   
#   # read info from database
#   current_pxk <- get_current_pxk(config_dict)
#   
#   # if this is a new pxk, write to database first
#   if (nrow(pxk_info[pxk_info$pxk_num==current_pxk,])==0){
#     appendPXKInfo <- data.frame(
#       pxk_num = current_pxk,
#       # time variable needs to be in UTC
#       sale_datetime = format(as.POSIXlt(Sys.time(),tz="UTC"),
#                              '%Y-%m-%d %H:%M:%S'),
#       customer_id = customer_info[
#         customer_info$customer_name==input$customer_name,'customer_id'],
#       payment_code = payment_type$payment_code[
#         payment_type$actual == input$payment_type],
#       completed = 0,
#       admin_id = admin_id
#     )
#     append_tbl_rld(config_dict,'pxk_info',appendPXKInfo)
#     
#     # set current_stt also
#     current_stt <- 1
#   }else{ #otherwise, read the info from the sale_log
#     conn <- db_open(config_dict)
#     stt_list <- dbGetQuery(conn, paste0("select stt from sale_log
#                                where pxk_num = (
#                                select pxk_num from pxk_info
#                                where completed = 0 and admin_id=",admin_id,
#                            ")"))
#     dbDisconnect(conn)
#     # if there is a result, determine the stt from list, otherwise set to 1
#     if (nrow(stt_list)>0){
#       for (i in 1:15){ # loop 20 times
#         if (!any(i==stt_list$stt)){
#           current_stt <- i
#           break
#         }
#       }
#     }else{
#       current_stt <- 1
#     }
#   }
#   
#   print(current_stt)
#   # build base sale_log for testing first
#   append_sale_log <- data.frame(
#     stt = current_stt,
#     prod_code = unique(
#       product_info[product_info$search_str==input$prod_name_select,
#                    "prod_code"]),
#     unit = input$unit_selector,
#     lot = input$lot_select,
#     unit_price = as.integer(input$unit_price),
#     qty = input$qty_selector,
#     pxk_num = current_pxk,
#     note = input$pxk_note
#   )
#   print(append_sale_log)
#   
#   # check and write append_sale_log to database
#   inv_out_ok <- check_inv_out(append_sale_log, config_dict)
#   if (current_stt>10){ #limit the max stt to 10
#     inv_out_ok <- F
#     show_alert('error','pxk_line_exceed','error')
#   }
#   if (inv_out_ok){
#     # add warehouse_id and tender_id
#     current_warehouse_id <- warehouse_info$warehouse_id[
#       warehouse_info$warehouse == input$warehouse_selector]
#     append_sale_log$warehouse_id <- current_warehouse_id
#     current_tender_id <- tender_info$tender_id[
#       tender_info$customer_tender_name==input$tender_name & 
#         tender_info$warehouse_id==current_warehouse_id]
#     
#     # special handling of no tender
#     tender_0_name <- tender_info$customer_tender_name[
#       tender_info$tender_id==0]
#     if(input$tender_name==tender_0_name){
#       current_tender_id <- 0
#     }
#     append_sale_log$tender_id <- current_tender_id
#     
#     append_sale_log$promotion_price <- as.numeric(input$promo_price)
#     
#     # writing to database
#     append_tbl_rld(config_dict,'sale_log',append_sale_log)
#     # update sys_msg
#     output$sys_msg <- render_sys_message(
#       ui_elem$actual[ui_elem$label=='inv_out_success'])
#     
#     # reload the ui
#     output <- reload_ui(input,output,split_semi(config$io_inv_out_ui_reload))
#   }
# }
# 
# # a function to delete certain stt on pxk, or if stt = 'all' will delete all
# delete_pxk <- function(pxk_num,stt,config_dict){
#   if (stt=='all'){
#     query <- paste0('delete from sale_log where pxk_num = ',
#                     pxk_num)
#   }else{
#     query <- paste0('delete from sale_log where pxk_num = ',
#                     pxk_num,' and stt = ',stt)
#   }
#   conn = db_open(config_dict)
#   dbExecute(conn,query)
#   read_tbl(conn,'sale_log')
#   dbDisconnect(conn)
# }
# 
# # this function is also shared with pxk_man tab
# render_selected_pxk <- function(selected_pxk_num,config_dict,localised=T){
#   
#   # read the pxk
#   output_pxk <- db_read_query(
#     paste0("select * from sale_log where pxk_num=",selected_pxk_num))
#   output_pxk <- merge(output_pxk,product_info %>% select(prod_code,comm_name))
# 
#   output_pxk <- output_pxk %>% 
#     select(stt,comm_name,unit,qty,unit_price,lot,note)
#   output_pxk <- output_pxk[order(output_pxk$stt),] # sort by stt
#   if (localised){
#     ui_elem <- get_ui_elem(config_dict)
#     for (i in 1:length(output_pxk)){
#       if (length(ui_elem$actual[ui_elem$label==names(output_pxk)[i]])==1){
#         names(output_pxk)[i] = ui_elem$actual[
#           ui_elem$label==names(output_pxk)[i]]
#       }
#     }
#   }
#   return(output_pxk)
# }
# 
# # this function create an excel PXK from a given pxk_num
# create_pxk_file <- function(pxk_num,open_file=T){
#   # create new PXK file
#   orig_path <- file.path(config$form_path,'pxk_form.xlsx')
#   dest_path <- file.path(config$pxk_out_path,
#                          paste0(company_name,".PXK.",
#                                 pxk_num,".xlsx"))
#   error_free <- check_print_pxk(input)
#   if(error_free){
#   wb <- loadWorkbook(orig_path)
#   
#   # get the expDate, if a Lot has 2 expDate, select only the 1st
#   # need to get all items, not just positive ones
#   tmp <- update_inventory(config_dict,pos_item=FALSE)
#   exp_date <- tmp %>% select(prod_code,lot,exp_date) %>% unique()
#   exp_date <- exp_date[!duplicated(exp_date$lot),]
#   
#   # read the data
#   conn <- db_open(config_dict)
#   # current_pxk_info
#   query <- paste("SELECT * from pxk_info where pxk_num =",pxk_num)
#   current_pxk_info <- dbGetQuery(conn,query)
#   payment_type <- dbReadTable(conn,'payment_type')
#   current_pxk_info <- merge(current_pxk_info,payment_type)
#   current_pxk_info <- merge(
#     current_pxk_info,ui_elem, by.x='payment_label', by.y='label')
#   
#   # form_data
#   query <- paste("SELECT sale_log.stt, product_info.comm_name, product_info.ref_smn,
#                    sale_log.unit, sale_log.unit_price,
#                    sale_log.qty,sale_log.lot, sale_log.note, sale_log.prod_code
#                    FROM   sale_log INNER JOIN product_info
#                    ON     sale_log.prod_code = product_info.prod_code
#                    WHERE  sale_log.pxk_num =",pxk_num)
#   
#   form_data <- dbGetQuery(conn,query)
#   form_data <- merge(form_data,exp_date,all.x=T)
#   
#   # calculate total price
#   form_data$total_price <- form_data$unit_price*form_data$qty
#   
#   # get customer data
#   query <- paste("SELECT DISTINCT customer_info.customer_name
#                     FROM pxk_info INNER JOIN customer_info
#                     ON pxk_info.customer_id = customer_info.customer_id
#                     WHERE pxk_info.PXK_num =", pxk_num)
#   printingCustomerName <- dbGetQuery(conn,query)
#   printingCustomerName <- printingCustomerName$customer_name[1]
#   
#   output_info <- dbGetQuery(
#     conn,'select * from output_info where type = "pxk_output"')
#   dbDisconnect(conn)
#   
#   # writing customer_name
#   customerNameRow <- as.numeric(
#     output_info$value[output_info$name=='customerNameRow'])
#   customerNameCol <- as.numeric(
#     output_info$value[output_info$name=='customerNameCol'])
#   writeData(wb,sheet=1,printingCustomerName, startRow=customerNameRow, 
#             startCol=customerNameCol, colNames = F)
#   
#   # append the customer code if needed
#   if(config_dict$value[config_dict$name=='add_customer_code']=='TRUE'){
#     # read the customer code, then write it to the cell next to customer name
#     outprefix <- config_dict$value[config_dict$name=='customer_code_outprefix']
#     customer_code <- paste0(outprefix,customer_info$customer_code[
#       customer_info$customer_name==printingCustomerName])
#     customer_code_pxkrow <- as.numeric(config_dict$value[
#       config_dict$name=='customer_code_pxkrow'])
#     customer_code_pxkcol <- as.numeric(config_dict$value[
#       config_dict$name=='customer_code_pxkcol'])
#     
#     writeData(wb,sheet=1,customer_code, startRow=customer_code_pxkrow, 
#               startCol=customer_code_pxkcol, colNames = F)
#   }
#   
#   # writing pxkNum
#   pxkNumRow <- as.numeric(output_info$value[output_info$name=='pxkNumRow'])
#   pxkNumCol <- as.numeric(output_info$value[output_info$name=='pxkNumCol'])
#   writeData(wb,sheet=1,pxk_num,startRow=pxkNumRow, 
#             startCol=pxkNumCol, colNames = F)
#   
#   # writing current date
#   date_row <- as.numeric(output_info$value[output_info$name=='date_row'])
#   date_col <- as.numeric(output_info$value[output_info$name=='date_col'])
#   writeData(wb, sheet=1, format(Sys.Date(),config$date_format), 
#             startRow=date_row, startCol=date_col, 
#             colNames = F)
#   
#   # writing payment type
#   out_payment_type <- current_pxk_info$actual
#   payment_type_row <- as.numeric(
#     output_info$value[output_info$name == 'payment_type_row'])
#   payment_type_col <- as.numeric(
#     output_info$value[output_info$name == 'payment_type_col'])
#   writeData(wb, sheet=1, out_payment_type, startRow=payment_type_row, 
#             startCol=payment_type_col, colNames = F)    
#   
#   # get pxkDataHeaders
#   pxkDataHeaders <-  data.frame(matrix(unlist(strsplit(
#     output_info$value[output_info$name=='dataHeaders'],';')),nrow=1))
#   
#   # rearrange Data and write
#   form_data <- form_data[order(as.numeric(form_data$stt)),]
#   dataColumns <- unlist(strsplit(
#     output_info$value[output_info$name=='dataToWrite'],';'))
#   
#   ## convert other info for display purpose
#   form_data$dqty <- formatC(
#     form_data$qty,format='f',big.mark=",",digits = 2)
#   # clean up big unit
#   form_data$dqty <- gsub('\\.00','',form_data$dqty)
#   form_data$dSL <- paste(form_data$dqty, form_data$unit)
#   out_digits <- as.numeric(output_info$value[output_info$name=='out_digits'])
#   form_data$dunit_price <- paste(
#     formatC(form_data$unit_price,format='f',big.mark=",",digits = out_digits),
#     form_data$unit, sep='/')
#   form_data$a_note <- ''
#   
#   # automatically note if unit is not ordering unit
#   ordering_unit <- get_ordering_unit(packaging)
#   names(ordering_unit) <- c('prod_code','ordering_unit')
#   form_data <- convert_to_pack(form_data,packaging,'qty','pack_qty')
#   if(!all(form_data$units_per_pack==1)){
#     # create converted display amount
#     form_data <- merge(form_data,ordering_unit, all.x=T)
#     form_data$a_note <- paste(form_data$pack_qty,form_data$ordering_unit)
#     form_data$a_note[form_data$units_per_pack==1] <- ''
#     form_data$note <- paste(form_data$a_note,form_data$note)
#   }
#   
#   # arrange & select columns for writing
#   form_data <- form_data[order(as.numeric(form_data$stt)),]
#   form_data <- form_data[,dataColumns]
#   
#   # write both data and headers
#   dataStartRow <- as.numeric(
#     output_info$value[output_info$name=='dataStartRow'])
#   dataStartCol <- as.numeric(
#     output_info$value[output_info$name=='dataStartCol'])
#   #write headers first
#   writeData(wb,sheet=1,pxkDataHeaders, startRow=dataStartRow,
#             startCol=dataStartCol, colNames=F)
#   # data is one row below
#   writeData(wb,sheet=1,form_data,startRow=dataStartRow+1,
#             startCol=dataStartCol, colNames=F)
#   # save the excel sheet
#   saveWorkbook(wb,dest_path,overwrite = T)
#   dbDisconnect(conn)
#   
#   #open the file if open_file=T
#   if(open_file){
#     open_file_wtimeout(dest_path)
#   }
# }
# }
# 
# # get_latest_price is a function to get the last price sold to a customer
# get_latest_price <- function(
#   customer_id, prod_code, unit, pxk_info,promo_include=FALSE){
#   sale_lookup <- merge(sale_log,pxk_info,on='pxk_num',all.x=T)
#   latest_price <- -9999
#   # filter through sale_lookup to find price
#   tmp <- sale_lookup[sale_lookup$prod_code == prod_code & 
#                        sale_lookup$customer_id == customer_id &
#                        sale_lookup$unit == unit,]
#   tmp <- tmp[!is.na(tmp$unit_price),]
#   
#   # normally we want to exclude promotion price
#   if (!promo_include){
#     tmp <- tmp[tmp$promotion_price==0,]
#   }
#   
#   # if we can find something, update latest price
#   if (nrow(tmp)>0){
#     tmp <- merge(tmp,pxk_info %>% select(pxk_num,sale_datetime))
#     if (class(tmp$sale_datetime) == "character"){
#       tmp$sale_datetime <- strptime(tmp$sale_datetime,'%Y-%m-%d %H:%M:%S')
#     }
#     latest_price <- tmp$unit_price[
#       tmp$sale_datetime == max(tmp$sale_datetime)]
#   }
#   return(latest_price)
# }