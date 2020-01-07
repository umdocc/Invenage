# ------------------ function to support render_functions ----------------------

# return the latest incomplete pxk, if there is none, create a new one
get_current_pxk <- function(cofig_dict){
  admin_id <- config_dict$value[config_dict$name=='admin_id']
  if (length(admin_id)!=1){
    stop('Critical Error! admin_id not found')
  }
  conn <- db_open(config_dict)
  pxk_num_list <- dbGetQuery(conn,'select pxk_num from pxk_info')
  current_pxk <- dbGetQuery(conn,
                    'select pxk_num from pxk_info where completed = 0')
  dbDisconnect(conn)
  if (nrow(current_pxk)>0){
    newPXK = current_pxk$pxk_num[1]
  }else{
    currentDate <- strftime(Sys.time(),'%d%m%y')
    i <- 1;newPXKNum <- F
    while (!newPXKNum){
      tmp_num <- as.numeric(paste0(admin_id, currentDate,
                                  sprintf("%02d",i)))
      if (length(pxk_num_list[pxk_num_list$pxk_num==tmp_num,'pxk_num'])==0){
        newPXK <- tmp_num
        newPXKNum <- T
      }else{
        i <- i+1
      }
    }
  }
  return(newPXK)
}

# function to select customer using the database to look at PXK
get_cust_list <- function(config_dict,type){
  conn <- db_open(config_dict)
  pxk_info <- dbReadTable(conn,"pxk_info")
  customer_info <- dbReadTable(conn,"customer_info")
  dbDisconnect(conn)
  current_pxk <- pxk_info[pxk_info$completed==0,'pxk_num']
  # in type=inv_out, if current_pxk has completion code 
  # then we force customer_name
  if (length(current_pxk)>0 & type=='inv_out'){
    current_cust_id <- pxk_info$customer_id[pxk_info$pxk_num==current_pxk]
    cust_choice <- customer_info$customer_name[
      customer_info$customer_id==current_cust_id]
  }else{
    cust_choice <- customer_info$customer_name
  }
  return(cust_choice)
}

# the get_avail_lot function get a list of available lot, it returns a vector
# if sortType ='fifo', the earliest exp_date will be on top
get_avail_lot <- function(current_prod_code,config_dict,sort_type='fifo'){
  inventory <- update_inventory(config_dict)
  if (sort_type == 'fifo'){
    avail_lot <- inventory[inventory$prod_code==current_prod_code,]
    avail_lot <- avail_lot[order(avail_lot$intexp_date,
                                       na.last = F, # put NA lot first
                                       decreasing = F),] #lowest exp_date first
  }
  avail_lot <- avail_lot$lot
  return(avail_lot)
}

# function to build estimated import cost from import_log
get_est_import_cost <- function(import_log, algorithm='weighted_average'){
  if (algorithm=='weighted_average'){
    import_log <- convert_to_pack(import_log,packaging,stringSL='qty',
                               packString = 'pack_qty')
    import_log$pack_import_cost <-
      import_log$actual_unit_cost*import_log$units_per_pack
    import_log$total_import_cost <-
      import_log$pack_import_cost*import_log$pack_qty
    tmp <- import_log %>% group_by(prod_code,lot) %>%
      summarise(total_pack = sum(pack_qty),
                sum_import_cost = sum(total_import_cost)) %>%
      ungroup
    tmp$ave_pack_import_cost <- tmp$sum_import_cost/tmp$total_pack
    tmp <- tmp %>% select(prod_code,lot,ave_pack_import_cost)
    return(tmp)
  }
}

create_lookup_tbl <- function(table_name,config_dict,local_name=TRUE){
  #re-read the basic tables
  conn <- db_open(config_dict)
  sale_log <- dbReadTable(conn,"sale_log")
  product_info <- dbReadTable(conn,"product_info")
  import_log <- dbReadTable(conn,"import_log")
  import_price <- dbReadTable(conn,"import_price")
  currency <- dbReadTable(conn,"currency")
  packaging <- dbReadTable(conn,"packaging")
  dbDisconnect(conn)
  if (table_name=='inventory'){
    lookup_tbl_output <- update_inventory(config_dict)
    lookup_tbl_output$remaining_qty <- round(
      lookup_tbl_output$remaining_qty, digits=2)
    lookup_tbl_output$unit <- NULL
    ordering_unit <- get_ordering_unit(packaging)
    lookup_tbl_output <- merge(
      lookup_tbl_output, ordering_unit,all.x=T)
    lookup_tbl_output <- merge(
      lookup_tbl_output, product_info %>% 
        select(prod_code,name,vendor,ref_smn), all.x = T) %>%
      select(name, vendor, ref_smn, lot, exp_date, remaining_qty, unit)
  }
    # query on simple table
  if (table_name=='product_info'){
    lookup_tbl_output <- product_info %>% 
      select(prod_code, name, vendor, ref_smn)
  }
  if (table_name=='import_price'){
    lookup_tbl_output <- merge(import_price,product_info %>% 
                                 select(prod_code,name,vendor,ref_smn))
    lookup_tbl_output <- merge(lookup_tbl_output,currency)
    lookup_tbl_output <- lookup_tbl_output %>% 
      select(name,vendor,ref_smn,import_price,currency,min_order)
  }
  if (table_name=='sale_log'){
    lookup_tbl_output <- merge(sale_log, product_info %>% select(
      prod_code,name,vendor,ref_smn))
    lookup_tbl_output <- merge(lookup_tbl_output,pxk_info %>% select(
      pxk_num,customer_id,sale_datetime))
    lookup_tbl_output$customer_id <- as.numeric(
      lookup_tbl_output$customer_id)
    lookup_tbl_output <- merge(
      lookup_tbl_output,customer_info %>% select(customer_id,customer_name))
    lookup_tbl_output$sale_date <- gsub(
      ' .*$', '', lookup_tbl_output$sale_datetime)
    lookup_tbl_output <- lookup_tbl_output %>%
      select(pxk_num, sale_date, customer_name, name, ref_smn, qty, unit, 
             lot, note)
  }
  if (table_name=='import_log'){
    lookup_tbl_output <- merge(import_log, product_info%>% select(
      prod_code,name)) %>% 
      select(name,unit,qty,lot,exp_date,po_name,actual_unit_cost)
    
  }
  # format the table
  if (local_name){
    lookup_tbl_output <- translate_tbl_column(lookup_tbl_output,ui_elem)
  }
  return(lookup_tbl_output)
}

# get_latest_price is a function to get the last price sold to a customer
get_latest_price <- function(customer_id, prod_code, unit, pxk_info){
  sale_lookup <- merge(sale_log,pxk_info,on='pxk_num',all.x=T)
  latest_price <- -9999
  # filter through sale_lookup to find price
  tmp <- sale_lookup[sale_lookup$prod_code == prod_code & 
                       sale_lookup$customer_id == customer_id &
                       sale_lookup$unit == unit,]
  tmp <- tmp[!is.na(tmp$unit_price),]
  # if we can find something, update latest price
  if (nrow(tmp)>0){
    tmp <- merge(tmp,pxk_info %>% select(pxk_num,sale_datetime))
    if (class(tmp$sale_datetime) == "character"){
      tmp$sale_datetime <- strptime(tmp$sale_datetime,'%Y-%m-%d %H:%M:%S')
    }
    latest_price <- tmp$unit_price[
      tmp$sale_datetime == max(tmp$sale_datetime)]
  }
  return(latest_price)
}

get_current_pricelist <- function(
  sale_log, pxk_info, customer_info, product_info){
  tmp <- sale_log[!is.na(sale_log$unit_price),]
  tmp <- tmp[tmp$unit_price>0,]
  tmp <- merge(tmp,pxk_info %>% select(pxk_num,customer_id,sale_datetime))
  tmp$sale_datetime <- strptime(tmp$sale_datetime,'%Y-%m-%d %H:%M:%S')
  tmp$sale_datetime <- as.POSIXct(tmp$sale_datetime)
  tmp <- merge(tmp,customer_info %>% select(customer_id,customer_name))
  tmp <- tmp %>% group_by(customer_id, prod_code,unit) %>% mutate(
    latest_date=max(sale_datetime))
  tmp$latest_price <- tmp$sale_datetime==tmp$latest_date
  tmp <- tmp[tmp$latest_price,]
  tmp <- merge(tmp,product_info %>% select(prod_code,name,vendor,ref_smn))
  tmp <- tmp %>% 
    select(customer_name,name,ref_smn,unit,unit_price,latest_date) %>%
    arrange(customer_name,name)
  # write.xlsx(tmp,'~/Downloads/price_list.xlsx')
  return(tmp)
}

# a function to get the sales summary by prod_code
get_sales_summary <- function(config_dict,max_backdate=365){
  conn <- db_open(config_dict)
  tmp <- dbReadTable(conn,'sale_log')
  packaging <- dbReadTable(conn,'packaging')
  pxk_info <- dbReadTable(conn,'pxk_info')
  dbDisconnect(conn)
  tmp <- convert_to_pack(tmp,packaging,'qty','pack_qty')
  tmp <- merge(tmp, pxk_info %>% select(pxk_num,sale_datetime))
  min_date <- Sys.time() - as.difftime(max_backdate, unit = "days")
  tmp <- tmp[tmp$sale_datetime>min_date,]
  tmp <- tmp %>% group_by(prod_code) %>% summarise(
    oldest_sale_datetime = min(sale_datetime),total_sale_pack=sum(pack_qty)) %>%
    ungroup
  tmp$oldest_sale_datetime <- strptime(tmp$oldest_sale_datetime,
                                       "%Y-%m-%d %H:%M:%S")
  tmp$days_diff <- as.numeric(
    difftime(Sys.time(),tmp$oldest_sale_datetime,units = 'days'))
  tmp$mths_diff <- ceiling(tmp$days_diff/(365.25/12)) # round to month
  tmp$ave_mth_sale <- (tmp$total_sale_pack/tmp$mths_diff)
  return(tmp)
}

# a function to replace duplicated line with NA
clean_duplicates <- function(
  data_df, col_list = c('customer_name','sale_date','pxk_num')){
  i <- nrow(data_df)
  while (i>=2){
    if (data_df[(i-1),col_list]==data_df[i,col_list]){
      data_df[i,col_list] <- NA
    }
    i <- i-1
  }
  return(data_df)
}

# return the pxk data from pxk_num
render_selected_pxk <- function(selected_pxk_num,config_dict,localised=T){
  conn <- db_open(config_dict)
  sale_log <- dbReadTable(conn,'sale_log')
  pxk_info <- dbReadTable(conn,'pxk_info')
  dbDisconnect(conn)
  # selected_pxk_num <- 2012007
  output_pxk <- sale_log[sale_log$pxk_num==as.integer(selected_pxk_num),]

  output_pxk <- merge(output_pxk,product_info %>% select(prod_code,name))
  output_pxk <- merge(output_pxk,pxk_info)
  output_pxk <- merge(output_pxk,customer_info)

  
  output_pxk <- output_pxk %>% 
    select(stt,name,unit,unit_price,qty,lot,customer_name,note)
  output_pxk <- output_pxk[order(output_pxk$stt),] # sort by stt
  if (localised){
    ui_elem <- get_ui_elem(config_dict)
    # output_pxk <- localise_tbl(output_pxk,ui_elem)
    for (i in 1:length(output_pxk)){
      if (length(ui_elem$actual[ui_elem$label==names(output_pxk)[i]])==1){
        names(output_pxk)[i] = ui_elem$actual[
          ui_elem$label==names(output_pxk)[i]]
      }
    }
  }
  return(output_pxk)
}

# translate column
translate_tbl_column <- function(input_df,ui_elem){
  for (i in 1:length(input_df)){
    if (length(ui_elem$actual[ui_elem$label==names(input_df)[i]])==1){
      names(input_df)[i] = ui_elem$actual[
        ui_elem$label==names(input_df)[i]]
    }
  }
  return(input_df)
}

# reverse the translated column
rev_trans_tbl_column <- function(input_df,ui_elem){
  for (i in 1:length(input_df)){
    if (length(ui_elem$label[ui_elem$actual==names(input_df)[i]])==1){
      names(input_df)[i] = ui_elem$label[
        ui_elem$actual==names(input_df)[i]]
    }
  }
  return(input_df)
}

get_pxk_entry_num <- function(selected_pxk_num,config_dict){
  conn <- db_open(config_dict)
  sale_log <- dbReadTable(conn,'sale_log')
  dbDisconnect(conn)
  output_pxk <- sale_log[sale_log$pxk_num==selected_pxk_num,]
  entry_list <- output_pxk$stt
  return(entry_list)
}

# a function to delete certain stt on pxk, or if stt = 'all' will delete all
delete_pxk <- function(pxk_num,stt,config_dict){
  if (stt=='all'){
    query <- paste0('delete from sale_log where pxk_num = ',
                    pxk_num)
  }else{
    query <- paste0('delete from sale_log where pxk_num = ',
                    pxk_num,' and stt = ',stt)
  }
  conn = db_open(config_dict)
  res <- dbExecute(conn,query)
  dbDisconnect(conn)
}

# function to check if an inv_out entry should be allowed before writing to db
check_inv_out <- function(append_sale_log, config_dict){
  curent_prodcode <- as.character(append_sale_log$prod_code[1])
  current_lot <- as.character(append_sale_log$lot[1])
  tmp <- convert_to_pack(append_sale_log,packaging,'qty','pack_qty')
  sale_qty <- tmp$pack_qty
  inventory <- update_inventory(config_dict)
  inv_remain <- inventory$remaining_qty[inventory$prod_code == curent_prodcode &
                                          inventory$lot == current_lot][1]

  if (is.na(inv_remain)){inv_remain <- 0}
  inv_out_ok <- (inv_remain>=sale_qty)
  return(inv_out_ok)
}

# create a list of ordering_unit based on packaging
get_ordering_unit <- function(packaging){
  ordering_unit <- packaging[
    packaging$units_per_pack==1, c('prod_code', 'unit')]
  ordering_unit <- ordering_unit[ordering_unit$unit!='pack',]
  ordering_unit <- ordering_unit[!duplicated(ordering_unit$prod_code),]
  return(ordering_unit)
}

# check the status of pxk_num
check_pxk_num <- function(selected_pxk_num,config_dict){
  conn <- db_open(config_dict)
  pxk_info <- dbReadTable(conn,"pxk_info")
  dbDisconnect(conn)
  pxk_status <- 'completed'
  complete_code <- pxk_info$completed[pxk_info$pxk_num==selected_pxk_num]
  if (length(complete_code)==0){
    pxk_status <- 'new'
  }else{
    if (complete_code==0){
      pxk_status <- 'in_progress'
    }
  }
  return(pxk_status)
}

get_latest_unit <- function(customer_id, prod_code, sale_log,pxk_info){
  sale_lookup <- merge(sale_log,pxk_info,on='pxk_num',all.x=T)
  # filter through sale_lookup to find price
  tmp <- sale_lookup[sale_lookup$prod_code == prod_code & 
                       sale_lookup$customer_id == customer_id,]
  tmp <- tmp[!is.na(tmp$sale_datetime),]
  # if we can find something, update priority unit
  if (nrow(tmp)>0){
    tmp <- tmp %>% select(pxk_num, unit, sale_datetime)
    if (class(tmp$sale_datetime) == "character"){
      tmp$sale_datetime <- strptime(tmp$sale_datetime,'%Y-%m-%d %H:%M:%S')
    }
    latest_unit <- tmp$unit[
      tmp$sale_datetime == max(tmp$sale_datetime)]
    return(latest_unit)
  }else{
    return(NULL)
  }
  
}

# function to edit pxk using pxk_num and 'cell' value
edit_db_pxk <- function(cell,pxk_num){
  #use render_selected_pxk maintain consistency with ui
  updated_pxk <- render_selected_pxk(pxk_num,config_dict)
  # reverse the column name
  updated_pxk <- rev_trans_tbl_column(updated_pxk, ui_elem)
  edited_stt <- updated_pxk$stt[cell$row] # get the edit row stt

  # read the pxk from database
  conn = db_open(config_dict)
  tmp <- dbGetQuery(
    conn,paste('select * from sale_log where pxk_num =',pxk_num))
  dbDisconnect(conn)

  pxk_col_list <- c( # list of pxk fields
    'stt', 'name', 'unit', 'unit_price', 'qty', 'lot', 'customer_name', 'note')
  allowed_chr_fields <- c(3,4,5,6,8) # allow editing certain fields only
  if (cell$col %in% (allowed_chr_fields-1) ){ # dt cell has offset of 1
    tmp[tmp$stt==edited_stt,
        pxk_col_list[cell$col+1]] <- as.character(cell$value)
  }
  # check data
  tmp$unit <- tolower(tmp$unit) # clean the unit first
  errorsFree=T
  if (pxk_col_list[cell$col+1]=='unit'){ #check the unit
    test <- merge(tmp,packaging)
    if(length(test$units_per_pack[test$stt==edited_stt])==0){
      errorsFree = F
      # do something to alert user here
    }
  }
  if (errorsFree){
    conn = db_open(config_dict)
    dbExecute(
      conn, paste('delete from sale_log where pxk_num =', pxk_num))
    dbWriteTable(conn,'sale_log',tmp,append=T)
    dbDisconnect(conn)
  }
  return(errorsFree)
}

# this function create an excel PXK from a given pxk_num
create_pxk_file <- function(pxk_num){
  # create new PXK file
  orig_path <- config_dict$value[config_dict$name=='pxk_form']
  dest_path <- file.path(config_dict$value[config_dict$name=='pxk_out_path'],
                         paste0(company_name,".PXK.",
                                pxk_num,".xlsx"))
  wb <- loadWorkbook(orig_path)
  
  # get the expDate, if a Lot has 2 expDate, select only the 1st
  # need to get all items, not just positive ones
  tmp <- update_inventory(config_dict,pos_item=FALSE)
  exp_date <- tmp %>% select(prod_code,lot,exp_date) %>% unique()
  exp_date <- exp_date[!duplicated(exp_date$lot),]
  
  # read the data
  conn <- db_open(config_dict)
  # current_pxk_info
  query <- paste("SELECT * from pxk_info where pxk_num =",pxk_num)
  current_pxk_info <- dbGetQuery(conn,query)
  payment_type <- dbReadTable(conn,'payment_type')
  current_pxk_info <- merge(current_pxk_info,payment_type)
  current_pxk_info <- merge(
    current_pxk_info,ui_elem, by.x='payment_label', by.y='label')
  
  # form_data
  query <- paste("SELECT sale_log.stt, product_info.name, product_info.ref_smn,
                   sale_log.unit, sale_log.unit_price,
                   sale_log.qty,sale_log.lot
                   FROM   sale_log INNER JOIN product_info
                   ON     sale_log.prod_code = product_info.prod_code
                   WHERE  sale_log.pxk_num =",pxk_num)
  
  form_data <- dbGetQuery(conn,query)
  form_data <- merge(form_data,exp_date,all.x=T)
  
  # calculate total price
  form_data$total_price <- form_data$unit_price*form_data$qty
  
  # get customer data
  query <- paste("SELECT DISTINCT customer_info.customer_name
                    FROM pxk_info INNER JOIN customer_info
                    ON pxk_info.customer_id = customer_info.customer_id
                    WHERE pxk_info.PXK_num =", pxk_num)
  printingCustomerName <- dbGetQuery(conn,query)
  printingCustomerName <- printingCustomerName$customer_name[1]
  
  output_info <- dbGetQuery(
    conn,'select * from output_info where type = "pxk_output"')
  dbDisconnect(conn)
  
  # writing customer_name
  customerNameRow <- as.numeric(
    output_info$value[output_info$name=='customerNameRow'])
  customerNameCol <- as.numeric(
    output_info$value[output_info$name=='customerNameCol'])
  
  writeData(wb,sheet=1,printingCustomerName, startRow=customerNameRow, 
            startCol=customerNameCol, colNames = F)
  
  # writing pxkNum
  pxkNumRow <- as.numeric(output_info$value[output_info$name=='pxkNumRow'])
  pxkNumCol <- as.numeric(output_info$value[output_info$name=='pxkNumCol'])
  writeData(wb,sheet=1,pxk_num,startRow=pxkNumRow, 
            startCol=pxkNumCol, colNames = F)
  
  # writing current date
  date_row <- as.numeric(output_info$value[output_info$name=='date_row'])
  date_col <- as.numeric(output_info$value[output_info$name=='date_col'])
  writeData(wb, sheet=1, Sys.Date(), startRow=date_row, startCol=date_col, 
            colNames = F)
  
  # writing payment type
  out_payment_type <- current_pxk_info$actual
  payment_type_row <- as.numeric(
    output_info$value[output_info$name == 'payment_type_row'])
  payment_type_col <- as.numeric(
    output_info$value[output_info$name == 'payment_type_col'])
  writeData(wb, sheet=1, out_payment_type, startRow=payment_type_row, 
            startCol=payment_type_col, colNames = F)    
  
  # get pxkDataHeaders
  pxkDataHeaders <-  data.frame(matrix(unlist(strsplit(
    output_info$value[output_info$name=='dataHeaders'],';')),nrow=1))
  
  # rearrange Data and write
  form_data <- form_data[order(as.numeric(form_data$stt)),]
  dataColumns <- unlist(strsplit(
    output_info$value[output_info$name=='dataToWrite'],';'))
  
  form_data <- form_data[,dataColumns]
  
  
  # write both data and headers
  dataStartRow <- as.numeric(
    output_info$value[output_info$name=='dataStartRow'])
  dataStartCol <- as.numeric(
    output_info$value[output_info$name=='dataStartCol'])
  #write headers first
  writeData(wb,sheet=1,pxkDataHeaders, startRow=dataStartRow,
            startCol=dataStartCol, colNames=F)
  # data is one row below
  writeData(wb,sheet=1,form_data,startRow=dataStartRow+1,
            startCol=dataStartCol, colNames=F)
  # save the excel sheet
  saveWorkbook(wb,dest_path,overwrite = T)
  # return the filename
  return(dest_path)
}