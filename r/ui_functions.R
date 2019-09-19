# All functions shared by the UIs
# call all required packages
# ------------------------------- Base functions -------------------------------
# the db_open create the appropriate connection for Invenage
db_open <- function(config_dict){
  db_type <- config_dict$value[config_dict$name=='db_type']
  if (db_type == 'SQLite'){
    database_path <- config_dict$value[config_dict$name=='db_file']
    sqlite.driver <- dbDriver("SQLite")
    conn <- dbConnect(sqlite.driver, dbname = database_path)
    return(conn)
  }
}

# get current_pxk is a function that use the database connection object conn
get_current_pxk <- function(cofig_dict){
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
      tmp_num <- as.numeric(paste0(strftime(Sys.time(),'%d%m%y'),
                                  sprintf("%02d",i)))
      # print(tmpNum)
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

# function to rebuild the Inventory table from import_log and sale_log
# if moreThanZero ==T, it will only return items with positive stock
update_inventory <- function(config_dict, pos_item=TRUE){
  
  conn <- db_open(config_dict)
  import_log <- dbReadTable(conn,"import_log")
  sale_log <- dbReadTable(conn,"sale_log")
  dbDisconnect(conn)
  
  tmp <- import_log %>% select(prod_code,unit,qty,lot,exp_date)
  tmp <- convert_to_pack(tmp,packaging,'qty','importQty')
  tmp <- tmp %>% group_by(prod_code,unit,lot) %>% 
    summarise(totalImportQty = sum(importQty)) %>% ungroup()
  tmp2 <- sale_log %>% select(prod_code,unit,qty,lot)
  tmp2 <- convert_to_pack(tmp2,packaging,'qty','saleQty')
  tmp2 <- tmp2 %>% group_by(prod_code,unit,lot) %>% 
    summarise(totalSaleQty = sum(saleQty)) %>% ungroup()
  totalInventory <- merge(tmp,tmp2,all=T,by=c('prod_code','unit','lot'))
  totalInventory$totalSaleQty[is.na(totalInventory$totalSaleQty)] <- 0
  totalInventory$totalImportQty[is.na(totalInventory$totalImportQty)] <- 0
  totalInventory$remaining_qty <- totalInventory$totalImportQty - 
    totalInventory$totalSaleQty
  
  # keep only the available items
  if (pos_item){
    threshold <- 0.01
    totalInventory <- totalInventory[totalInventory$remaining_qty>threshold,] %>% 
      distinct()
  }
  # recover the exp_date
  exp_dateData <- import_log[!duplicated(import_log[c('prod_code','lot')]),] %>% 
    select(prod_code,lot,exp_date) %>% distinct()
  
  # merge, distinct and remove NA
  totalInventory <- merge(totalInventory,exp_dateData,all.x=T) %>% distinct()
  totalInventory <- totalInventory[!is.na(totalInventory$prod_code),]
  
  # calculate the intexp_date, which is the exp_date in standard format
  totalInventory$exp_date <- gsub('/','-',totalInventory$exp_date)
  totalInventory$exp_date <- gsub(' .*$','',totalInventory$exp_date)
  totalInventory$intexp_date <- parse_date_time(
    totalInventory$exp_date,c('%Y-%m','%m-%Y','%d-%m-%Y','%Y-%m-%d'))
  
  return(totalInventory)
}

# convertToPack is a critical function
convert_to_pack <- function(inputDF,packaging,stringSL,packString){
  inputDF <- merge(
    inputDF,packaging %>% select(prod_code,unit,units_per_pack),all.x=T)
  # check integrity
  if(nrow(inputDF[is.na(inputDF$units_per_pack),])>0){
    print(inputDF[is.na(inputDF$units_per_pack),])
    stop('inputDF contains unrecognised packaging')
  }
  inputDF[[packString]] <- inputDF[[stringSL]]/inputDF$units_per_pack
  # clean up
  inputDF$unit <- 'pack'
  inputDF[[stringSL]] <- NULL
  # inputDF$units_per_pack <- NULL
  return(inputDF)
}

# function to select customer using the database to look at PXK
get_cust_list <- function(config_dict){
  conn <- db_open(config_dict)
  pxk_info <- dbReadTable(conn,"pxk_info")
  customer_info <- dbReadTable(conn,"customer_info")
  dbDisconnect(conn)
  current_pxk <- pxk_info[pxk_info$completed==0,'pxk_num']
  # if current_pxk has completion code then we force customer_name
  if (length(current_pxk)>0){
    current_cust_id <- pxk_info$customer_id[pxk_info$pxk_num==current_pxk]
    cust_choice <- customer_info$customer_name[
      customer_info$customer_id==current_cust_id]
  }else{
    cust_choice <- customer_info$customer_name
  }
  return(cust_choice)
}

# the getAvailablelot function get a list of available lot, it returns a vector
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

# function to rebuild the productInfo HTML string
build_prod_info <- function(config_dict,input){
  conn <- db_open(config_dict)
  product_info <- dbReadTable(conn,"product_info")
  dbDisconnect(conn)
  inventory <- update_inventory(config_dict)
  
  current_select <- product_info[product_info$name==input$prodNameSelector,]
  # current_ref <- product_info[
  #   product_info$name==input$prodNameSelector, "ref_smn"]
  # current_prod_code <- product_info[
  #   product_info$name==input$prodNameSelector, "prod_code"]
  # currentNSX <- productInfo[productInfo$Name==input$prodNameSelector, "NSX"]
  total_available <- inventory[inventory$prod_code == current_select$prod_code &
                            inventory$lot == input$lotSelector, 'remaining_qty']
  current_exp_date <- inventory[
    inventory$prod_code == current_select$prod_code &
                      inventory$lot == input$lotSelector, 'exp_date']
  packaging_str <- packaging[
    packaging$prod_code == current_select$prod_code &
      packaging$unit == input$unitSelector,]
  packaging_str <- paste0(packaging_str$units_per_pack[1],
                          packaging_str$unit[1],'/pack')
  return(paste("REF: ",current_select$ref_smn,'<br/>',
               ui_elem$actual[ui_elem$label=='prod_code'],':',
               current_select$prod_code, '<br/>',
               ui_elem$actual[ui_elem$label=='vendor'],':',
               current_select$vendor, '<br/>',
               ui_elem$actual[ui_elem$label=='exp_date'],':',
               current_exp_date, '<br/>',
               ui_elem$actual[ui_elem$label=='total_available'],':',
               total_available, '<br/>',
               ui_elem$actual[ui_elem$label=='packaging_str'],
               ':',packaging_str)
  )
}


# 
# # function to render current PXK as an integer
# getCurrentPXK <- function(conn){
#   pxk_info <- dbReadTable(conn,"pxk_info")
#   current_pxk <- pxk_info[pxk_info$completionCode==0,'pxk_num']
#   if (length(current_pxk)>0){
#     current_pxk = as.integer(unique(current_pxk))
#   }else{
#     currentDate <- strftime(Sys.time(),'%d%m%y')
#     i <- 1;newPXKNum <- F
#     while (!newPXKNum){
#       # for a given day, increase last 2 digit until we cannot find a match
#       tmpNum = as.integer(
#         paste0(strftime(Sys.time(),'%d%m%y'),sprintf("%02d",i)))
#       if (length(pxk_info[pxk_info$PXKNum==tmpNum,'PXKNum'])==0){
#         current_pxk <- tmpNum
#         newPXKNum <- T
#       }else{
#         i <- i+1
#       }
#     }
#   }
#   return(current_pxk)
# }
# 
# # render raw pxk into readable format
# renderPXK <- function(current_pxk){
#   query <- paste("select sale_log.Stt, productInfo.Name, sale_log.unit, 
#                 sale_log.unitPrice,sale_log.qty, sale_log.lot,
#                 sale_log.PXKNum, sale_log.Note 
#                 from sale_log inner join productInfo
#                  on sale_log.prod_code = productInfo.prod_code 
#                  where sale_log.PXKNum =",
#                  current_pxk)
#   conn <- db_open(config_dict)
#   outTable <- dbGetQuery(conn,query)
#   dbDisconnect(conn)
#   return(outTable)
# }
# 
# # function to build estimated import cost from import_log
# getEstImportCost <- function(import_log, algorithm='weighted_average'){
#   if (algorithm=='weighted_average'){
#     import_log <- convertToPack(import_log,packaging,stringSL='qty',
#                                packString = 'packQty')
#     import_log$packImportCost <- 
#       import_log$actualunitImportCost*import_log$units_per_pack
#     import_log$totalImportCost <- 
#       import_log$packImportCost*import_log$packQty
#     tmp <- import_log %>% group_by(prod_code,lot) %>%
#       summarise(totalPack = sum(packQty),
#                 sumImportCost = sum(totalImportCost)) %>%
#       ungroup
#     tmp$avePackImportCost <- tmp$sumImportCost/tmp$totalPack
#     tmp <- tmp %>% select(prod_code,lot,avePackImportCost)
#     return(tmp)
#   }
# }
# 
# buildCompletePath <- function(pathString,sep=';'){
#   pathString <-unlist(strsplit(pathString,split = ';'))
#   for (i in c(1:length(pathString))){
#     if (i==1){
#       fullPath <- pathString[i]
#     }else{
#       fullPath <- file.path(fullPath,pathString[i])
#     }
#   }
#   return(fullPath)
# }

# # roll back x months from current month, not counting current month,
# # return the beginning date of the rolled back month
# roll_back_date <- function(rolling_mth){
#   beginDate <- as.Date(as.character(cut(Sys.Date(), "month")),'%Y-%m-%d')
#   backDate <- as.Date(as.character(cut(beginDate - 28*rolling_mth,'month')))
#   return(backDate)
# }