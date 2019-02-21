# Manella R helper, which contains function used in R reports
# try to build functions that display things only
# data cleaning should be done in python
require(dplyr)

# convert to Box is the most used function of all time
convertToBox <- function(amtFrame,packaging,stringSL){
  amtFrame <- merge(amtFrame,packaging %>% select(prodCode,Unit,unitsPerPack),
                     all.x=T, by = c('prodCode','Unit'))
  amtFrame[,paste0(stringSL,'Pack')] <- as.numeric(amtFrame[,stringSL])/
    as.numeric(amtFrame$unitsPerPack)
  return(amtFrame)
}

# convertToVND take a similar approach
convertToVND <- function(priceFrame,exchangeRate,stringPrice,stringCurrency){
  priceFrame <- merge(priceFrame,exchangeRate,
                    all.x=T, by.x = stringCurrency, by.y = 'Currency')
  priceFrame[,paste0(stringPrice,'VND')] <- 
    as.numeric(priceFrame[,stringPrice])*as.numeric(priceFrame$exchangeRate)

  return(priceFrame)
}

# clean tenderDetails
cleanTenderDetails <- function(tenderDetails,packaging){
  # clean tenderDetails in term of box and box price
  tenderDetails <- tenderDetails[!grepl('nan',tenderDetails$prodCode),]
  tenderDetails <- tenderDetails[!duplicated(tenderDetails$prodCode),]
  tenderDetails <- merge(tenderDetails, packaging %>% select(
    prodCode,Unit,unitsPerPack),all.x=T)
  tenderDetails <- tenderDetails[!duplicated(tenderDetails),]
  tenderDetails$packAmt <- as.numeric(tenderDetails$Amount)/as.numeric(
    tenderDetails$unitsPerPack)
  tenderDetails$packPrice <- as.numeric(tenderDetails$unitPrice)*
    as.numeric(tenderDetails$unitsPerPack)
  return(tenderDetails)
}

# format with commas, stringsToFormat is a vector containing all columns name 
# to be formatted
formatCommas <- function(outputData,stringsToFormat){
  for (colName in stringsToFormat){
    outputData[,colName] <- formatC(outputData[,colName],
                                    format="d", big.mark=",")
  }
  return(outputData)
}