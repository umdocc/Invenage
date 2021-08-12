convert_to_pack <- function(inputDF,packaging,stringSL,packString){
  inputDF <- merge(
    inputDF,packaging %>% select(prod_code,unit,units_per_pack),
    by = c('prod_code','unit'),
    all.x=T)
  
  # check integrity
  if(nrow(inputDF[is.na(inputDF$units_per_pack),])>0){
    print(inputDF[is.na(inputDF$units_per_pack),])
    stop('inputDF contains unrecognised packaging')
  }
  
  inputDF[[packString]] <- as.numeric(inputDF[[stringSL]])/as.numeric(
    inputDF$units_per_pack)
  # clean up
  inputDF$unit <- 'pack'
  inputDF[[stringSL]] <- NULL
  
  return(inputDF)
}