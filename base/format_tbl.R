convert_to_pack <- function(inputDF,packaging,stringSL,packString,write_error=F){
  inputDF <- merge(
    inputDF,packaging %>% select(prod_code,unit,units_per_pack),
    by = c('prod_code','unit'),
    all.x=T)
  
  # check integrity
  if(nrow(inputDF[is.na(inputDF$units_per_pack),])>0){
    error_df <- inputDF[is.na(inputDF$units_per_pack),]
    if(write_error){
      write.xlsx(error_df, "~/Downloads/pkg_error.xlsx")
    }else{
      print(error_df)
    }
    stop('inputDF contains unrecognised packaging')
  }
  
  inputDF[[packString]] <- as.numeric(inputDF[[stringSL]])/as.numeric(
    inputDF$units_per_pack)
  # clean up
  inputDF$unit <- 'pack'
  inputDF[[stringSL]] <- NULL
  
  return(inputDF)
}