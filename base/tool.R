# function to replace a pattern, default replacement to "", use with mapply
rep_vector <- function(pattern_col,target_col,replacement=""){
  gsub(pattern_col,replacement,target_col)
}