###############
## Contains my own created functions
# Name: Aidan Patt
# Mon Mar 17 22:33:54 2025 ------------------------------


# Need a function to remove all columns that only have one unique answer
drop_one_val_col = function(x){
  
  # getting number of columns of data set
  ncol = dim(x)[2]
  # creating a list to put in names of columns to eliminate
  cols_to_elim = c()
  
  # collect all column names to be eliminated
  for(i in 1:ncol){
    
    
 
    if(is_tibble(x) == TRUE){
      num_uvals = nrow(unique(x[,i]))
    }
    else{
      num_uvals = length(unique(x[,i]))
    }
    if(num_uvals == 1){
      cols_to_elim = c(cols_to_elim, colnames(x)[i])
    }
  }
  
  # print(cols_to_elim)
  # used during report but commented out so not constantly listing columns
  # that are deleted
  
  # get rid of columns from the data set
  t = x |> select(!all_of(cols_to_elim))
  return(t)
  
}

# drop_one_value_col <- function(df, prt_val = FALSE){ 
#   # browser()
#   df_id <- ensym(df)
#   if(prt_val){
#     msg = paste("Looking for single value columns in data frame: ",as.character(df_id) )
#     print(msg)}
#   ## takes whole dataframe
#   dropc <- NULL
#   val <- NULL
#   ## test each column for a single value
#   for(i in 1:dim(df)[2]){   
#     if(dim(distinct(df[,i]))[1] == 1){
#       dropc <- c(dropc, i)
#       val <- c(val, df[1,i])
#     }
#   } 
#   
#   if(prt_val){
#     if(is.null(dropc)){
#       print("No columns dropped")
#       return(df)}else{
#         print("Columns dropped:")
#         # print(colnames(df)[drop])
#         print(unlist(val))
#         df <- df[, -1*dropc]
#         return(df)
#       }
#   }
#   df <- df[, -1*dropc]
#   return(df)
# }








# Shift loc function for moving data from one column to an adjacent column

shift_loc <- function(df, col_name, dat_name, num_col, num_shift){
  # browser()
  col_num = which(colnames(df) == col_name)
  row_num = which(df[,col_num] == dat_name)  ## calcs a vector of rows
  
  for(k in 1:length(row_num)){
    d = rep(0,num_col) ## storage for items to be moved
    for(i in 1:num_col){
      d[i] = df[row_num[k], col_num + i - 1]
    }
    for(i in 1:num_col){
      ra = row_num[k]
      cb = col_num + i - 1
      df[ra, cb] <-  NA
    }
    for(j in 1:num_col){
      rc = row_num[k]
      cd = col_num + j - 1 + num_shift
      df[rc, cd] = d[j]
    }
  }
  # sprintf("Rows adjusted:")
  # print("%d",row_num)
  return(df)
}

# function to truncate values


