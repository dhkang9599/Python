filtering_keywords_grepl <- function(df,key.words,contain = F){                  
  
  row_n <- dim(df)[1]
  filtering_vector <- c()
  for(j in 1:row_n){
    
    y.bool.matrix  <- matrix(NA, 
                             nrow = length(key.words), 
                             ncol = 1)
    
    for(i in 1:length(key.words)){
      y.bool.matrix[i,]  <- grepl(x = df$TITLE[j], pattern = key.words[i])
    }
    if(contain){             
      if(sum(y.bool.matrix) != 0){
        filtering_vector[j] <- j
      }else{
        filtering_vector[j]   <- NA
      }
    }else{               
      if(sum(y.bool.matrix) == 0){
        filtering_vector[j] <- j 
      }
      else{
        filtering_vector[j]   <- NA
      }
    }
    rm(y.bool.matrix)
  }
  filtering_index <- filtering_vector[!is.na(filtering_vector)]
  return(df[filtering_index,])
}





























