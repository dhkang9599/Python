## delete items(rows) that contain some keywords that have been specified. 
## jiebaR need to be loaded already
filtering_keywords <- function(df,key.words,contain = F){                  #df         : a df that created using rbind_data function
                                                                           #key.words  : a key word vector
                                                                           #contain = F: return a df that items do not contain any of the key words.
                                   jieba.engine <- worker()
                                   new_user_word(jieba.engine, key.words)     # tell jieba worker() the key words that we want.
  
                                   row_n <- dim(df)[1]              
                                   segmented.list  <- vector("list", length = row_n)
                                   for(i in 1:row_n){
                                         segmented.list[[i]] <- segment(df$TITLE[i], jieba.engine)
                                   }
  
                                   filtering_vector <- c()
                                   for(j in 1:row_n){
    
                                      y.bool.matrix  <- matrix(NA, 
                                                               nrow = length(key.words), 
                                                               ncol = length(segmented.list[[j]]))
    
                                         for(i in 1:length(key.words)){
                                             y.bool.matrix[i,]  <- (segmented.list[[j]] == key.words[i])
                                         }
                                      if(contain){             # contain == T: return a df that items contain at least one of the key words.
                                         if(sum(y.bool.matrix) != 0){
                                             filtering_vector[j] <- j
                                           }
                                          }else{               # contain == F: return a df that items do not contain any of the key words.
                                         if(sum(y.bool.matrix) == 0){
                                             filtering_vector[j] <- j 
                                           } 
                                          }
                                    rm(y.bool.matrix)
                                   }
                                   filtering_index <- filtering_vector[!is.na(filtering_vector)]
                                   return(df[filtering_index,])
                                 }

