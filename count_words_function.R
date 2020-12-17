## count words function

# df             : is a df that created by rbind_data function
# keywords.vector: is the keywords.vector that makes sure the words we are interested in can be segemented accurately.
# top            : if top = 20, then it returns top 20 words, otherwise it returns full matrix.
#only.keywords   : if only.keywords = T, then it only returns the counted matrix of the words in keywords.vecotors.
count_words <- function(df, keywords.vector, top = NULL, only.keywords = F){
  
                          jieba.engine <- worker()
                          new_user_word(jieba.engine, keywords.vector)
                          x       <- segment(df$TITLE,jieba.engine)
                          x.table <- sort(table(x),T)
                          if(length(top) != 0){
                                count_df <- x.table[1:top]
                          }else{
                                count_df <- x.table
                          }
                          if(only.keywords == F){
                                return(count_df)
                          }else{
                                only.key.matrix <- matrix(NA, nrow = 1, ncol = length(keywords.vector))
                                colnames(only.key.matrix) <- keywords.vector
                             for(i in 1:length(keywords.vector)){
                               
                                only.key.matrix[1,i] <- count_df[keywords.vector[i]]
                                
                             }
                              return(only.key.matrix)
                          }
                          
                      }




























