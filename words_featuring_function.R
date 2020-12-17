## words featuring function. It returns a 0-1 matrix
## df           : a df created by rbind_data function
## feature.word : word vector that contains the words we want to feature.
words_featuring <- function(df, feature.word){
  
                                 row_n <- dim(df)[1]
                                 feature_matrix <- matrix(NA, nrow = row_n, ncol = length(feature.word))
                                 colnames(feature_matrix) <- feature.word

                                 jieba.engine <- worker()
                                 new_user_word(jieba.engine,feature.word)
                                 for(j in 1:row_n){
                                      for(i in 1:length(feature.word)){
                                            if(sum(feature.word[i] == segment(df$TITLE[j], jieba.engine)) != 0){
                                                  feature_matrix[j,i]  <- 1
                                            }else{
                                                  feature_matrix[j,i]  <- 0
                                            }
                                          }
                                        }
                                 return(feature_matrix)
                                 }












