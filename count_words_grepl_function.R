## count words function

# df             : is a df that created by rbind_data function
# keywords.vector: is the keywords.vector that makes sure the words we are interested in can be segemented accurately.
# top            : if top = 20, then it returns top 20 words, otherwise it returns full matrix.
#only.keywords   : if only.keywords = T, then it only returns the counted matrix of the words in keywords.vecotors.
count_words_grepl <- function(df, keywords.vector, sort = F){
  count_matrix <- matrix(NA, nrow = 1, ncol = length(keywords.vector))
  colnames(count_matrix) <- keywords.vector
  
  for(i in 1:length(keywords.vector)){
    count_matrix[i] <- dim(filtering_keywords_grepl(df = df, key.words = keywords.vector[i], contain = T))[1]
 }
 
  if(sort == T){
    return(sort(count_matrix, decreasing = T))
  }else{
    return(count_matrix)
  }
 
}





















