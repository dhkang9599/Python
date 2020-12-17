


## filtering the item's url_id and title which contain "+" or "*"
## to find out the set item.(combination)
## need jiebaR library

filter_combination <- function(df, return.csv = F){
  
                                            return.vector.url   <- c()
                                            return.vector.title <- c()
  
                                            row_n <- dim(df)[1]
                                            gg <- worker(symbol = T)
  
                                            for(i in 1:row_n){
    
                                            x <- str_detect(segment(df$TITLE[i], gg), pattern = "\\+")  #df is a df created by rbind_data function
                                            y <- str_detect(segment(df$TITLE[i], gg), pattern = "\\*")   #rbind_data function : github
    
                                              if(sum(x | y) != 0) {
                                                     return.vector.url[i]   <- segment(df$URL[i], gg)[17]
                                                     return.vector.title[i] <- df$TITLE[i]
                                                     }
                                            rm(x)
                                            rm(y)
                                            }
                                            return.vector.url   <- return.vector.url[!is.na(return.vector.url)]
                                            return.vector.title <- return.vector.title[!is.na(return.vector.title)]
  
                                            return.matrix.url   <- (as.matrix(return.vector.url, ncol = 1))
                                            return.matrix.title <- (as.matrix(return.vector.title, ncol = 1))
                                            return.df           <- data.frame(return.matrix.title, return.matrix.url) 
                                            colnames(return.df) <- c("title", "url.id")
                                            return(return.df)
  
                                              if(return.csv == return.csv){
                                                     write.csv(return.matrix, "return_matrix.csv")
                                                     print(paste("csv file has been saved in", getwd()))
                                                    }
                                            }



## example : filter_combination(df = mois_11, return.csv = F)









