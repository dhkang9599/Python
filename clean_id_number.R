#去掉已有category id 数字的函数


 clean_id_number<- function(taobao_data, existing_category_data){
  
  
                                   colnames(existing_category_data) <- c("id", "name")
                                   z         <- nchar(existing_category_data$id[1]) #提取该数字的长度
                                   first_num <- substr(existing_category_data$id[1],2,z)
                                   existing_category_data$id[1] <- as.numeric(first_num)


                                   
                                   
                                   
                                   
                                   if(unique(is.na(existing_category_data$id)) == F){
                                      print("existing_category_data: No NA in id column")
                                   }else{
                                      print("Something is wrong in existing_category_data")
                                   }
                                   
                                   if(unique(is.na(taobao_data$category_id)) == F){
                                      print("taobao_data: No NA in id column")
                                   }else{
                                      print("Something is wrong in taobao_data")
                                   }

                                   selected_data <- taobao_data
                                   for (i in 1:length(existing_category_data$id)){
                                        selected_data <- selected_data[existing_category_data$id[i] != selected_data$category_id,]
                                   }
                                   
                                   raw_index <- rownames(selected_data)
                                   selected_data$raw_index <- raw_index 
                                   selected_data_index <- (1:dim(selected_data)[1])
                                   rownames(selected_data) <-selected_data_index
                                   
                                   return(selected_data)
                                }




select_try <- clean_id_number(taobao_data = taobao_data, existing_category_data = exc_number)

dim(select_try)
select_try




clean_id_number(taobao_data = select_try, existing_category_data = traditional_number)













