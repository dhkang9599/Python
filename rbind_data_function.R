#   this function is for the situation that when I download more than one csv/xlsx files. This function can help me rowbind the 
# files together one time.

## path: its the path that contain all the csv/xlsx files.
## type: its only for xlsx or csv type for now.

rbind_data <- function(path, type){
  pattern          <- paste0("*.", type)
  target_file      <- list.files(path = path, pattern = pattern) # get all docs names in "path" which are formed as "parttern"
  file_list        <- vector(mode = "list", length = length(target_file))   # creat a list to save each doc in "path"
  names(file_list) <- sub(pattern, target_file, replacement = "")    # name each element in file_list as read doc names(exclue "parttern")
  
  if(type == "xlsx"){
    for(i in 1:length(target_file)){
      setwd(path)
      file_list[[i]] <- readxl::read_xlsx(target_file[i])       # load every xlsx type doc data in path into file_list  
    }
  } else if(type == "csv"){
    for(i in 1:length(target_file)){
      file_list[[i]] <- read.csv(file = target_file[i], header = T, sep = ",",encoding = "UTF-8")
    }                                                          # load every csv type doc data in path into file_list 
  } else {
    print("not ready for this type yet")
  }
  
  file_data <- file_list[[1]]                                  # load first element(file) in file list to bind all docs together
  
  for(i in 2:length(target_file)){
    file_data <- rbind(file_data, file_list[[i]])            # bind them 
  }
  
  return(file_data)                                            # return binded data
  
}

#example:
rbind_data(path = "C:/Users/Daniel Kang/Desktop/processing/hou/file7", type = "xlsx")

#output is a dataframe that contain all xlsx or csv files together in a path. 
