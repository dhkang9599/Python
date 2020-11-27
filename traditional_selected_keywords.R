library(rJava)
library(Rwordseg)
library(NLP)
library(wordcloud2)
library(RColorBrewer)
library(wordcloud)
library(jiebaR)
library(data.table)
#添加新的id


taobao_data <- read.csv(file = "C:/Users/Daniel Kang/Desktop/工作相关/nutrition_with_category.csv",
                        header = T,encoding = "UTF-8", sep= ",")

exc_number <- read.csv(file = "C:/Users/Daniel Kang/Desktop/工作相关/root_category_number/root_r.csv",
                       header = F,encoding = "UTF-8", sep= ",")

colnames(exc_number) <- c("id", "name")
exc_number$id <- as.numeric(exc_number$id)
exc_number$id[1] <- 50026800
head(exc_number)
exc_number$id
unique(is.na(exc_number$id))

taobao_data <- taobao_data[,-2]
unique(is.na(taobao_data$category_id))

selected_data <- taobao_data
for (i in 1:length(exc_number$id)){
  selected_data <- selected_data[exc_number$id[i] != selected_data$category_id,]
}
selected_data
dim(selected_data)


cookie_number <- read.csv(file = "C:/Users/Daniel Kang/Desktop/工作相关/root_category_number/cookie.csv",
                          header = F, encoding = "UTF-8", sep= ",")
colnames(cookie_number) <- c("id", "name")
cookie_number$id <- as.numeric(cookie_number$id)
cookie_number$id[1] <- 50002766
unique(is.na(cookie_number$id))
head(cookie_number)
for (i in 1:length(cookie_number$id)){
  selected_data <- selected_data[cookie_number$id[i] != selected_data$category_id,]
}
dim(selected_data)
selected_data

#traditional
traditional_number <- read.csv(file = "C:/Users/Daniel Kang/Desktop/工作相关/root_category_number/traditional.csv",
                               header = F, encoding = "UTF-8", sep= ",")

colnames(traditional_number) <- c("id", "name")
traditional_number$id <- as.numeric(traditional_number$id)
traditional_number$id[1] <- 50020275
unique(is.na(traditional_number$id))
head(traditional_number)

traditional_selected_data <- matrix(NA, nrow = 1, ncol = 2)
traditional_selected_data <- as.data.frame(traditional_selected_data)
colnames(traditional_selected_data) <- c("title", "category_id")
for (i in 1:length(traditional_number$id)){
  if((dim(selected_data[traditional_number$id[i] == selected_data$category_id,])[1]) != 0){
    
    
    traditional_selected_data <- rbind(traditional_selected_data, 
                                       selected_data[traditional_number$id[i] == selected_data$category_id,])
    
    print(i)
  
  }
}
traditional_selected_data <- traditional_selected_data[-1,]
traditional_selected_data_title <- traditional_selected_data$title

mixseg <-  worker()
traditional_selected_data_title_words <- unlist(lapply(X= myfile_selected,
                                                FUN = jiebaR :: segment, jiebar = mixseg))
traditional_selected_data_title_words <- gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","", traditional_selected_data_title_words)
traditional_selected_data_title_words <- gsub("\n","",traditional_selected_data_title_words)
traditional_selected_data_title_words <- gsub("　","",traditional_selected_data_title_words)
traditional_selected_data_title_words <- subset(traditional_selected_data_title_words
                                                , nchar(as.character(traditional_selected_data_title_words))>1)

traditional.freq  <- table(unlist(traditional_selected_data_title_words))
traditional.freq  <- rev(sort(traditional.freq))
traditional.freq  <- data.frame(word = names(traditional.freq), freq = traditional.freq);
traditional.freq2 <- subset(traditional.freq, traditional.freq$freq.Freq >= 2 )

head(traditional.freq2)

keyword_number <- c(2,3,6,8,9,11,12,13,15,18,19,21,25,31,33,37,39,45,46,47,48,52,53,
                    59,60,61,65,66,68,69,70,77,80,81,83,98,94,109,112,116,120,121,
                    129,131,133,134,139,145,149,161,162,165,166,168,171,172,173,175,176,183,184,192,194,
                    195,196,210,213,214,218,221,229,231,232,233,
                    238,259,294,238,298,391,398,416,429)

traditional.freq2[keyword_number,]
traditional_selected_data_title











