setwd("C:/Users/Daniel Kang/Desktop/file_9")
temp_9 <- list.files(pattern="*.xlsx")
data_9 <- vector(mode = "list", length = 10)
names(data_9) <- sub(".xlsx", temp_9, replacement = "")
for (i in 1:length(temp_9)){
  data_9[[i]] <- readxl::read_xlsx(temp_9[i])
}


setwd("C:/Users/Daniel Kang/Desktop/file_10")
temp_10 <- list.files(pattern="*.xlsx")
data_10 <- vector(mode = "list", length = 10)
names(data_10) <- sub(".xlsx", temp_10, replacement = "")
for (i in 1:length(temp_10)){
  data_10[[i]] <- readxl::read_xlsx(temp_10[i])
}

data_10



file_9_data    <- data_9[[1]]
file_10_data   <- data_10[[1]]


for(i in 2:10){
  
  file_9_data   <- rbind(file_9_data, data_9[[i]])
  file_10_data  <- rbind(file_10_data, data_10[[i]])

}

file_9_data  <- file_9_data[,-c(1,5)]
file_10_data <- file_10_data[,-c(1,5)]

file_9_data <- file_9_data[!duplicated(file_9_data$URL),]
file_10_data <- file_10_data[!duplicated(file_10_data$URL),]
unique(file_10_data$URL)
unique(file_9_data$URL)


month_check_10 <- as.data.frame(matrix(NA, nrow = 1000, ncol = 8))
month_check_9  <- as.data.frame(matrix(NA, nrow = 1000, ncol = 8))



for(i in 1:dim(file_9_data)[1]){
  if(dim(file_10_data[file_10_data$URL == file_9_data$URL[i],])[1] != 0){
    month_check_10[i,] <- file_10_data[file_10_data$URL == file_9_data$URL[i],]
    month_check_9[i,] <- file_9_data[i,]
  } 
}


month_check_9_narem  <- month_check_9[!is.na(month_check_9$V2),]
month_check_10_narem <- month_check_10[!is.na(month_check_10$V2),]


month_check_10_narem$V9 <- (month_check_10_narem$V7 - month_check_9_narem$V7)/month_check_9_narem$V7
dim(month_check_10_narem)

library(jiebaR)
mixseg <-  worker()
traditional_selected_data_title_words <- unlist(lapply(X= month_check_10_narem$V3,
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

head(traditional.freq)

word_dimention <- t(traditional.freq[,-1])[1,]
dim(month_check_10_narem)





  sum(word_dimention[1] == segment(month_check_10_narem$V3[1], jiebar = mixseg))






text_data <- as.data.frame(matrix(NA, nrow = dim(month_check_10_narem)[1], ncol = length(word_dimention)))
colnames(text_data)  <- word_dimention

for (z in 1:dim(month_check_10_narem)[1]){
 for(i in 1:length(word_dimention)){
      text_data[z,i] <-  sum(word_dimention[i] == segment(month_check_10_narem$V3[z], jiebar = mixseg))
 }
}

text_data <- cbind(month_check_10_narem[,c(3,9)], text_data)
colnames(text_data) <- c("title", "return", word_dimention)
dim(text_data)

library(glmnet)


dim(text_data)
apply(text_data[, 3:243],1, FUN = sum)

lasso_res <- glmnet(x = as.matrix(text_data[,3:636]), ,y = text_data$return, family = "gaussian", nlambda = 50, alpha = 1)
round(lasso_res$beta,0)





pca <- prcomp(text_data[,3:636,], scale. = T)
pca.var <- pca$sdev^2  ## sdev是标准偏差，十个样本，就有十个标准偏差，平方是避免负数的干扰
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)  ##求每个样本的variation
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")  ##用柱状图可视化


pca.data <- data.frame(Sample=rownames(pca$x),
                       X=pca$x[,1],
                       Y=pca$x[,2])
pca.data



glmnet(x = as.matrix(text_data[,3:636]), y = text_data$return,
       family = 'gaussian',
       alpha = 1,
       lambda = 0.01)
       



xx <- rnorm(1000,3,100)

xx <- matrix(xx,nrow = 100, ncol = 10)

rownames(xx) <- 1:100
colnames(xx) <- 201:210



lasso_xx <- glmnet(x = xx[,2:10], y = xx[,1], family = "gaussian", nlambda = 50, alpha = 1)
lasso_xx$beta




model.lasso <- cv.glmnet(x = as.matrix(text_data[,3:636]), ,y = text_data$return, family = "gaussian", alpha = 1)
lam <- model.lasso$lambda.1se
coef(model.lasso)

text_data
