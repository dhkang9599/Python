
skincare_subcategory   <- read.csv(file = "pt4_1.csv", header = T, sep = ",",encoding = "UTF-8") # skincare subcategory data
colnames(skincare_subcategory) <- c("日期", "爽肤水", "面部精华", "乳液","防晒", "眼部护理", "护理套装", "护理旅行装")
skincare_subcategory$TTL <- apply(skincare_subcategory[,-1], 1, sum)


skincare_double_sum_matrix <- matrix(NA, nrow = 8, ncol = 3)
colnames(skincare_double_sum_matrix) <- c("19年 9-10月", "20年 9-10月", "YoY%")
rownames(skincare_double_sum_matrix) <- c("爽肤水", "面部精华", "乳液", "防晒", "眼部护理", "护理套装", "护理旅行装", "TTL")


double_mon_sum(skincare_subcategory, colvec = "爽肤水", year = 2019, mon1 = 9, mon2 = 10)

rownames(skincare_double_sum_matrix)

for(i in rownames(skincare_double_sum_matrix)){
  skincare_double_sum_matrix[i,1] <- double_mon_sum(skincare_subcategory, colvec = i, year = 2019, mon1 = 9, mon2 = 10) %>% as.matrix()
  skincare_double_sum_matrix[i,2] <- double_mon_sum(skincare_subcategory, colvec = i, year = 2020, mon1 = 9, mon2 = 10) %>% as.matrix()
}
skincare_double_sum_matrix[, 3] <- round(((skincare_double_sum_matrix[, 2] -  skincare_double_sum_matrix[, 1])/skincare_double_sum_matrix[, 1]) *100, digits = 0)



skincare_subcategory_ratio           <- matrix(NA, nrow = 7, ncol = 2)
rownames(skincare_subcategory_ratio) <- c("爽肤水", "面部精华", "乳液", "防晒", "眼部护理", "护理套装", "护理旅行装")
colnames(skincare_subcategory_ratio) <- c("19年 9-10月", "20年 9-10月")
for(i in 1:7){
  skincare_subcategory_ratio[i,]  <- skincare_double_sum_matrix[i,-3]/skincare_double_sum_matrix[8,-3]
}



skincare_double_sum_matrix[,1:2] <- round(skincare_double_sum_matrix[,1:2]/100000000, digits = 0)  #除以1亿
skincare_subcategory_ratio <- round(skincare_subcategory_ratio * 100,digits = 0)



skincare_subcategory_ratio
skincare_double_sum_matrix








