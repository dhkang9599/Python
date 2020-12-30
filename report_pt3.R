full_category       <- read.csv(file = "pt3-1.csv", header = T, sep = ",",encoding = "UTF-8") # category data
colnames(full_category) <- c("日期", "洁面", "基础护肤", "面膜", "基础底妆", "彩妆", "男士化妆品")
full_category$日期 <- anydate(full_category$日期)
full_category$TTL <- apply(full_category[,-1],1, sum)


full_category$洁面_YoY       <- YoY(full_category, colname = "洁面", datecol = "日期")
full_category$基础护肤_YoY   <- YoY(full_category, colname = "基础护肤", datecol = "日期")
full_category$面膜_YoY       <- YoY(full_category, colname = "面膜", datecol = "日期")
full_category$基础底妆_YoY   <- YoY(full_category, colname = "基础底妆", datecol = "日期")
full_category$彩妆_YoY       <- YoY(full_category, colname = "彩妆", datecol = "日期")
full_category$男士化妆品_YoY <- YoY(full_category, colname = "男士化妆品", datecol = "日期")
full_category$TTL_YoY        <- YoY(full_category, colname = "TTL", datecol = "日期")

full_category$洁面_MoM       <- round(c(NA, diff(full_category$洁面)/full_category$洁面[-length(full_category$洁面)])*100, 0)
full_category$基础护肤_MoM   <- round(c(NA, diff(full_category$基础护肤)/full_category$基础护肤[-length(full_category$基础护肤)])*100, 0)
full_category$面膜_MoM       <- round(c(NA, diff(full_category$面膜)/full_category$面膜[-length(full_category$面膜)])*100, 0)
full_category$基础底妆_MoM   <- round(c(NA, diff(full_category$基础底妆)/full_category$洁面[-length(full_category$基础底妆)])*100, 0)
full_category$彩妆_MoM       <- round(c(NA, diff(full_category$彩妆)/full_category$彩妆[-length(full_category$彩妆)])*100, 0)
full_category$男士化妆品_MoM <- round(c(NA, diff(full_category$男士化妆品)/full_category$男士化妆品[-length(full_category$男士化妆品)])*100, 0)
full_category$TTL_MoM        <- round(c(NA, diff(full_category$TTL)/full_category$TTL[-length(full_category$TTL)])*100, 0)

full_category$日期 <- format(full_category$日期,"%Y-%m")

category_YoY <- matrix(NA, nrow = 7, ncol = 3)
rownames(category_YoY) <- c("洁面", "基础护肤", "面膜", "基础底妆", "彩妆", "男士化妆品","TTL")
colnames(category_YoY) <- c("2019-08", "2020-08", "GR%")
category_YoY[, 1] <- full_category %>% 
                                dplyr:: filter(日期 == '2019-08') %>% 
                                dplyr:: select(c("洁面", "基础护肤", "面膜", "基础底妆", "彩妆", "男士化妆品","TTL")) %>% 
                                as.matrix() 
category_YoY[, 2] <- full_category %>% 
                                dplyr:: filter(日期 == '2020-08') %>% 
                                dplyr:: select(c("洁面", "基础护肤", "面膜", "基础底妆", "彩妆", "男士化妆品", "TTL")) %>%
                                as.matrix()
category_YoY[, 3] <- full_category %>% 
                                dplyr:: filter(日期 == '2020-08') %>% 
                                dplyr:: select(c("洁面_YoY","基础护肤_YoY", "面膜_YoY", "基础底妆_YoY", "彩妆_YoY", "男士化妆品_YoY","TTL_YoY")) %>% 
                                as.matrix()



category_ratio <- matrix(NA, nrow = 7, ncol = 2)
rownames(category_ratio) <- c("洁面", "基础护肤", "面膜", "基础底妆", "彩妆", "男士化妆品","TTL")
colnames(category_ratio) <- c("2019-08", "2020-08")
for(i in 1:7){
  category_ratio[i,]  <- category_YoY[i,-3]/category_YoY[7,-3]
}



category_YoY[, 1:2] <- round(category_YoY[,1:2]/1000000000, digits = 1)
category_ratio <- round((category_ratio * 100), digits = 0)


full_category
category_YoY
category_ratio







