pt5_c   <- read.csv(file = "pt5_c.csv", header = T, sep = ",",encoding = "UTF-8") #产地中国， 所有平台数据
pt5_k   <- read.csv(file = "pt5_k.csv", header = T, sep = ",",encoding = "UTF-8")
pt5_j   <- read.csv(file = "pt5_j.csv", header = T, sep = ",",encoding = "UTF-8")
pt5_a   <- read.csv(file = "pt5_a.csv", header = T, sep = ",",encoding = "UTF-8")
pt5_f   <- read.csv(file = "pt5_f.csv", header = T, sep = ",",encoding = "UTF-8")
pt5_grand   <- read.csv(file = "pt5_grand.csv", header = T, sep = ",",encoding = "UTF-8")










colnames(pt5_c)[1] <- "日期"
colnames(pt5_a)[1] <- "日期"
colnames(pt5_k)[1] <- "日期"
colnames(pt5_j)[1] <- "日期"
colnames(pt5_f)[1] <- "日期"
colnames(pt5_grand)[1] <- "日期"


country_matrix <- matrix(NA, nrow = dim(pt5_c)[1], ncol = 9) %>% as.data.frame() 
colnames(country_matrix) <- c("日期", "CN", "FR", "JP", "KR", "US", "TTL", "ratio", "GrandTTL")
country_matrix$日期  <- pt5_a$日期
country_matrix$CN    <- apply(pt5_c[,-1], 1, sum)
country_matrix$FR    <- apply(pt5_f[,-1], 1, sum)
country_matrix$JP    <- apply(pt5_j[,-1], 1, sum)
country_matrix$KR    <- apply(pt5_k[,-1], 1, sum)
country_matrix$US    <- apply(pt5_a[,-1], 1, sum)
country_matrix$TTL   <- apply(country_matrix[,2:6], 1, sum)
country_matrix$GrandTTL <- apply(pt5_grand[,-1], 1, sum)
country_matrix$ratio <- round(country_matrix$TTL/country_matrix$GrandTTL * 100, digits = 0)



country_ratio_matrix      <- matrix(NA, nrow = dim(pt5_c)[1], ncol = 7) %>% as.data.frame() 
colnames(country_ratio_matrix) <- c("日期", "CN", "FR", "JP", "KR", "US", "TTL")
country_ratio_matrix$日期 <- country_matrix$日期
country_ratio_matrix$CN   <-  round((country_matrix$CN/country_matrix$GrandTTL * 100), digits = 1)
country_ratio_matrix$FR   <-  round((country_matrix$FR/country_matrix$GrandTTL * 100), digits = 1)
country_ratio_matrix$JP   <-  round((country_matrix$JP/country_matrix$GrandTTL * 100), digits = 1)
country_ratio_matrix$KR   <-  round((country_matrix$KR/country_matrix$GrandTTL * 100), digits = 1)
country_ratio_matrix$US   <-  round((country_matrix$US/country_matrix$GrandTTL * 100), digits = 1)
country_ratio_matrix$TTL  <-  country_matrix$ratio



country_vector <- c("CN", "FR", "JP", "KR", "US")
double_mon_country_matrix <- matrix(NA, nrow = 3, ncol = 5)
colnames(double_mon_country_matrix) <- country_vector
rownames(double_mon_country_matrix) <- c("19年 9-10月", "20年 9-10月","GR%")

for (i in country_vector){
  double_mon_country_matrix[1,i] <- country_matrix %>% double_mon_sum(colvec = i, year_num = 2019, mon1 = 9, mon2 = 10)
  double_mon_country_matrix[2,i] <- country_matrix %>% double_mon_sum(colvec = i, year_num = 2020, mon1 = 9, mon2 = 10)
}

double_mon_country_matrix[3,] <- round(((double_mon_country_matrix[2,] - double_mon_country_matrix[1,])/double_mon_country_matrix[1,]) * 100, digits = 0)



double_mon_ratio_country_matrix <-  matrix(NA, nrow = 3, ncol = 5)
colnames(double_mon_ratio_country_matrix) <- country_vector
rownames(double_mon_ratio_country_matrix) <- c("19年 9-10月 M/S", "20年 9-10月 M/S", "GR%")
double_mon_ratio_country_matrix[1, ]  <- double_mon_country_matrix[1,]/double_mon_sum(country_matrix, colvec = "GrandTTL", year = 2019, mon1 = 9, mon2 = 10)
double_mon_ratio_country_matrix[2, ]  <- double_mon_country_matrix[2,]/double_mon_sum(country_matrix, colvec = "GrandTTL", year = 2020, mon1 = 9, mon2 = 10)
double_mon_ratio_country_matrix       <- round(double_mon_ratio_country_matrix * 100, digits = 1)
double_mon_ratio_country_matrix[3, ]  <- double_mon_country_matrix[3,]
double_mon_ratio_country_matrix 








#conclu
country_matrix
double_mon_country_matrix
double_mon_ratio_country_matrix 




