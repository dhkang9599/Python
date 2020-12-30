plat_revenue_mon       <- read.csv(file = "pt1-1.csv", header = T, sep = ",",encoding = "UTF-8")
colnames(plat_revenue_mon) <- c("日期","天猫国际","天猫中国","淘宝国际","淘宝中国")
plat_revenue_mon$日期  <- anytime::anydate(plat_revenue_mon$日期)
plat_revenue_mon$TTL   <- apply(plat_revenue_mon[,2:5], MARGIN = 1, FUN = sum)
plat_revenue_mon$MoM   <- round(c(NA, diff(plat_revenue_mon$TTL)/plat_revenue_mon$TTL[-length(plat_revenue_mon$TTL)])*100, 0)
plat_revenue_mon$YoY   <- YoY(df = plat_revenue_mon, colname = "TTL", datecol = "日期", n = 0)
plat_revenue_mon$日期  <- format(plat_revenue_mon$日期, "%Y-%m")
plat_revenue_mon[,2:6] <- plat_revenue_mon[,2:6]/1000000000  #除以10亿
plat_revenue_mon[,2:5] <- round(plat_revenue_mon[,2:5], 1)
plat_revenue_mon[,6]   <- round(plat_revenue_mon[,6], 1)



double_mon_sum <- function(df, colvec, year_num, mon1, mon2){
                                                    library(dplyr)
                                                    library(lubridate)
  
                                                    df_tem      <- df  
                                                    datevec     <- df_tem$日期
                                                    df_tem$year <- lubridate:: year(anydate(datevec))
                                                    df_tem$mon  <- lubridate:: month(anydate(datevec))
                                                    df_tem %>% 
                                                            dplyr:: filter(year == year_num) %>% 
                                                            dplyr:: filter(mon == mon1 | mon == mon2) %>%
                                                            dplyr:: select(colvec) %>%
                                                            apply(2,sum) %>%
                                                            return()
                                                              
                                                   }
double_mon_19 <- double_mon_sum(df = plat_revenue_mon, 
                                colvec = c("天猫国际", "天猫中国", "淘宝国际", "淘宝中国","TTL"),
                                year_num = 2019, mon1 = 9, mon2 = 10)
double_mon_20 <- double_mon_sum(df = plat_revenue_mon, 
                                colvec = c("天猫国际", "天猫中国", "淘宝国际", "淘宝中国","TTL"),
                                year_num = 2020, mon1 = 9, mon2 = 10)

double_mon_matrix <- rbind(double_mon_19, double_mon_20)
double_growth <- (double_mon_matrix[2,] - double_mon_matrix[1,])/double_mon_matrix[1,] * 100
double_mon_matrix <- rbind(double_mon_matrix, double_growth)
double_mon_matrix[3,] <- round(double_mon_matrix[3,],0)
rownames(double_mon_matrix) <- c("19年 9-10", "20年 9-10", "YOY(%)")
double_mon_matrix




double_mon_sales_19 <- double_mon_sum(df = plat_sales_mon,
                                      colvec = c("天猫国际", "天猫中国", "淘宝国际", "淘宝中国","TTL"),
                                      year_num = 2019,
                                      mon1 = 9, mon2 = 10)

double_mon_sales_20 <- double_mon_sum(df = plat_sales_mon,
                                      colvec = c("天猫国际", "天猫中国", "淘宝国际", "淘宝中国","TTL"),
                                      year_num = 2020,
                                      mon1 = 9, mon2 = 10)
double_mon_sales_matrix <- rbind(double_mon_sales_19, double_mon_sales_20)
double_sales_growth <- (double_mon_sales_matrix[2,] - double_mon_sales_matrix[1,])/double_mon_sales_matrix[1,] * 100
double_mon_sales_matrix <- rbind(double_mon_sales_matrix, double_sales_growth)
double_mon_sales_matrix[3,] <- round(double_mon_sales_matrix[3,],0)
rownames(double_mon_sales_matrix) <- c("19年 9-10", "20年 9-10", "YOY(%)")
double_mon_sales_matrix


subset(plat_avg_mon, 日期 > anydate("2019-10"), 日期 <= anydate("2020-10")) #最近一年

avg_summary_matrix <- matrix(NA, nrow = 3, ncol = 4)
rownames(avg_summary_matrix) <- c("Min", "Med", "Max")
colnames(avg_summary_matrix) <- c("天猫国际", "天猫中国", "淘宝国际", "淘宝中国")
avg_summary_matrix[1,]  <- apply(subset(plat_avg_mon, 日期 > anydate("2019-10"), 日期 <= anydate("2020-10"))[, -1],
                                 MARGIN = 2, FUN = min)
avg_summary_matrix[2,]  <- apply(subset(plat_avg_mon, 日期 > anydate("2019-10"), 日期 <= anydate("2020-10"))[, -1],
                                 MARGIN = 2, FUN = median)
avg_summary_matrix[3,]  <- apply(subset(plat_avg_mon, 日期 > anydate("2019-10"), 日期 <= anydate("2020-10"))[, -1],
                                 MARGIN = 2, FUN = max)
avg_summary_matrix






single_mon_avg <- matrix(NA, nrow = 2, ncol = 4)
rownames(single_mon_avg) <- c("19年 10月", "20年 10月")
colnames(single_mon_avg) <- c("天猫国际","天猫中国", "淘宝国际","淘宝中国")
single_mon_avg[1,] <- plat_avg_mon %>% filter(日期 == anydate("2019-10")) %>% dplyr::select("天猫国际","天猫中国", "淘宝国际",
                                                                                          "淘宝中国") %>% as.matrix()
single_mon_avg[2,] <- plat_avg_mon %>% filter(日期 == anydate("2020-10")) %>% dplyr::select("天猫国际","天猫中国", "淘宝国际",
                                                                                          "淘宝中国") %>% as.matrix()
single_mon_avg


## conclu
double_mon_matrix
double_mon_sales_matrix
avg_summary_matrix
single_mon_avg
































