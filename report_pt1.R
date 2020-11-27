setwd("C:/Users/Daniel Kang/Desktop")
library(lubridate)
library(zoo)
library(anytime)


# YoY function
YoY <- function(df, colname, datecol, n = 2){
  YoY    <- vector()
  for(i in 1:dim(df)[1]){
    x      <- df[i,colname]
    x_date <- df[,datecol]
    if(length((x - df[x_date == (x_date - years(1))[i],colname])/df[x_date == (x_date - years(1))[i],colname]) == 0){
      YoY[i] <- NA
    } else{
      YoY[i] <- (x - df[x_date == (x_date - years(1))[i],colname])/df[x_date == (x_date - years(1))[i], colname]
    }
  }
  YoY <- round(YoY*100, n)
  return(YoY)
}



# 월별 플랫품 매출액

plat_revenue_mon       <- read.csv(file = "pt1-1.csv", header = T, sep = ",",encoding = "UTF-8")
colnames(plat_revenue_mon) <- c("日期","天猫国际","天猫中国","淘宝国际","淘宝中国")
plat_revenue_mon$日期  <- anytime::anydate(plat_revenue_mon$日期)
plat_revenue_mon$TTL   <- apply(plat_revenue_mon[,2:5], MARGIN = 1, FUN = sum)
plat_revenue_mon[,2:6] <- plat_revenue_mon[,2:6]/1000000000  #除以10亿
plat_revenue_mon[,2:5] <- round(plat_revenue_mon[,2:5],0)
plat_revenue_mon[,6]   <- round(plat_revenue_mon[,6],1)
plat_revenue_mon$MoM   <- round(c(NA,diff(plat_revenue_mon$TTL)/plat_revenue_mon$TTL[-length(plat_revenue_mon$TTL)])*100,0)
plat_revenue_mon$YoY   <- YoY(df = plat_revenue_mon, colname = "TTL", datecol = "日期", n = 0)
plat_revenue_mon$日期  <- format(plat_revenue_mon$日期, "%Y-%m")





# 월별 플랫품 판매량
plat_sales_mon       <- read.csv(file = "pt1-2.csv", header = T, sep = ",",encoding = "UTF-8")
colnames(plat_sales_mon) <- c("日期","天猫国际","天猫中国","淘宝国际","淘宝中国")
plat_sales_mon$日期  <- anytime::anydate(plat_sales_mon$日期)
plat_sales_mon$TTL   <- apply(plat_sales_mon[,2:5], MARGIN = 1, FUN = sum)
plat_sales_mon[2:6]  <- plat_sales_mon[,2:6]/1000000  #除以10亿
plat_sales_mon[,2:5] <- round(plat_sales_mon[,2:5],0)
plat_sales_mon[,6]   <- round(plat_sales_mon[,6],0)
plat_sales_mon$MoM_sales <- round(c(NA, diff(plat_sales_mon$TTL)/plat_sales_mon$TTL[-length(plat_sales_mon$TTL)])*100,0)
plat_sales_mon$YoY_sales <- YoY(df = plat_sales_mon, colname = "TTL", datecol = "日期", n = 0)
plat_sales_mon$avg     <- round(plat_revenue_mon$TTL/plat_sales_mon$TTL * 1000, 0)
plat_sales_mon$MoM_avg <- round(c(NA, diff(plat_sales_mon$avg)/plat_sales_mon$avg[-length(plat_sales_mon$avg)])*100,0)
plat_sales_mon$YoY_avg <- YoY(df = plat_sales_mon, colname = "avg", datecol = "日期", n = 0)



# 월별 플랫품 상품개수 

##平台/平台走势/csv文件
##选满所有日期；数据类型：商品数量


plat_counts_mon      <- read.csv(file = "pt1-3.csv", header = T, sep = ",",encoding = "UTF-8")
colnames(plat_counts_mon) <- c("日期","天猫国际","天猫中国","淘宝国际","淘宝中国")
plat_counts_mon



# 월별 플랫품 평균가격 


plat_avg_mon      <- read.csv(file = "pt1-4.csv", header = T, sep = ",",encoding = "UTF-8")
colnames(plat_avg_mon) <- c("日期","天猫国际","天猫中国","淘宝国际","淘宝中国")
plat_avg_mon[,2:5] <- round(plat_avg_mon[,2:5],0)
plat_avg_mon









