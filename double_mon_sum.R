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
double_mon_sum(df = plat_revenue_mon, colvec = c("天猫国际", "天猫中国", "淘宝国际", "淘宝中国"),
               year_num = 2019, mon1 = 9, mon2 = 10)