会议的目的

    新客户的数据展示

客户对产品的使用不熟悉，或者不知道如何使用数据。





lancome_19_sales   <- read.csv("C:/Users/Daniel Kang/Desktop/priceel/lancome_19_sales.csv", header = T, encoding = "UTF-8")
lancome_19_price   <- read.csv("C:/Users/Daniel Kang/Desktop/priceel/lancome_19_price.csv", header = T, encoding = "UTF-8")

lancome_20_sales   <- read.csv("C:/Users/Daniel Kang/Desktop/priceel/lancome_20_sales.csv", header = T, encoding = "UTF-8")
lancome_20_price   <- read.csv("C:/Users/Daniel Kang/Desktop/priceel/lancome_20_price.csv", header = T, encoding = "UTF-8")

ahc_19_sales   <- read.csv("C:/Users/Daniel Kang/Desktop/priceel/ahc_19_sales.csv", header = T, encoding = "UTF-8")
ahc_19_price   <- read.csv("C:/Users/Daniel Kang/Desktop/priceel/ahc_19_price.csv", header = T, encoding = "UTF-8")

ahc_20_sales   <- read.csv("C:/Users/Daniel Kang/Desktop/priceel/ahc_20_sales.csv", header = T, encoding = "UTF-8")
ahc_20_price   <- read.csv("C:/Users/Daniel Kang/Desktop/priceel/ahc_20_price.csv", header = T, encoding = "UTF-8")

sk2_19_sales   <- read.csv("C:/Users/Daniel Kang/Desktop/priceel/sk2_19_sales.csv", header = T, encoding = "UTF-8")
sk2_19_price   <- read.csv("C:/Users/Daniel Kang/Desktop/priceel/sk2_19_price.csv", header = T, encoding = "UTF-8")

sk2_20_sales   <- read.csv("C:/Users/Daniel Kang/Desktop/priceel/sk2_20_sales.csv", header = T, encoding = "UTF-8")
sk2_20_price   <- read.csv("C:/Users/Daniel Kang/Desktop/priceel/sk2_20_price.csv", header = T, encoding = "UTF-8")

lancome_19_df <- data.frame(date = lancome_19_sales$X.U.FEFF.Category, sales = lancome_19_sales$Lancome.兰蔻, price = lancome_19_price$Lancome.兰蔻)
lancome_20_df <- data.frame(date = lancome_20_sales$X.U.FEFF.Category, sales = lancome_20_sales$Lancome.兰蔻, price = lancome_20_price$Lancome.兰蔻)

ahc_19_df <- data.frame(date = ahc_19_sales$X.U.FEFF.Category, sales = ahc_19_sales$AHC, price = ahc_19_price$AHC)
ahc_20_df <- data.frame(date = ahc_20_sales$X.U.FEFF.Category, sales = ahc_20_sales$AHC, price = ahc_20_price$AHC)

sk2_19_df <- data.frame(date = sk2_19_sales$X.U.FEFF.Category, sales = sk2_19_sales$SK.II, price = sk2_19_price$SK.II)
sk2_20_df <- data.frame(date = sk2_20_sales$X.U.FEFF.Category, sales = sk2_20_sales$SK.II, price = sk2_20_price$SK.II)


summary(lm(log(sales)~log(price),data = lancome_19_df))
summary(lm(log(sales)~log(price),data = lancome_20_df))
summary(lm(log(sales)~log(price),data = ahc_19_df))
summary(lm(log(sales)~log(price),data = ahc_20_df))
summary(lm(log(sales)~log(price),data = sk2_19_df))
summary(lm(log(sales)~log(price),data = sk2_20_df))


ggplot(aes(x = log(price), y = log(sales)), data = lancome_19_df) +
  geom_point() + 
  stat_smooth(method = "lm", data = lancome_19_df)

ggplot(aes(x = log(price), y = log(sales)), data = lancome_20_df) +
  geom_point() + 
  stat_smooth(method = "lm", data = lancome_19_df)

ggplot(aes(x = log(price), y = log(sales)), data = ahc_19_df) +
  geom_point() + 
  stat_smooth(method = "lm", data = lancome_19_df)

ggplot(aes(x = log(price), y = log(sales)), data = ahc_20_df) +
  geom_point() + 
  stat_smooth(method = "lm", data = lancome_19_df)

ggplot(aes(x = log(price), y = log(sales)), data = sk2_19_df) +
  geom_point() + 
  stat_smooth(method = "lm", data = lancome_19_df)

ggplot(aes(x = log(price), y = log(sales)), data = sk2_20_df) +
  geom_point() + 
  stat_smooth(method = "lm", data = lancome_19_df)



###lancome

plot(y= log(lancome_19_df$sales), x = log(lancome_19_df$price))
lm_lancom_19 <- lm(log(lancome_19_df$sales) ~ log(lancome_19_df$price))
abline(lm_lancom_19)
summary(lm_lancom_19)

plot(y= log(lancome_20_df$sales), x = log(lancome_20_df$price))
lm_lancom_20 <- lm(log(lancome_20_df$sales) ~ log(lancome_20_df$price))
abline(lm_lancom_20)
summary(lm_lancom_20)


lancome_full <- rbind(lancome_19_df,lancome_20_df)
plot(y= log(lancome_full$sales, x = log(lancome_full$price))
lm_lancome_full <- lm(log(lancome_full$sales) ~ log(lancome_full$price))
abline(lm_lancome_full)
summary(lm_lancome_full)


###ahc

plot(y= log(ahc_19_df$sales), x = log(ahc_19_df$price))
lm_ahc_19 <- lm(log(ahc_19_df$sales) ~ log(ahc_19_df$price))
abline(lm_ahc_19)
summary(lm_ahc_19)

plot(y= log(ahc_20_df$sales), x = log(ahc_20_df$price))
lm_ahc_20 <- lm(log(ahc_20_df$sales) ~ log(ahc_20_df$price))
abline(lm_ahc_20)
summary(lm_ahc_20)


ahc_full <- rbind(ahc_19_df, ahc_20_df)
plot(y= log(ahc_full$sales), x = log(ahc_full$price))
lm_ahc_full <- lm(log(ahc_full$sales) ~ log(ahc_full$price))
abline(lm_ahc_full)
summary(lm_ahc_full)

## sk2

plot(y= log(sk2_19_df$sales), x = log(sk2_19_df$price))
lm_sk2_19 <- lm(log(sk2_19_df$sales) ~ log(sk2_19_df$price))
abline(lm_sk2_19)
summary(lm_sk2_19)

plot(y= log(sk2_20_df$sales), x = log(sk2_20_df$price))
lm_sk2_20 <- lm(log(sk2_20_df$sales) ~ log(sk2_20_df$price))
abline(lm_sk2_20)
summary(lm_sk2_20)


sk2_full <- rbind(sk2_19_df, sk2_20_df)
plot(y= log(sk2_full$sales), x = log(sk2_full$price))
lm_sk2_full <- lm(log(sk2_full$sales) ~ log(sk2_full$price))
abline(lm_sk2_full)
summary(lm_sk2_full)












####################################################################################################################################

price_18   <- read.csv("C:/Users/Daniel Kang/Desktop/工作相关/data/makeupela/18/price_18.csv", header = T, encoding = "UTF-8")
sales_18   <- read.csv("C:/Users/Daniel Kang/Desktop/工作相关/data/makeupela/18/sales_18.csv", header = T, encoding = "UTF-8")

price_19   <- read.csv("C:/Users/Daniel Kang/Desktop/工作相关/data/makeupela/18/price_19.csv", header = T, encoding = "UTF-8")
sales_19   <- read.csv("C:/Users/Daniel Kang/Desktop/工作相关/data/makeupela/18/sales_19.csv", header = T, encoding = "UTF-8")

price_20   <- read.csv("C:/Users/Daniel Kang/Desktop/工作相关/data/makeupela/18/price_20.csv", header = T, encoding = "UTF-8")
sales_20   <- read.csv("C:/Users/Daniel Kang/Desktop/工作相关/data/makeupela/18/sales_20.csv", header = T, encoding = "UTF-8")


##cleansing
plot(y = log(sales_18$洁面.卸妆) ,x =  log(price_18$洁面.卸妆))
summary(lm(log(sales_18$洁面.卸妆) ~ log(price_18$洁面.卸妆)))

plot(y = log(sales_19$洁面.卸妆) ,x =  log(price_19$洁面.卸妆))
summary(lm(log(sales_19$洁面.卸妆) ~ log(price_19$洁面.卸妆)))

plot(y = log(sales_20$洁面.卸妆) ,x =  log(price_20$洁面.卸妆))
summary(lm(log(sales_20$洁面.卸妆) ~ log(price_20$洁面.卸妆)))

price_full <- rbind(price_18, price_19)
price_full <- rbind(price_full, price_20)

sales_full <- rbind(sales_18, sales_19)
sales_full <- rbind(sales_full, sales_20)

#----------------------------------------------------------------------
plot(y = log(sales_full$洁面.卸妆) ,x =  log(price_full$洁面.卸妆))
lm_jiemian <- lm(log(sales_full$洁面.卸妆) ~ log(price_full$洁面.卸妆))
abline(lm_jiemian)
summary(lm_jiemian)

##

plot(y = log(sales_18$基础护肤) ,x =  log(price_18$基础护肤))
summary(lm(log(sales_18$基础护肤) ~ log(price_18$基础护肤)))

plot(y = log(sales_19$基础护肤) ,x =  log(price_19$基础护肤))
summary(lm(log(sales_19$基础护肤) ~ log(price_19$基础护肤)))

plot(y = log(sales_20$洁面.卸妆) ,x =  log(price_20$洁面.卸妆))
summary(lm(log(sales_20$洁面.卸妆) ~ log(price_20$洁面.卸妆)))



plot(y = log(sales_full$基础护肤) ,x =  log(price_full$基础护肤))
lm_basic <- lm(log(sales_full$基础护肤) ~ log(price_full$基础护肤))
abline(lm_basic)
summary(lm_basic)





#----T-mall--------------------------------------------------------------

price <- read.csv("C:/Users/Daniel Kang/Desktop/工作相关/data/makeupela/price.csv", header = T, encoding = "UTF-8")
sales <- read.csv("C:/Users/Daniel Kang/Desktop/工作相关/data/makeupela/sales.csv", header = T, encoding = "UTF-8")


par(mfrow = c(2,3))
plot(y = log(sales$基础护肤) ,x =  log(price$基础护肤))
basic_tmall <- lm(log(sales$基础护肤) ~ log(price$基础护肤))
abline(basic_tmall)
summary(basic_tmall)
title(main = summary(basic_tmall)$coef[2,1])

plot(y = log(sales$洁面.卸妆) ,x =  log(price$洁面.卸妆))
cleansing_tmall <- lm(log(sales$洁面.卸妆) ~ log(price$洁面.卸妆))
abline(cleansing_tmall)
summary(cleansing_tmall)
title(main = summary(cleansing_tmall)$coef[2,1])

plot(y = log(sales$面膜) ,x =  log(price$面膜))
mask_tmall <- lm(log(sales$面膜) ~ log(price$面膜))
abline(mask_tmall)
summary(mask_tmall)
title(main = summary(mask_tmall)$coef[2,1])

plot(y = log(sales$基础底妆) ,x =  log(price$基础底妆))
base_tmall <- lm(log(sales$基础底妆) ~ log(price$基础底妆))
abline(base_tmall)
summary(base_tmall)
title(main = summary(base_tmall)$coef[2,1])

plot(y = log(sales$彩妆) ,x =  log(price$彩妆))
color_tmall <- lm(log(sales$彩妆) ~ log(price$彩妆))
abline(color_tmall)
summary(color_tmall)
title(main = summary(color_tmall)$coef[2,1])

plot(y = log(sales$男士化妆品) ,x =  log(price$男士化妆品))
male_tmall <- lm(log(sales$男士化妆品) ~ log(price$男士化妆品))
abline(male_tmall)
summary(male_tmall)
title(main = summary(male_tmall)$coef[2,1])
par(mfrow = c(1,1))

##---------------------------------------------------------------------------------------------------------


library(lubridate)


sales$year <- anytime::anydate(sales$X.U.FEFF.Category)  %>% year()
price$year <- anytime::anydate(price$X.U.FEFF.Category)  %>% year()


sales_2019 <- sales %>% filter(year == 2019) 
sales_2020 <- sales %>% filter(year == 2020)

price_2019 <- price %>% filter(year == 2019)
price_2020 <- price %>% filter(year == 2020)

plot(y = sales_2019$面膜, x = price_2019$面膜)
plot(y = sales_2020$面膜, x = price_2020$面膜)



#-----------------------------------------------------------------------------------------------------------------


cbind_sp <- cbind(sales, price) 
colnames(cbind_sp) <- c("date_s", "cleansing_sales", "basic_sales", "mask_sales", "base_sales", "color_sales", "male_sales", "year_sales",
     "date_p", "cleansing_price", "basic_price", "mask_price", "base_price", "color_price", "male_price", "year_price")
cbind_sp <- cbind_sp %>% dplyr::select("date" = date_s, "year" = year_sales, cleansing_sales, basic_sales, mask_sales, base_sales, color_sales, male_sales, 
      cleansing_price, basic_price, mask_price, base_price, color_price, male_price)
cbind_sp

cbind_sp %>% filter(year == 2019) %>% ggplot(aes(x = log(cleansing_price), y = log(cleansing_sales))) + geom_point() + stat_smooth(method = "lm")
cbind_sp %>% ggplot(aes(x = log(cleansing_price), y = log(cleansing_sales))) + geom_point() + stat_smooth(method = "lm")
cbind_sp %>% ggplot(aes(x = log(basic_price), y = log(basic_sales))) + geom_point() + stat_smooth(method = "lm")
cbind_sp %>% ggplot(aes(x = log(mask_price), y = log(mask_sales))) + geom_point() + stat_smooth(method = "lm")
cbind_sp %>% ggplot(aes(x = log(base_price), y = log(base_sales))) + geom_point() + stat_smooth(method = "lm")
cbind_sp %>% ggplot(aes(x = log(color_price), y = log(color_sales))) + geom_point() + stat_smooth(method = "lm")
cbind_sp %>% ggplot(aes(x = log(male_price), y = log(male_sales))) + geom_point() + stat_smooth(method = "lm")


cbind_sp %>%
  filter(year == 2019 | year == 2020) %>% 
  ggplot(aes(x = log(male_price), y = log(male_sales))) + geom_point() + stat_smooth(method = "lm")

cbind_sp %>%
  filter(year == 2019 | year == 2020) %>% 
  ggplot(aes(x = log(cleansing_price), y = log(cleansing_sales))) + geom_point() + stat_smooth(method = "lm")

cbind_sp %>%
  filter(year == 2019 | year == 2020) %>% 
  ggplot(aes(x = log(basic_price), y = log(basic_sales))) + geom_point() + stat_smooth(method = "lm")


summary(lm(log(male_sales) ~ log(male_price), data = subset(cbind_sp, year == 2019 | year == 2020)))
  




########-----------------------------------------------------------------------------------------------------------------------------


color_sales   <- read.csv("C:/Users/Daniel Kang/Desktop/工作相关/data/makeupela/color_top10_brand_sales.csv", header = T, encoding = "UTF-8")
color_price   <- read.csv("C:/Users/Daniel Kang/Desktop/工作相关/data/makeupela/color_top10_brand_price.csv", header = T, encoding = "UTF-8")


lm(log(color_sales$PERFECT.DIARY.完美日记) ~ log(color_price$PERFECT.DIARY.完美日记)) %>% summary()
lm(log(color_sales$YSL.圣罗兰) ~ log(color_price$YSL.圣罗兰)) %>% summary()




ess_sales   <- read.csv("C:/Users/Daniel Kang/Desktop/工作相关/data/makeupela/ess_top10_brand_sales.csv", header = T, encoding = "UTF-8")
ess_price   <- read.csv("C:/Users/Daniel Kang/Desktop/工作相关/data/makeupela/ess_top10_brand_price.csv", header = T, encoding = "UTF-8")


lm(log(ess_sales$Olay.玉兰油) ~ log(ess_price$Olay.玉兰油)) %>% summary()
lm(log(ess_sales$Estee.Lauder.雅诗兰黛) ~ log(ess_price$Estee.Lauder.雅诗兰黛)) %>% summary()
plot(x = log(ess_price$Estee.Lauder.雅诗兰黛), y = log(ess_sales$Estee.Lauder.雅诗兰黛))



#-------------------------------------------------

color_sales   <- read.csv("C:/Users/Daniel Kang/Desktop/工作相关/data/makeupela/color_sales_weekly.csv", header = T, encoding = "UTF-8")
color_price   <- read.csv("C:/Users/Daniel Kang/Desktop/工作相关/data/makeupela/color_price_weekly.csv", header = T, encoding = "UTF-8")


lm(log(color_sales$PERFECT.DIARY.完美日记) ~ log(color_price$PERFECT.DIARY.完美日记)) %>% summary()
plot(x = log(color_price$PERFECT.DIARY.完美日记), y = log(color_sales$PERFECT.DIARY.完美日记))
abline(lm(log(color_sales$PERFECT.DIARY.完美日记) ~ log(color_price$PERFECT.DIARY.完美日记)))


lm(log(color_sales$PERFECT.DIARY.完美日记[1:39]) ~ log(color_price$PERFECT.DIARY.完美日记[1:39])) %>% summary()
lm(log(color_sales$PERFECT.DIARY.完美日记[40:90]) ~ log(color_price$PERFECT.DIARY.完美日记[40:90])) %>% summary()
plot(x = log(color_price$PERFECT.DIARY.完美日记[1:39]), y = log(color_sales$PERFECT.DIARY.完美日记[1:39]))
plot(x = log(color_sales$PERFECT.DIARY.完美日记[40:90]),  y = log(color_price$PERFECT.DIARY.完美日记)[40:90])


lm(log(color_sales$MAC.魅可) ~ log(color_price$MAC.魅可)) %>% summary()
lm(log(color_sales$MAC.魅可[1:39]) ~ log(color_price$MAC.魅可[1:39])) %>% summary()
lm(log(color_sales$MAC.魅可[40:90]) ~ log(color_price$MAC.魅可[40:90])) %>% summary()
plot(x = log(color_price$MAC.魅可[1:39]), y = log(color_sales$MAC.魅可[1:39]))
plot(x = log(color_price$MAC.魅可[40:90]), y = log(color_sales$MAC.魅可[40:90]))




lm(log(color_sales$X3CE[1:39]) ~ log(color_price$X3CE[1:39])) %>% summary()
lm(log(color_sales$X3CE[40:90]) ~ log(color_price$X3CE[40:90])) %>% summary()
plot(x = log(color_price$X3CE[1:39]), y = log(color_sales$X3CE[1:39]))
plot(x = log(color_price$X3CE[40:90]), y = log(color_sales$X3CE[40:90]))

lm(log(color_sales$Dior.迪奥[1:39]) ~ log(color_price$Dior.迪奥[1:39])) %>% summary()
lm(log(color_sales$Dior.迪奥[40:90]) ~ log(color_price$Dior.迪奥[40:90])) %>% summary()
plot(x = log(color_price$Dior.迪奥[1:39]), y = log(color_sales$Dior.迪奥[1:39]))
plot(x = log(color_price$Dior.迪奥[40:90]), y = log(color_sales$Dior.迪奥[40:90]))


lm(log(color_sales$Dior.迪奥) ~ log(color_price$Dior.迪奥)) %>% summary()
plot(x = log(color_price$Dior.迪奥), y = log(color_sales$Dior.迪奥))




#---------------------------------------------------------------------------

color_meike_sales_weely   <- read.csv("C:/Users/Daniel Kang/Desktop/工作相关/data/makeupela/color_meike_sales_weekly.csv", header = T, encoding = "UTF-8")
color_meike_price_weely   <- read.csv("C:/Users/Daniel Kang/Desktop/工作相关/data/makeupela/color_meike_price_weekly.csv", header = T, encoding = "UTF-8")

plot(x = log(color_meike_price_weely$MAC.魅可)[1:39], y = log(color_meike_sales_weely$MAC.魅可)[1:39])
lm(log(color_meike_sales_weely$MAC.魅可[40:90]) ~ log(color_meike_price_weely$MAC.魅可[40:90])) %>% summary()

plot(x = log(color_meike_price_weely$MAC.魅可), y = log(color_meike_sales_weely$MAC.魅可))
abline(lm(log(color_meike_sales_weely$MAC.魅可) ~ log(color_meike_price_weely$MAC.魅可)))
lm(log(color_meike_sales_weely$MAC.魅可) ~ log(color_meike_price_weely$MAC.魅可)) %>% summary()
plot(residuals(lm(log(color_meike_sales_weely$MAC.魅可) ~ log(color_meike_price_weely$MAC.魅可))))
#-----------------------------------------------------------------------


















