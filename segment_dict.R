
# https://pinyin.sogou.com/dict/detail/index/79686/?rf=dictindex&pos=dict_rcmd

dic_brand <- read.table("C:/Users/Daniel Kang/Desktop/工作相关/词库/品牌.txt", encoding = "UTF-8")
dic_all   <- read.table("C:/Users/Daniel Kang/Desktop/工作相关/词库/化妆品大全.txt", encoding = "UTF-8")
dic_hufu  <- read.table("C:/Users/Daniel Kang/Desktop/工作相关/词库/化妆美容护肤.txt", encoding = "UTF-8")
dic_brand$V1
dic_all$V1



engine_new_word <- worker()
new_user_word(engine_new_word, c(dic_brand$V1, dic_all$V1,"bb霜","BB霜","牛奶肌", "花西子","郑瑄茉", "气垫","cc霜","萃汇尔",
                                 "泫雅","蘑菇头","雪花秀","CC霜"))
segment(BB_shops_6_8$title,engine_new_word)
tail(segment(BB_shops_6_8$title,engine_new_word),200)

bb_6_8_title <- vector("list", length = 1000)

for(i in 1:1000){
  bb_6_8_title[[i]] <- unlist(segment(BB_shops_6_8$title[i],engine_new_word))
}
bb_6_8_title















