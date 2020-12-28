#全部套装
shampoo_test_11 %>% # （全部套装)   
  filtering_keywords_grepl(key.words = c("组合","套","\\*","\\+"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("育发套装"),contain = F) %>%
  View()



### 男套装一
shampoo_test_11 %>% # 只有"男"字的挑出来   
  filtering_keywords_grepl(key.words = c("组合","套","\\*","\\+"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("男"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("女"), contain = F) %>%
  filtering_keywords_grepl(key.words = c("育发套装"),contain = F) %>%
  View()

shampoo_test_v11 %>% # （组合 OR 套 OR \\* OR \\+）NOT(男 OR 育发套装)   
  filtering_keywords_grepl(key.words = c("组合","套","\\*","\\+"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("育发套装"),contain = F) %>%
  View()

shampoo_test_s11 %>% # （组合 OR 套 OR \\* OR \\+）NOT(男 OR 育发套装)   
  filtering_keywords_grepl(key.words = c("组合","套","\\*","\\+"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("男"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("女"), contain = F) %>%
  filtering_keywords_grepl(key.words = c("育发套装"),contain = F) %>%
  View()



shampoo_test_9 %>% 
  filtering_keywords_grepl(key.words = c("男"), contain = T) %>%
  View()


#普通套装
##全部 (329)
shampoo_test_11 %>%    
  filtering_keywords_grepl(key.words = c("组合","套","\\*","\\+"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("育发套装"), contain = F) %>%
  filtering_keywords_grepl(key.words = c("洗发水","洗发液","洗发露","润发露","洗发膏","洗护"), contain = T) %>%
  View()


##(1)不带"女"套装 (232)
shampoo_test_11 %>%    
  filtering_keywords_grepl(key.words = c("组合","套","\\*","\\+"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("女"), contain = F) %>%
  filtering_keywords_grepl(key.words = c("育发套装"),contain = F) %>%
  filtering_keywords_grepl(key.words = c("洗发水","洗发液","洗发露","润发露","洗发膏","洗护"), contain = T) %>%
  View()

##(2)带"男女"套装 (69)
shampoo_test_11 %>%    
  filtering_keywords_grepl(key.words = c("组合","套","\\*","\\+"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("女"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("男"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("育发套装"),contain = F) %>%
  filtering_keywords_grepl(key.words = c("洗发水","洗发液","洗发露","润发露","洗发膏","洗护"), contain = T) %>%
  View()


##(3)只带"女"套装 (28)
shampoo_test_11 %>%    
  filtering_keywords_grepl(key.words = c("组合","套","\\*","\\+"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("女"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("男"), contain = F) %>%
  filtering_keywords_grepl(key.words = c("育发套装"),contain = F) %>%
  filtering_keywords_grepl(key.words = c("洗发水","洗发液","洗发露","润发露","洗发膏","洗护"), contain = T) %>%
  View()







#全套装
shampoo_test_11 %>%    #(338)
  filtering_keywords_grepl(key.words = c("组合","套","\\*","\\+"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("洗面奶","面膜"), contain = F) %>%
  View()


#套装(一)
##普通套装1
shampoo_test_11 %>%    #(250)
  filtering_keywords_grepl(key.words = c("组合","套","\\*","\\+"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("男"), contain = F) %>%
  filtering_keywords_grepl(key.words = c("洗面奶","面膜"),contain = F) %>%
  View()

##普通套装2
shampoo_test_11 %>%    #(70)
  filtering_keywords_grepl(key.words = c("组合","套","\\*","\\+"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("男"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("女"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("洗面奶","面膜"),contain = F) %>%
  View()

#套装(二)
shampoo_test_11 %>%    #(2)
  filtering_keywords_grepl(key.words = c("组合","套","\\*","\\+"), contain = F) %>%
  filtering_keywords_grepl(key.words = c("男"), contain = F) %>%
  filtering_keywords_grepl(key.words = c("沐浴"),contain = T) %>%
  View()

shampoo_test_11 %>%    #(1)
  filtering_keywords_grepl(key.words = c("组合","套","\\*","\\+"), contain = F) %>%
  filtering_keywords_grepl(key.words = c("男"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("女"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("沐浴"),contain = T) %>%
  View()



##套装男
###1
shampoo_test_11 %>%    #(18)
  filtering_keywords_grepl(key.words = c("组合","套","\\*","\\+"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("男"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("女"), contain = F) %>%
  filtering_keywords_grepl(key.words = c("洗面奶","面膜", contain = F)) %>%
  View()

###2
shampoo_test_11 %>%    #(4)
  filtering_keywords_grepl(key.words = c("组合","套","\\*","\\+"), contain = F) %>%
  filtering_keywords_grepl(key.words = c("沐浴"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("男"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("女"), contain = F) %>%
  View()



#男洗发水(待check)






#头皮护理
shampoo_test_11 %>% #(咨询 OR 白 OR 黑 OR 乌 OR 育发 OR 毛囊 OR 头皮 OR 增发 OR 发际 OR 生发 OR 密发) NOT (洗发水 OR 洗发液 OR 洗发露 OR 润发露)
  filtering_keywords_grepl(key.words = c("咨询","白","黑","乌","育发","毛囊","头皮","增发", "发际","生发","密发"), contain = T) %>%
  filtering_keywords_grepl(key.words = c("洗发水","洗发液","洗发露","润发露","洗发膏"), contain = F) %>%
  View()




#######################################################################################################################################


set_test_9  <- rbind_data(path = "C:/Users/Daniel Kang/Desktop/haircaretest/set/9",  type = "xlsx")
set_test_10 <- rbind_data(path = "C:/Users/Daniel Kang/Desktop/haircaretest/set/10", type = "xlsx")
set_test_11 <- rbind_data(path = "C:/Users/Daniel Kang/Desktop/haircaretest/set/11", type = "xlsx")
count_words(set_test_9, keywords.vector = c("套装"))


set_test_s9  <- rbind_data(path = "C:/Users/Daniel Kang/Desktop/haircaretest/set/9s",  type = "xlsx")
set_test_s10 <- rbind_data(path = "C:/Users/Daniel Kang/Desktop/haircaretest/set/10s", type = "xlsx")
set_test_s11 <- rbind_data(path = "C:/Users/Daniel Kang/Desktop/haircaretest/set/11s", type = "xlsx")
count_words(set_test_s9, keywords.vector = c("套装"))





#发膜
set_test_s9 %>%
  filtering_keywords_grepl(key.words = c("洗发水","洗发液","洗发露","润发露","洗发膏","洗护","护发素"), contain = F) %>%
  View()

set_test_s9 %>%
  filtering_keywords_grepl(key.words = c("组合","套","\\*","\\+"), contain = F) %>%
  View()



set_test_s10

#精油



#头皮护理



#洗发水


#护发素


#男套装









set_test_s9 %>%
  filtering_keywords_grepl(key.words = c("洗发水","洗发液","洗发露","润发露","洗发膏"), contain = T) %>% 
  filtering_keywords_grepl(key.words = c("护发素"),F) %>%
  filtering_keywords_grepl(key.words = c("组合","套","\\*","\\+"), contain = F) %>%
  View()










