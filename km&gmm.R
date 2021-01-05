mois_11 <- rbind_data(path = "C:/Users/Daniel Kang/Desktop/base/mois", type = "xlsx")
mois_11 <- mois_11[,-c(1,3,5)]

feature_words_s <-  c("爽肤水", "护肤水", "柔肤水", "润肤水", "保湿水")
engine_new_word <- worker()
new_user_word(engine_new_word, feature_words_up)


mois_seg_list <- vector("list", length = 2000)
for(i in 1:2000){
  mois_seg_list[[i]] <- segment(mois_11$TITLE[i], engine_new_word)
}

check_vector <- c()
for(i in 1:2000){
  if(sum(mois_seg_list[[i]] == c("爽肤水") | mois_seg_list[[i]] ==  c("保湿水") | mois_seg_list[[i]] ==  c("柔肤水") | 
         mois_seg_list[[i]] ==  c("护肤水") | mois_seg_list[[i]] ==  c("润肤水")) == 0){
    check_vector[i] <- i
  }
}
mois_11_modi <- mois_11[-which(is.na(check_vector)), ]
mois_11_modi %>% head()
mois_11_modi %>% dim()
View(mois_11_modi)

mois_11_onlyt <- mois_11[which(is.na(check_vector)), ]
View(mois_11_onlyt)

segment(mois_11_onlyt$TITLE, engine_new_word)
sort(table(segment(mois_11_onlyt$TITLE, engine_new_word)))


mois_only_feature_word  <- c("保湿","补水","化妆水","毛孔","收缩","滋润","柔肤","舒缓")
engine_new_word2        <- worker()
new_user_word(engine_new_word2, mois_only_feature_word)
feature_11_ex           <- matrix(NA, nrow = (2000 - dim(mois_11_onlyt)[1]), ncol = length(mois_only_feature_word))
colnames(feature_11_ex) <- mois_only_feature_word

for(j in 1:dim(feature_11_ex)[1]){
  for(i in 1:length(mois_only_feature_word)){
    if(sum(mois_only_feature_word[i] == segment(mois_11_onlyt$TITLE[j], engine_new_word2)) != 0){
      feature_11_ex[j,i] <- 1
    }else{
      feature_11_ex[j,i] <- 0
    }
  }
}
feature_11_ex <- as.data.frame(feature_11_ex)
feature_11_ex$brand <- mois_11_modi$brand
head(feature_11_ex)

kmodes.ex.results <- kmodes(data = feature_11_ex[, -9], 2, iter.max = 10, weight = F)
feature_11_ex$cluster <- kmodes.ex.results$cluster
mois_11_modi$cluster <- kmodes.ex.results$cluster
mois_11_modi[,c(1,2,3,8)] %>% filter(cluster == 2) %>% View()
mois_11_modi[,c(1,2,3,8)] %>% filter(cluster == 1) %>% View()



sg <- rep(0,10)
for(i in 1: 10){
  gg <- kmodes(data = feature_11_ex[, -9], i, iter.max = 10, weight = F)$withindiff
  gg.sum <- sum(gg)
  sg[i] <- gg.sum
}
sg
plot(1:10, sg, type = "b",xlab = "Numbers of clusters", ylab = "within group sum of squares")







##########################
mois_11 
engine_full <- worker()
sort(table(segment(mois_11$TITLE, engine_full)),T)


feature_words_full <-  c("爽肤水","保湿","清爽", "精华水" ,"补水", "修复","滋润", "护肤水", "精华液", "柔肤水","毛孔", "修护", "收缩", 
                         "精粹", "舒缓", "敏感", "修护", "化妆水",  "精粹", "爽肤", "润肤水", "精萃液", "原液", "保湿水", "紧致", "敏感")
new_user_word(engine_full, feature_words_full)
feature_11_full <- matrix(nrow = 2000, ncol = length(feature_words_full))
colnames(feature_11_full) <- feature_words_full

for(j in 1:2000){
  for(i in 1:length(feature_words_full)){
    if(sum(feature_words_full[i] == segment(mois_11$TITLE[j], engine_full)) != 0){
      feature_11_full[j,i] <- 1
    }else{
      feature_11_full[j,i] <- 0
    }
  }
}
feature_11_full
sort(table(segment(mois_11$TITLE, engine_full)))['保湿']


sg <- c()
for(i in 1:10){
  sg[i] <- sum(kmodes(feature_11_full, i )$withindiff)
}
plot(1:10, sg, type = "b")

kmodes(feature_11_full, 2)



## calculate weights
wt <- c()
for(i in 1:length(feature_words_full)){
  wt[i] <- sort(table(segment(mois_11$TITLE, engine_full)))[feature_words_full[i]]
}
wt/sum(wt)
kmeans(daisy(feature_11_full, metric  = "gower", weights = wt/sum(wt)), 2)






##
essence <- rbind_data(path = "C:/Users/Daniel Kang/Desktop/essence", type = "xlsx")
essence_word_engine <- worker()
sort(table(segment(essence$TITLE, essence_word_engine)),T)
c(原液,淡化,修复,肌底,祛痘,抗初老,祛斑)




##
engine_new_word <- worker()
new_user_word(engine_new_word, feature_words_s)

mois_seg_list  <- vector("list", length = 2000)
for(i in 1:2000){
  mois_seg_list[[i]] <- segment(mois_11$TITLE[i], engine_new_word)
}

check_vector <- c()
for(i in 1:2000){
  if(sum(mois_seg_list[[i]] == c("爽肤水") | mois_seg_list[[i]] ==  c("保湿水") | mois_seg_list[[i]] ==  c("柔肤水") | 
         mois_seg_list[[i]] ==  c("护肤水") | mois_seg_list[[i]] ==  c("润肤水") | mois_seg_list[[i]] == c("爽肤化妆水")) == 0){
    check_vector[i] <- i
  }
}
mois_11_modi <- mois_11[-which(is.na(check_vector)), ]
engine_new_word <- worker()
sort(table(segment(mois_11_modi$TITLE,engine_new_word)),T)

mois_seg_modi_list <- vector("list", length = 707)
for(i in 1:707){
  mois_seg_modi_list[[i]] <- segment(mois_11_modi$TITLE[i], engine_new_word)
}

for(i in 1:707){
  if(sum(mois_seg_modi_list[[i]] == "原液") != 0){
    print(mois_11_modi$URL[i])
  }
}




烟酰胺，原液，精华液，玻尿酸，三文鱼，鲑鱼卵，鱼籽，安瓶，水光针，神仙水，啫喱


for(i in 1:707){
  if(sum(mois_seg_modi_list[[i]] == "神仙") != 0){
    print(mois_11_modi$URL[i])
  }
}



#原液
https://item.taobao.com/item.htm?id=601909402912
https://item.taobao.com/item.htm?id=533049404615
https://item.taobao.com/item.htm?id=605980452769
https://item.taobao.com/item.htm?id=626121121617
https://item.taobao.com/item.htm?id=618736329486
https://item.taobao.com/item.htm?id=618892058879
https://item.taobao.com/item.htm?id=606172397978
https://item.taobao.com/item.htm?id=606172397978
https://item.taobao.com/item.htm?id=622619387516
https://item.taobao.com/item.htm?id=531869578250
https://item.taobao.com/item.htm?id=616672695710
https://item.taobao.com/item.htm?id=628264299321
https://item.taobao.com/item.htm?id=35594710854
https://item.taobao.com/item.htm?id=13669091880
https://item.taobao.com/item.htm?id=623713591673
https://item.taobao.com/item.htm?id=23366616741
https://item.taobao.com/item.htm?id=612526603851
https://item.taobao.com/item.htm?id=582692848969
https://item.taobao.com/item.htm?id=577757437384
https://item.taobao.com/item.htm?id=611244370970
https://item.taobao.com/item.htm?id=583021886609

#烟酰胺
https://item.taobao.com/item.htm?id=631898954822
https://item.taobao.com/item.htm?id=37265373073
https://item.taobao.com/item.htm?id=626305424839
https://item.taobao.com/item.htm?id=610354590161
https://item.taobao.com/item.htm?id=598128249492


#安瓶

anping <- worker()
new_user_word(anping, c("安瓶"))
mois_seg_modi_list <- vector(mode = "list")
for(i in 1:707){
  mois_seg_modi_list[[i]] <- segment(mois_11_modi$TITLE[i], anping)
}
for(i in 1:707){
  if(sum(mois_seg_modi_list[[i]] == "安瓶") != 0){
    print(mois_11_modi$URL[i])
  }
}
c <- c("安瓶")
filtering_keywords(df = mois_11_modi, key.words = c, contain = T)$URL




##鱼籽
anping <- worker()
new_user_word(anping, c("鱼籽"))
for(i in 1:707){
  mois_seg_modi_list[[i]] <- segment(mois_11_modi$TITLE[i], anping)
}
for(i in 1:707){
  if(sum(mois_seg_modi_list[[i]] == "鱼籽") != 0){
    print(mois_11_modi$URL[i])
  }
}

##鱼子
anping <- worker()
new_user_word(anping, c("鱼子"))
for(i in 1:707){
  mois_seg_modi_list[[i]] <- segment(mois_11_modi$TITLE[i], anping)
}
for(i in 1:707){
  if(sum(mois_seg_modi_list[[i]] == "鱼子") != 0){
    print(mois_11_modi$URL[i])
  }
}

https://item.taobao.com/item.htm?id=538811376465
https://item.taobao.com/item.htm?id=545353968123
https://item.taobao.com/item.htm?id=625730557596
https://item.taobao.com/item.htm?id=600746313776
https://item.taobao.com/item.htm?id=593689176116


###



feature_word_exskin <- c("烟酰胺", "原液", "精华液", "玻尿酸", "三文鱼", "鲑鱼卵", "鱼籽", "安瓶", "水光针", "神仙水", "啫喱")#原生液/肌能
feature_11_exskin <- matrix(nrow = 707, ncol = length(feature_word_exskin))
colnames(feature_11_exskin) <- feature_word_exskin


engine_new_word <- worker()
new_user_word(engine_new_word, feature_word_exskin)

for(j in 1:707){
  for(i in 1:length(feature_word_exskin)){
    if(sum(feature_word_exskin[i] == segment(mois_11_modi$TITLE[j], engine_new_word)) != 0){
      feature_11_exskin[j,i] <- 1
    }else{
      feature_11_exskin[j,i] <- 0
    }
  }
}
feature_11_exskin
mois_11_modi[52,]$TITLE
kmodes_res <- kmodes(data = feature_11_exskin, 3, iter.max = 10, weight = F)
mois_11_modi$cluster <- kmodes_res$cluster
kmodes_res
mois_11_modi %>%
  filter(cluster == 2) %>%
  dplyr::select(URL)


####
https://item.taobao.com/item.htm?id=630651024019
https://item.taobao.com/item.htm?id=605866177650
https://item.taobao.com/item.htm?id=616812914797
https://item.taobao.com/item.htm?id=585621180727
https://item.taobao.com/item.htm?id=570773533857
https://item.taobao.com/item.htm?id=606568196436
https://item.taobao.com/item.htm?id=569917037400#组合
https://item.taobao.com/item.htm?id=629590404977#
https://item.taobao.com/item.htm?id=630869153322
https://item.taobao.com/item.htm?id=629987558544#
https://item.taobao.com/item.htm?id=621764584817#套装
https://item.taobao.com/item.htm?id=614086530573#
https://detail.tmall.com/item.htm?id=628750331992#
https://item.taobao.com/item.htm?id=601909402912
https://item.taobao.com/item.htm?id=631065361050#神金水
https://item.taobao.com/item.htm?id=629453254009
https://item.taobao.com/item.htm?id=557263377503#套装
https://item.taobao.com/item.htm?id=631171442891
https://item.taobao.com/item.htm?id=631016497485#神金水
https://item.taobao.com/item.htm?id=624010709188








 

skinkeyword
ffff <- ff(mois_11, skinkeyword)
ffff
rm(ffff)





x.keyword <- paste('x', 1:3, sep = "")
key.words <- c("爽肤水", "保湿水","润肤水")
for(i in 1:3){
  assign(x.keyword[i],key.words[i])
}







mois_seg_list[[1]] == c("爽肤水") | mois_seg_list[[1]] ==  c("保湿水") | mois_seg_list[[1]] ==  c("柔肤水") | 
  mois_seg_list[[1]] ==  c("护肤水") | mois_seg_list[[1]] ==  c("润肤水")


x1 <- mois_seg_list[[1]] ==  c("爽肤水")
x2 <- mois_seg_list[[1]] ==  c("保湿水")
x3 <- mois_seg_list[[1]] ==  c("柔肤水")
x4 <- mois_seg_list[[1]] ==  c("护肤水")
x5 <- mois_seg_list[[1]] ==  c("润肤水")

x.m <- rbind(x1,x2,x3,x4,x5)

apply(x.m, 1, sum)





x.keyword <- paste('x', 1:length(key.words), sep = "")
y.bool    <- paste('y', 1:length(key.words), sep = "")) 
for(i in 1:length(key.words)){
  assign(x.keyword[i], key.words[i])
}


######
filtering_vector <- c()
for(j in 1:row_n){
  y.bool.matrix <- matrix(NA, nrow = length(x.keyword), ncol = length(segment.list[[j]]))
  for(i in 1:length(key.words)){
    y.bool[i,]  <- segment.list[[j]] == key.words[i]
  }
  if(sum(y.bool.matrix) != 0){
    filtering_vector[j] <- j
  }
}




#################################################################


feature_11_exskin

library(flexmix)

# Fit Bernoulli mixture model
set.seed(2020)
bernoulli_mix_model <- flexmix(feature_11_exskin ~ 1,
                               k = 3,
                               model = FLXMCmvbinary(),
                               control = list(tolerance = 0.005, iter.max = 1000))

prior(bernoulli_mix_model)

#Remember that you can call prior() to check the proportions of each cluster.


# Extract the parameters for each cluster
param_comp_1 <- parameters(bernoulli_mix_model, component = 1)
param_comp_2 <- parameters(bernoulli_mix_model, component = 2)
param_comp_3 <- parameters(bernoulli_mix_model, component = 3)

param_comp_1
param_comp_2
param_comp_3



flexmix::clusters(bernoulli_mix_model)
flexmix::summary(bernoulli_mix_model)

feature_11_exskin[,12] <- as.vector(flexmix::clusters(bernoulli_mix_model))

feature_11_exskin <- cbind(feature_11_exskin, as.vector(flexmix::clusters(bernoulli_mix_model)))
colnames(feature_11_exskin) <- c(feature_word_exskin, "clusters")
head(feature_11_exskin)
table(bernoulli_mix_model@cluster)
feature_11_exskin <- cbind(feature_11_exskin, bernoulli_mix_model@posterior$scaled)
colnames(feature_11_exskin) <- c(feature_word_exskin, "clusters","clusters1","clusters2","cluster3")
as.data.frame(feature_11_exskin) %>% dplyr::filter(clusters == 1)


mois_11_modi_gmm <- mois_11_modi[,1:2]
mois_11_modi_gmm$clusters <- as.vector(flexmix::clusters(bernoulli_mix_model))
mois_11_modi_gmm <- cbind(mois_11_modi_gmm, bernoulli_mix_model@posterior$scaled)
mois_11_modi_gmm %>% dplyr::filter(clusters == 1)

#wrong in cluster1
https://item.taobao.com/item.htm?id=629652797327  0.7471316
https://item.taobao.com/item.htm?id=618892058879  0.9999610
https://item.taobao.com/item.htm?id=5103843348    0.8696217
https://item.taobao.com/item.htm?id=601708973394  0.8696217
https://item.taobao.com/item.htm?id=631536492685  0.8652067

https://item.taobao.com/item.htm?id=571242443104  0.8652067
https://item.taobao.com/item.htm?id=564220165118 0.8652067
https://item.taobao.com/item.htm?id=37265373073  0.7471316
https://item.taobao.com/item.htm?id=626305424839  0.7471316
https://item.taobao.com/item.htm?id=23366616741  0.9939069
https://item.taobao.com/item.htm?id=618278479479 0.8834471
https://item.taobao.com/item.htm?id=598128249492 0.7471316
https://item.taobao.com/item.htm?id=632102350170 0.7471316


#wrong in cluster1 (delete 爽肤化妆水)

https://item.taobao.com/item.htm?id=585621180727  0.8696217
https://item.taobao.com/item.htm?id=611907537135  0.8652067
https://item.taobao.com/item.htm?id=614086530573  0.8696217
https://item.taobao.com/item.htm?id=599899240229  0.8696217
https://item.taobao.com/item.htm?id=526282095758  0.8696217
https://item.taobao.com/item.htm?id=618736329486  0.9999610
https://item.taobao.com/item.htm?id=597464531153  0.8696217
https://item.taobao.com/item.htm?id=629652797327  0.7471316
https://item.taobao.com/item.htm?id=618892058879  0.9999610
https://item.taobao.com/item.htm?id=624863768438  0.8696217
https://item.taobao.com/item.htm?id=558776655532  0.8652067
https://item.taobao.com/item.htm?id=601708973394  0.8696217
https://item.taobao.com/item.htm?id=570165268598  0.8696217
https://item.taobao.com/item.htm?id=571242443104  0.8652067
https://item.taobao.com/item.htm?id=564220165118  0.8652067
https://item.taobao.com/item.htm?id=37265373073   0.7471316
https://item.taobao.com/item.htm?id=618278479479  0.8834471
https://item.taobao.com/item.htm?id=598128249492  0.7471316
https://item.taobao.com/item.htm?id=632102350170  0.7471316





### cluster == 3
mois_11_modi_gmm %>% dplyr::filter(clusters == 3) %>% View()
mois_11_modi_gmm %>% dplyr::filter(clusters == 3) %>% dim()


https://item.taobao.com/item.htm?id=630816049414
https://item.taobao.com/item.htm?id=630791259213
https://item.taobao.com/item.htm?id=630787821673
https://item.taobao.com/item.htm?id=630784431573
https://item.taobao.com/item.htm?id=630561033106
https://item.taobao.com/item.htm?id=630351146291
https://item.taobao.com/item.htm?id=630350585240
https://item.taobao.com/item.htm?id=630305944285
https://item.taobao.com/item.htm?id=630284888082
https://item.taobao.com/item.htm?id=630216044774
https://item.taobao.com/item.htm?id=629987558544
https://item.taobao.com/item.htm?id=628368841645
https://item.taobao.com/item.htm?id=626699001004
https://item.taobao.com/item.htm?id=626558171856
https://item.taobao.com/item.htm?id=625730557596



#####
set.seed(2020)
bernoulli_mix_model_2 <- flexmix(feature_11_exskin ~ 1,
                               k = 2,
                               model = FLXMCmvbinary(),
                               control = list(tolerance = 0.005, iter.max = 1000))
summary(bernoulli_mix_model_2)







