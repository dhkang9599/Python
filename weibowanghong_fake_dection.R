##rweibo
install.packages('rvest')
library(rvest)
URLs <-  'https://m.weibo.cn/api/container/getIndex?type=uid&value=1757744065&containerid=1076031757744065'
web <- read_html(URLs, encoding = "UTF-8")



require(jsonlite)

wbwatcher <- function(jsonURL) {
  ### read json file for weibo lists
  
  json <-  readLines(jsonURL, encoding = 'UTF-8')
  
  ### format the json file fetched
  json <- prettify(json)
  writeLines(json,"C:/Users/Daniel Kang/Desktop/json.txt")
  
  page   <- readLines('C:/Users/Daniel Kang/Desktop/json.txt')
  starts <- grep('"card_type": 9,',page)
  ends   <- grep('"show_type": 0',page)
  
  
  topwb <- length(grep('"text": "置顶",',page))!=0
  db    <- matrix(nrow = length(starts),ncol = 8)
  if (topwb) {db <- matrix(nrow = length(starts)-1,ncol = 8)}
  colnames(db) <- c('weibo_ID', 'weibo_text', 'timestamp', 'repost', 'comment', 'like', 'isRepost', 'fromChaoHua')
  
  
  
  #### fill in the data
  for (i in 1:length(starts)){
    weibo <- page[starts[i]:ends[i]]
    
    # remove pinned weibo
    if (length(grep('"text": "置顶",',weibo))!=0) {next}
    
    # correct i for skipped weibos
    rowdb <- length(db[,1])-(length(starts)-i)
    
    # get attributes
    wbid    <- gsub('                    "bid": "','',weibo[grep('                    "bid": "',weibo)])
    wbid    <- gsub(',|"|\\s','',wbid)
    wbtext  <- gsub('                    "raw_text": "','',weibo[grep('"raw_text": "',weibo)])
    wbtext  <- gsub('",','',wbtext)
    repo    <- gsub("[^0-9]",'',weibo[grep('"reposts_count": ',weibo)])
    comment <- gsub("[^0-9]",'',weibo[grep('"comments_count": ',weibo)])
    like    <- gsub("[^0-9]",'',weibo[grep('"attitudes_count": ',weibo)])
    if (length(grep('                        "retweeted": 1,',weibo))>0) {repost <- 1} else {repost <- 0}
    if (grepl('超话',weibo[grep('                   "source": ',weibo)][1])) {fromch <- 1} else {fromch <- 0}
    
    # if a weibo is a repost, keep only the data from the user being observed
    db[rowdb,1] <- as.character(wbid[length(wbid)])
    db[rowdb,2] <- wbtext[length(wbtext)]
    db[rowdb,3] <- as.numeric(Sys.time())
    db[rowdb,4] <- repo[length(repo)]
    db[rowdb,5] <- comment[length(comment)]
    db[rowdb,6] <- like[length(like)]
    db[rowdb,7] <- repost
    db[rowdb,8] <- fromch
  }
  return(db)
}




URLs <-c( 
  "https://m.weibo.cn/api/container/getIndex?type=uid&value=1281047653&containerid=1076031281047653", # 阿花花酱
  "https://m.weibo.cn/api/container/getIndex?type=uid&value=2610441361&containerid=1076032610441361", # 博妞
  "https://m.weibo.cn/api/container/getIndex?type=uid&value=2035812517&containerid=1076032035812517", # 波克比
  "https://m.weibo.cn/api/container/getIndex?type=uid&value=1993670563&containerid=1076031993670563" # 易烫YCC
)
names <- c("ahua", "boniu", "bokebi", "YCC")

urltable <- data.frame(cbind(names, URLs))

while (Sys.time() < "2020-12-10 18:50:17 KST") { # define stop time
  for (i in 1:length(urltable$URLs)) {
    URL=urltable$URLs[i]
    if (length(grep('m.weibo.cn/statuses/show?id=',URL,fixed = T))!=0) {
      try({                                        # use try() to prevent breaks from errors caused by network failure
        db=wbwatcher(URL)
      })  
      if (!is.na(db[1,1])) {
        print(paste('Success!', 'Checked', urltable$names[i], ' at',Sys.time()))
      }
    }
    else {
      try({
        db=wbwatcher(URL)   # another similar function, used to watch a single weibo only
      })  
      if (!is.na(db[1,1])) {
        print(paste('Success!', 'Checked', urltable$names[i], ' at',Sys.time()))
      }
    }
    db=as.table(db)
    if (!is.na(db[1,1])) {
      db=cbind(rep(urltable$names[i],length(db[,1])),db)
      colnames(db)[1]='name'
      write.table(db, "C:/Users/Daniel Kang/Desktop/工作相关/爬虫/db_sisters.csv", sep = ",", col.names = !file.exists("C:/Users/Daniel Kang/Desktop/工作相关/爬虫/db_sisters.csv"), row.names = F, append = T)
    }
    else {print(paste('Error! at',Sys.time()))}
    Sys.sleep(10)     # Pause query to prevent being banned by weibo
  }
}




db <- read.csv("C:/Users/Daniel Kang/Desktop/工作相关/爬虫/db_sisters.csv", header = T)

db %>% 
  filter(weibo_text == weibo_text[18]) %>%
  dplyr::select(timestamp, like) %>%
  mutate('time' = ymd_hms(anytime(timestamp))) %>%
  mutate('interval' = c(NA,as.numeric(diff(time)))) %>%
  mutate('inc_like' = c(NA, diff(like)))



db %>% 
  dplyr::select(weibo_text) %>% 
  unique() %>% length()