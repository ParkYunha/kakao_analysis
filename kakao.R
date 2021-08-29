## 카톡 분석기

rm(list = ls())
setwd("/Users/yunha/Desktop/kakaotalk_analysis")

library(pander)
library(quanteda)
library(tidytext)
library(tibble)
library(dplyr)
library(tm)
library(stringr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)


#### enter here !!! ###
filename <- "leah"
graphname <- filename
year <-NULL     # input an integer or NULL for all
who <- NULL   # input a string or NULL for all 



#---- 데이터 불러오기 ----#
kakao <- paste(filename, ".csv", sep = "")
kakao.df <- read.csv(kakao)
  View(kakao.df)
  # str(kakao.df)




#---- 전처리 함수 ----#
preprocess <- function(year = NULL, who = NULL) {

  #--- 특정 년도만 추출
  if(!is.null(year)) {
    kakao.df <<- select(filter(kakao.df, str_detect(Date, toString(year))), 
                       c(Date, User, Message))
    graphname <<- paste(graphname, "_", toString(year), sep = "")
  }
  
  
  #--- 특정 유저만 추출
  if(!is.null(who)) {
    kakao.df <<- select(filter(kakao.df, str_detect(User, who)), 
                       c(Date, User, Message))
    graphname <<- paste(graphname, "_", who, sep = "")
  }
  
  
  #--- 불용어? 제거 
  kakao.df <<- select(filter(kakao.df, !str_detect(Message, 'ㅋㅋ')),
                     c(Message)) 
}



#---- 전처리 진행 ----#
preprocess(year, who)

kakao.stopwords <- c("이모티콘", "사진")
kakao.corpus <- VCorpus(VectorSource(kakao.df$Message))

kakao.corpus <- tm_map(kakao.corpus, content_transformer(tolower))
kakao.corpus <- tm_map(kakao.corpus, removeWords, kakao.stopwords)
kakao.corpus <- tm_map(kakao.corpus, removePunctuation)
kakao.corpus <- tm_map(kakao.corpus, removeNumbers)
kakao.corpus <- tm_map(kakao.corpus, stripWhitespace)
kakao.corpus <- tm_map(kakao.corpus, content_transformer(trimws))

# lapply(kakao.corpus, content)



#---- make document-term matrix ----#
kakao.dtm <- DocumentTermMatrix(kakao.corpus)
# inspect(kakao.dtm)

termfreq <- colSums(as.matrix(kakao.dtm))
termfreq.df <- data.frame(word = names(termfreq), frequency = termfreq)



set.seed(123)
png(paste("kakao_cloud_", graphname, ".png", sep = ""), 
    width = 700, height = 700)
wordcloud(words = names(termfreq), freq = termfreq, scale = c(6, 1), min.freq = 6,
          rot.per = 0.1, random.order = FALSE, random.color = FALSE,
          colors = brewer.pal(6, "Dark2"),
          family="AppleGothic")
dev.off()





### reset work place
setwd("/Users/yunha/Desktop/BME500_R")









