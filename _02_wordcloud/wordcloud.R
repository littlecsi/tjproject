library(xlsx)
setwd('d:/rwork/hahaha')
polcom_1911 <- read.xlsx('2019_comment_data_politics.xlsx', sheetIndex = 1, encoding = 'UTF-8')

library(KoNLP)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(xlsx)
library(jsonlite)
library(dplyr)


  eco_data_1911_1 <- polcom_1911[2]
  colnames(eco_data_1911_1) <- c('comment')
  eco_data_1911_2 <- polcom_1911[3]
  colnames(eco_data_1911_2) <- c('comment')
  eco_data_1911_all <- rbind(eco_data_1911_1, eco_data_1911_2)
  
eco_data_1911_all

head(eco_data_1911_all)



eco_data_comment_la <- apply(eco_data_1911_all, 1, extractNoun)
eco_data_comment_la <- unlist(eco_data_comment_la)
eco_data_comment_la <- as.character.factor(eco_data_comment_la)
eco_data_comment_la <- Filter(function(x){ nchar(x) >= 2}, eco_data_comment_la)
eco_data_comment_la <- str_replace_all( eco_data_comment_la, '[0-9]', '')
eco_data_comment_la <- str_replace_all( eco_data_comment_la, "[\\.]", '')
eco_data_comment_la <- gsub("\\[","",eco_data_comment_la)
eco_data_comment_la <- gsub("\\]","",eco_data_comment_la)
eco_data_comment_la <- gsub("\\.","",eco_data_comment_la)
eco_data_comment_la <- gsub("\\‘","",eco_data_comment_la)
eco_data_comment_la <- gsub("\\’","",eco_data_comment_la)
eco_data_comment_la <- gsub("\\,","",eco_data_comment_la)

eco_data_comment_la <- gsub("경제|앵커|서울연합뉴스|투데이|머니|최대|개월|들이|분기|이상|문재|이후|서울뉴스|이재|가운데|서울경제|아경|금융|아시|세종연합뉴스|뉴스|서울연합|서울신문|나경","",eco_data_comment_la)

eco_data_comment_la <- gsub('[~!@#$%&*()_+=?<>▶]','',eco_data_comment_la)


# 정렬
eco_data_comment_wc <- table(eco_data_comment_la)

eco_data_comment_wc_top <-head(sort(eco_data_comment_wc, decreasing = T),101)
eco_data_comment_wc_top
eco_data_comment_wc_top <- eco_data_comment_wc_top[2:101]
eco_data_comment_wc_top

eco_com_rank <- as.data.frame(eco_data_comment_wc_top, stringsAsFactors = F)
colnames(eco_com_rank) <- c('word', 'freq')


eco_com_rank_20 <- eco_com_rank[1:20,]
ordering <- arrange(eco_com_rank_20, freq)$word


ggplot(data=eco_com_rank_20, aes(x=word, y=freq, fill=word, color=word)) + ylim(0,max(eco_com_rank_20$freq) + 100) + geom_col() + coord_flip() + scale_x_discrete(limit=ordering) + geom_text(aes(label=freq), hjust=-0.3)





# 워드클라우드
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)



display.brewer.all()
palette <- brewer.pal(8, 'Dark2')
palette


wordcloud(words = eco_com_rank$word, freq = eco_com_rank$freq, min.freq = 10, max.words = 50, random.order = F, rot.per = .1, colors = palette)

wordcloud2(data = eco_com_rank, fontFamily = '맑은 고딕', size = 1.5, color = 'random-light', backgroundColor = 'black')
