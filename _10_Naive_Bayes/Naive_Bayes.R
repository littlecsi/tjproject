
##
## 비교군 만들기
##
pol_review_unlist<- unlist(pol_review_list)
pol_review_df <- as.data.frame(pol_review_unlist)

pol_review_df_fil <- gsub('[0-9]{3,4}', '', pol_review_df[,1])
pol_review_df_fil <- gsub(' [0-9]{2}\\:[0-9]{2}', '', pol_review_df_fil)
pol_review_df_fil <- gsub('작성자에 의해 삭제된 댓글입니다.', '', pol_review_df_fil)
pol_review_df_fil <- gsub('운영규정 미준수로 인해 삭제된 댓글입니다.', '', pol_review_df_fil)

# 필터 단어
pol_review_filterword <- read.csv('filter.csv')

pol_review_filterword <- as.vector(pol_review_filterword[,2])

for(i in 1:length(pol_review_df_fil)){
  total <- 0
  for (k in 1:length(pol_review_filterword)) {
    check <- grep(pol_review_filterword[k], pol_review_df_fil[i])
    total <- total + length(check)
  }
  ifelse(total >= 1, pol_review_df_fil[i], pol_review_df_fil[i] <- NA)
  cat("\n",i,"번째 입력 완료") 
  
}

head(pol_review_df_fil_omit)

pol_review_df_fil_omit <- na.omit(pol_review_df_fil)

pol_review_filter <- data.frame(text=pol_review_df_fil_omit)

pol_review_filter$type <- ''



for(i in 1:nrow(pol_review_filter)){
  total <- 0
  for (k in 1:length(pol_review_filterword)) {
    check <- grep(pol_review_filterword[k], as.character.factor(pol_review_filter[i,1]))
    total <- total + length(check)
  }
  ifelse(total >= 1, pol_review_filter[i,]$type <- 'break', pol_review_filter[i,]$type <- 'keep')
  cat("\n",i,"번째 입력 완료") 
  
}

pol_review_filter[,3]$nchar <- nchar(as.character(pol_review_filter[1,1]))



str(pol_review_filter$type)
unique(pol_review_filter$type)
table(pol_review_filter$type)

library(tm)



# write.table(pol_review_df, 'pol_review_df.csv', row.names = F)

##########################################################################################################
## 테스트용
######################################################################################################

pol_review_test_unlist<- unlist(pol_test_review_list)
pol_review_test_df <- as.data.frame(pol_review_test_unlist)

pol_review_test_df_fil <- gsub('[0-9]{3,4}', '', pol_review_test_df[,1])
pol_review_test_df_fil <- gsub(' [0-9]{2}\\:[0-9]{2}', '', pol_review_test_df_fil)
pol_review_test_df_fil <- gsub('작성자에 의해 삭제된 댓글입니다.', '', pol_review_test_df_fil)
pol_review_test_df_fil <- gsub('운영규정 미준수로 인해 삭제된 댓글입니다.', '', pol_review_test_df_fil)







for(i in 1:length(pol_review_test_df_fil)){
  total <- 0
  for (k in 1:length(pol_review_filterword)) {
    check <- grep(pol_review_filterword[k], pol_review_test_df_fil[i])
    total <- total + length(check)
  }
  ifelse(total >= 1, pol_review_test_df_fil[i], pol_review_test_df_fil[i] <- NA)
  cat("\n",i,"번째 입력 완료") 
  
}

pol_review _test_df_fil_omit <- na.omit(pol_review_test_df_fil)

pol_review_test_filter <- data.frame(text=pol_review_test_df_fil_omit)

pol_review_test_filter$type <- ''


for(i in 1:nrow(pol_review_test_filter)){
  total <- 0
  for (k in 1:length(pol_review_filterword)) {
    check <- grep(pol_review_filterword[k], as.character.factor(pol_review_test_filter[i,1]))
    total <- total + length(check)
  }
  ifelse(total >= 1, pol_review_test_filter[i,]$type <- 'break', pol_review_test_filter[i,]$type <- 'keep')
  cat("\n",i,"번째 입력 완료") 
  
}


####################################################################################################
########################################################################################################














vecsrc <- VectorSource(pol_review_filter$text) 
corpus <- Corpus( vecsrc ) 
vecsrc_test <- VectorSource(pol_review_test_filter$text)
corpus_test <- Corpus( vecsrc_test )


clean <- tm_map(x=corpus, FUN=tolower)
clean <- tm_map(x=clean, FUN=removeNumbers)
clean_test <- tm_map(x=corpus_test, FUN=tolower)
clean_test <- tm_map(x=clean_test, FUN=removeNumbers)


clean <- tm_map(clean, removeWords, stopwords())
clean_test <- tm_map(clean_test, removeWords, stopwords())

