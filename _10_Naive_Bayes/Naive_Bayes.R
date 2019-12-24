
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
pol_review_filterword <- c('간나새끼','개간나','종간나','쌍간나','좆간나','갈보','걸레','창녀','개간년','개나리','개년','개돼지','개새끼','좆','좃같은','족같은','개쓰레기','개소리','개씨발','개씹할','개씹','개좆','개지랄','개지랄','개족새','개차반','거렁뱅이','과메기','광녀','광년이','괴뢰','괴뢰군','그지깽깽이','급식충','김치녀','꺼벙이','꼬붕','꼰대','꼴통','대가리','연놈','노슬아치','논다니','느개비','느금마','니미럴','느금마','니기미','닝기미','니상년','새꺄','새1끼','나가뒤져','닭','뒈져','뒤져','등신','따까리','딸딸','떨거지','또라이','돌아이','똘빡','똘','똘추','똥','빡대','맘충','멍청이','머저리','무뇌','무뇌충','미친개','병신','보슬','보슬아치','보전깨','보지','보지년','보추','불알','부랄','불한당','븅딱','빠가','애미','애비','애미애비','빨갱이','빨통','떡','법규','뻐큐','빡대가리','새끼','십장생','썅','쓰레기','씨발','십할','씌발','시발','씨이발','씨발놈','씨발년','씨발새끼','씨방새','씨방놈','씨방년','씨방짭새','씨부랄','시부랄','시브랄','씨브랄','씹','씹새끼','씹쌔','씹창','씹년','싸이코','사이코','뒤졌네','씹치남','씹치녀','암닭년','암탉','아다','아가리','여물','애자','애비충','씹선비','선비','양아치','얼간이','엠창','니미씨발','엠창인생','염병','옘병','우라질','운지','육변기','육시럴','니애비','니애미','자지','저능아','조센징','ㅈㄴ','졸라','ㅈ1ㄴ','ㅈ ㄴ','ㅈㄹ','ㅈ ㄹ','ㅈ1ㄹ','종간나새끼','좆간나새끼','좃새끼','좃간나새끼','좆','좆까','좆까라','좆망','좆밥','좆대가리','좆집','쥐새끼','지랄','짭새','찐따','틀딱충','한남','한남충','호로','호로새끼','홍어','화냥년','후장','똥꼬','후빨')




pol_review_filterword[125]
class(pol_review_filter[4,1])



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

