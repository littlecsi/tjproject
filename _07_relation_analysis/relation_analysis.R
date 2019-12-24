library(arules)

library(stringr)
library(xlsx)
library(dplyr)

library(arulesViz)
setwd('D:/빅데이터 프로젝트/econ')

library(KoNLP)


eco_data_1910 <- read.xlsx('2019_comment_data_econ.xlsx', header = T, sheetIndex = 1, encoding = 'UTF-8')
eco_data_1910_1 <- eco_data_1910[2]
colnames(eco_data_1910_1) <- c('comment')
eco_data_1910_2 <- eco_data_1910[3]
colnames(eco_data_1910_2) <- c('comment')
eco_data_1910_all <- rbind(eco_data_1910_1, eco_data_1910_2)
  

eco_data_1910_all_list_app <- apply(eco_data_1910_all, 1, extractNoun)
head(eco_data_1910_all_list_app)

eco_data_1910_all_list_app[[21]]
a <- eco_data_1910_all_list_app[[26]]
a
aa<- paste(a, collapse = ' ')
aa

write.csv(aa, 'b.csv')
write.table(aa, 'b.csv', append=T, col.names = F, row.names = F, quote = F)
write.csv(aa, 'b.csv', append=T)

length(eco_data_1910_all_list_app)
eco_data1910_all_list_app
nrow(eco_data1910_all_list_app)


a <- eco_data_1910_all_list_app[[1]]
aa <- Filter(function(x){ nchar(x) >= 2}, a)
aa <- paste(aa, collapse = ',')
# aa <- gsub("[0-9]{,2}","",aa)
aa <- gsub("\\[","",aa)
aa <- gsub("\\]","",aa)
aa <- gsub("\\.","",aa)
aa <- gsub("\\'","",aa)
aa <- gsub("\\‘","",aa)
aa <- gsub("\\’","",aa)
aa <- gsub("\\”","",aa)
aa <- gsub("\\“","",aa)
aa <- gsub('\\"',"",aa)
aa <- gsub("···",' ',aa)
aa <- gsub("\\·",' ',aa)
aa <- gsub("\\’","",aa)
aa <- gsub("\\`","",aa)

# aa <- str_extract_all(aa, '[\\w]{1,}')
aa <- gsub('[~!@#$%&*()_+=?<>▶↓]','',aa)





for (idx in 1:length(eco_data_1910_all_list_app)) {
  a <- eco_data_1910_all_list_app[[idx]]
  aa <- Filter(function(x){ nchar(x) >= 2}, a)
  aa <- paste(aa, collapse = ',')
  aa <- gsub("\\d{,2}","",aa)
  aa <- gsub("\\[","",aa)
  aa <- gsub("\\]","",aa)
  aa <- gsub("\\.","",aa)
  aa <- gsub("\\'","",aa)
  aa <- gsub("\\‘","",aa)
  aa <- gsub("\\’","",aa)
  aa <- gsub("\\”","",aa)
  aa <- gsub("\\“","",aa)
  aa <- gsub('\\"',"",aa)
  aa <- gsub("···",' ',aa)
  aa <- gsub("\\·",' ',aa)
  aa <- gsub("\\’","",aa)
  aa <- gsub("\\`","",aa)
  # aa <- str_extract_all(aa, '[\\w]{1,}')
  aa <- gsub('[\\-]','',aa)
  aa <- gsub('[∼/]','',aa)
  aa <- gsub('[~!@#$%&*()_+=?<>▶↓→↑【】]','',aa)
  aa <- gsub('단독|증가종합보|증가종합','',aa)
  

  
  write.table(aa, 'eco_data_1910_tran3.csv', append=T, col.names = F, row.names = F, quote = F)  
  
}

eco_data_filter <- read.table('eco_data_1910_tran3.csv')
head(eco_data_filter)

eco_data_filter_list <- list()
for(idx in 1:nrow(eco_data_filter)){
  eco_data_filter_list[[idx]] <- str_split(eco_data_filter[idx,], ',')
}
eco_data_filter_unlist <- unlist(eco_data_filter_list)
wordcount <- table(eco_data_filter_unlist)
wordcount <- sort(wordcount, decreasing = T)

eco_data_filter_rank <- head(wordcount, 41)
eco_data_filter_rank <- eco_data_filter_rank[2:41]
class(eco_data_filter_rank)
eco_data_filter_df <- data.frame(eco_data_filter_rank)
eco_data_filter_words <- as.vector(eco_data_filter_df[,1])

class(eco_data_filter_words)

grep(eco_data_filter_words[1], eco_data_filter[i,])

# 문자열 여부 연습
testGsub <- c('최저', '나라')
testGsub[1]
test_a <- grep(testGsub[1], eco_data_filter[1,])
test_b <- grep(testGsub[2], eco_data_filter[1,])
as.character(test_b)
class(test_b)
mode(test_b)
is.
isTRUE(test_a)
is.numeric(test_b)
is.integer(test_b)
length(test_b)


# 랭킹40 단어 없는 행 NA로 비우기
for(i in 1:nrow(eco_data_filter)){
  total <- 0
  for (k in 1:length(eco_data_filter_words)) {
    check <- grep(eco_data_filter_words[k], eco_data_filter[i,])
    total <- total + length(check)
  }
  if (total < 1) {
    eco_data_filter[i,] <- NA
  }
}
# NA행 삭제
eco_data_filter_com <- na.omit(eco_data_filter)


head(eco_data_filter)
eco_data_filter$V1


write.table(eco_data_filter_com, 'eco_data_com2.csv', append=F, col.names = F, row.names = F, quote = F)  








# 연관 분석 만들기
trans <- read.transactions('eco_data_com2.csv', sep=',')

trans # 행과 컬럼 구조를 알려 준다.
image(trans)
summary(trans) 

myframe <- as(trans, 'data.frame')
head(myframe,8)
myframe


# apriori 알고리즘을 이용하여 rules 객체에 저장
rules <- apriori(myframe, parameter=list(supp=0.2, conf=0.1))
sink('eco_data_1910_tran.csv.txt')
inspect( rules )
sink()

# 제품은 희소 행렬에 알파벳 순으로 정렬이 되어 있다.
# alcohol은 39.57%, bakery는 42.87% 정도 거래 되었다.
itemFrequency(trans[,1:10])

itemFrequencyPlot(trans, support = 0.1)

itemFrequencyPlot(trans, topN = 10, ylim=c(0, 0.5), col=rainbow(10))

# 정렬
inspect(sort(rules, by = "lift", decreasing = FALSE))

inspect(sort(rules, by = "lift", decreasing = TRUE))

inspect(sort(rules, by = c("lift", "support"), decreasing = c(TRUE, FALSE)))

# 안되는 듯..
inspect(sort(rules, by = c("lift", "support"), decreasing = c(TRUE, TRUE)))

# 슬라이싱
inspect(sort(rules, by = "lift")[1:5])

# 필터링
subset_rules <- subset(rules, items %in% "정부")

inspect(rules) # 원본 갯수 확인
inspect(subset_rules) # 필터링된 것 갯수 확인

filter01 <- subset(rules, rhs %in% '정부') 
filter01 
inspect(filter01) # 규칙 확인
plot(rules, method="graph")
plot(filter01, method="scatterplot") 

# %pin% : ~ 가 포함된 규칙만....
filter02 <- subset(rules, rhs %pin% 'o') # 알파벳 o가 포함되어 있는 ....
filter02 # set of 10 rules
inspect(filter02) # 규칙 확인

filter03 <- subset(rules, lhs %in% c('snack','frozen')) 
filter03 # set of 4  rules
inspect(filter03) # 규칙 확인
plot(filter03, method="graph") 

# 지지도와 신뢰도, 향상도에 대한 산포도 그래프
plot(rules, method='scatterplot')
# savePlot('패키지 추천 산포도 그래프.png', type='png')

# 가로축(조건 : X 아이템)과 세로축(결과 : Y 아이템)
plot(rules, method='grouped')
# savePlot('패키지 추천 매트릭스 그래프.png', type='png')

# 각 규칙별로 어떠한 아이템들이 연관되어 있는가를 보여 주는 네트워크 그래프
plot(rules, method='graph', control=list(types='items'))
# savePlot('패키지 추천 네트워크 그래프.png', type='png')

# 팝업창을 띄워 준다.
plot(rules, method='graph', interactive= TRUE, control = list(type='items'))
# savePlot('패키지 추천 네트워크 팝업 그래프.png', type='png')

# CSV 파일에 규칙 쓰기
write(rules, file = "myrules.basket.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

# 규칙들을 데이터 프레임으로 변환
rules_df <- as(rules, "data.frame")
str(rules_df)
rules_df


# 단계 : 연관 규칙 해석
# 단일 item에서 발생 확률이 가장 높은 아이템은 ?
# 단일 item에서 발생 확률이 가장 낮은 아이템은 ?
# 가장 발생 가능성이 높은 <2개 상품 간>의 연관 규칙?
# 가장 발생 가능성이 높은 <2개 상품 이상에서> <3개의 상품으로>의 연관 규칙?
