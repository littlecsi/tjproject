####################################################################################################
# Library
library(stringr)
library(reshape2)
library(ggplot2)
library(cluster)

source("base/db.R")

### Main

type <- 'C'
Edf <- getSectionData(sections[1], type)
Idf <- getSectionData(sections[2], type)
Ldf <- getSectionData(sections[3], type)
Pdf <- getSectionData(sections[4], type)
Sdf <- getSectionData(sections[5], type)
Wdf <- getSectionData(sections[6], type)

df <- data.frame()
df <- rbind(Edf, Idf, Ldf, Pdf, Sdf, Wdf)

# 군집화를 할때, Vector 메모리의 부족과,
# 덴드로그램을 그릴때 세션이 Abort되어,
# 적정 수치로 sampling 함
idx <- sample(1:nrow(df), 0.005 * nrow(df))
sampling <- df[idx, ]

# 뉴스 카테고리를 정수로 표현
# type : 경제(1), IT(2), 생활(3), 정치(4), 사회(5), 세계(6)
sampling$type <- ifelse(substr(sampling[,1], 1, 1) == 'E', 1,
                    ifelse(substr(sampling[,1], 1, 1) == 'I', 2,
                    ifelse(substr(sampling[,1], 1, 1) == 'L', 3,
                    ifelse(substr(sampling[,1], 1, 1) == 'P', 4,
                    ifelse(substr(sampling[,1], 1, 1) == 'S', 5, 6)))))
table(sampling$type)

# gender : 댓글단 성별의 비율이 많은 쪽을 하나만 선택
## 남성 > 50 : 1
## 여성 > 50 : 2
## 남성 == 여성 : 0
sampling$gender <- ifelse(sampling$MALER > sampling$FEMALER, 1, 
                    ifelse(sampling$MALER == sampling$FEMALER, 0, 2))

# age : 댓글단 연령의 비율이 많은 쪽을 하나만 선택
## (10대 : 10) ~ (60대 : 60)
max(df[1,14:19])
df[2,14:19]
tmp <- 999
res <- 999

for(i in c(1:nrow(df))) {
    
}

# smapling의 20번째 열 : 카테고리
# sampling의 21번째 열 : 성별
target <- sampling[,20:21]
# target된 데이터의 유클라디언 거리 측정
gender_type_dist <- dist(target, 'euclidean')
gender_type_res <- hclust(gender_type_dist , method="ave") # 평균 연결 방법

# 군집화된 데이터 시각화
plot(gender_type_res, hang = -1, main = 'Gender and Category Cluster Dendrogram')
rect.hclust(gender_type_res, k = 4, border = rainbow(4))

# K개로 군집 분류를 하기 위한 적정 K값 찾기
## kmeans 함수
gender_kmeans <- kmeans(target, 4)
gender_kmeans
# K-means clustering with 4 clusters of sizes 119, 72, 90, 47
# 
# Cluster means:
#     type    gender
# 1 4.487395 1.0420168
# 2 1.347222 1.0000000
# 3 2.633333 0.3444444
# 4 6.000000 0.8723404

# 1번 그룹
## 4, 5번 카테고리(정치, 사회)는 남성이 대부분 댓글 수가 더 많았지만,
## 소수의 여성이 더 많은(0.4%) 댓글을 단 기사가 있었다.
# 2번 그룹
## 1, 2번 카테고리(경제, IT)는 모든 기사가 남성의 댓글 수가 많았다.
# 3번 그룹
## 2, 3번 카테고리(IT, 생활)는 같은 비율의 성별이 댓글을 단 기사가
## 남성이 더 많은 댓글을 단 기사보다 많은 것을 볼 수 있다.
# 4번 그룹
## 6번 카테고리(세계)는 남성이 여성보다 많은 비율의 댓글을 단 기사가
## 대부분이였지만 일부 기사는 남성과 여성이 같은 비율로 댓글을 달았다.

# 성별과 뉴스 분류와의 피어슨 상관계수
cor(target, method = 'pearson')
            # type    gender
# type   1.0000000 0.1074362
# gender 0.1074362 1.0000000
## 피어슨 상관 계수는 절대값 1에 가까울 수록 상관관계가 있다고 판단
## 뉴스 카테고리와 성별의 상관관계는 0.1074362이므로,
## 서로 상관관계가 없다고 볼 수 있다.
