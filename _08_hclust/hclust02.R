####################################################################################################
# Library
library(stringr)
library(reshape2)
library(ggplot2)
library(cluster)

source("base/db.R")

### Function
changeNum <- function(df) {
    # Get real values of comments
    pb <- progress_bar$new(
        format="[:bar] :current/:total (:percent)", total=nrow(df)
    )
    
    pb$tick(0)
    for(i in c(1:nrow(df))) {
        pb$tick(1)
        # 5 ~ 12
        for(j in c(2:9)) {
            df[i,j] <- round(df[i,1] * df[i,j] / 100, 2)
        }
    }
    return(df)
}

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

df$cate <- substr(df$NEWSID, 1, 1)
df <- df[,c(-1,-7)]

# 군집화를 할때, Vector 메모리의 부족과,
# 덴드로그램을 그릴때 세션이 Abort되어,
# 적정 수치로 sampling 함
idx <- sample(1:nrow(df), 0.01 * nrow(df))
df <- df[idx, ]

# target words
words <- c('날씨', '먼지', '서울', '오늘', '전국', '논란', '태풍', '아침', '배우', '오후', '주말', '현대', '국내', '공기', '기온', '여성', '영화', '추위', '방탄')

pb <- progress_bar$new(
    format="[:bar] :current/:total (:percent)", total=nrow(df)
)

df02 <- data.frame()
for(i in c(1:nrow(df))) {
    for(j in c(1:length(words))) {
        if(grep(words[1], df$TITLE) != 0) {
            df02 <- rbind(df02, df[i,])
            break
        }
    }
    pb$tick()
}
colnames(df02)
df02 <- df02[, c(6, 10:18)]


# 30대가 0인 경우는 댓글 수가 100개 미만인 경우,
# 따라서, 통계가 있는 경우만 따로 골라온다.
df02 <- subset(df02, df02$X30 != 0)
df02 <- changeNum(df02)

# 뉴스 카테고리를 정수로 표현
# type : 경제(1), IT(2), 생활(3), 정치(4), 사회(5), 세계(6)
df02$type <- ifelse(df02$cate == 'E', 1,
                  ifelse(df02$cate == 'I', 2,
                         ifelse(df02$cate == 'L', 3,
                                ifelse(df02$cate == 'P', 4,
                                       ifelse(df02$cate == 'S', 5, 6)))))
table(df02$type)
sampling <- df02

# smapling의 2 ~ 9 컬럼 : 성별과 나이
target <- sampling[,2:9]
# target된 데이터의 유클라디언 거리 측정
gender_type_dist <- dist(target, 'euclidean')
gender_type_res <- hclust(gender_type_dist , method="ave") # 평균 연결 방법

# 군집화된 데이터 시각화
K <- 6
plot(gender_type_res, hang = -1, main = 'Gender and Category Cluster Dendrogram')
rect.hclust(gender_type_res, k = K, border = rainbow(6))

#Elbow point구하기 
wss <- 0
# k를 1~10까지 변화시키면서 각 withinss 값을 wss에 저장한다.
maxlen <- 10
for(i in 1:maxlen){
    wss[i] <- sum(kmeans(target, centers = i)$withinss)
} 
wss

barplot(wss, col=rainbow(maxlen))

plot(1:10, wss, type="b",xlab = "Number of Clusters", ylab = "Within group sum of squares")

# 위 plot에서 완만해 지는 부분이 4로 보임
# elbow point를 4로 설정
elbow <- 4

# k-means군집 
# library(graphics)
kms <- kmeans(target , elbow)
kms

plot(target , col = kms$cluster)

# Transpose : 군집을 위한 t()
target <- t(sampling[,2:9])
# target된 데이터의 유클라디언 거리 측정
gender_type_dist <- dist(target, 'euclidean')
gender_type_res_2 <- hclust(gender_type_dist , method="ave") # 평균 연결 방법

# 군집화된 데이터 시각화
plot(gender_type_res_2, hang = -1, main = 'Gender and Category Cluster Dendrogram')
rect.hclust(gender_type_res_2, k = K, border = rainbow(6))


# 군집수 자르기
ghc <- cutree(gender_type_res, k = K) # 군집 분석 결과를 대상의 6개의 군집수를 지정한다.
# 관측치를 대상으로 6개의 군집수를 지정하여 군집을 의미하는 숫자(1~6)가 출력이 된다.

unique(ghc) # [1] 6

# sampling 데이터 셋에 ghc 컬럼 추가
sampling$ghc <- ghc
table(sampling$ghc ) # 빈도수

head(sampling)

# 요약 통계량 구하기
g1 <- subset(sampling, ghc == 1 )
summary(g1[2:9]) # 제 1군집 요약 통계량
# MALER            FEMALER             X10               X20              X30               X40         
# Min.   :   2.44   Min.   :   1.62   Min.   :  0.000   Min.   :  2.62   Min.   :  10.78   Min.   :  11.25  
# 1st Qu.: 185.37   1st Qu.:  48.96   1st Qu.:  0.000   1st Qu.: 25.09   1st Qu.:  70.14   1st Qu.:  77.53  
# Median : 443.24   Median : 125.34   Median :  2.850   Median : 49.12   Median : 140.43   Median : 196.88  
# Mean   : 619.23   Mean   : 200.39   Mean   :  6.911   Mean   : 76.25   Mean   : 201.11   Mean   : 269.50  
# 3rd Qu.: 859.51   3rd Qu.: 279.20   3rd Qu.:  9.790   3rd Qu.: 99.06   3rd Qu.: 271.37   3rd Qu.: 372.49  
# Max.   :2435.22   Max.   :1813.08   Max.   :166.720   Max.   :630.58   Max.   :1323.92   Max.   :1340.04  
# X50              X60        
# Min.   :  1.92   Min.   :  0.65  
# 1st Qu.: 43.92   1st Qu.: 15.52  
# Median :109.16   Median : 39.94  
# Mean   :183.51   Mean   : 82.00  
# 3rd Qu.:255.23   3rd Qu.:118.14  
# Max.   :897.26   Max.   :469.87 

g2 <- subset(sampling, ghc == 2 )
summary(g2[2:9])
# MALER         FEMALER            X10              X20              X30              X40        
# Min.   :2428   Min.   : 514.6   Min.   :  0.00   Min.   : 153.7   Min.   : 373.8   Min.   : 872.2  
# 1st Qu.:2890   1st Qu.: 741.1   1st Qu.:  0.00   1st Qu.: 221.9   1st Qu.: 666.6   1st Qu.:1138.9  
# Median :3308   Median : 998.6   Median : 35.59   Median : 325.1   Median : 965.2   Median :1466.3  
# Mean   :3494   Mean   :1034.4   Mean   : 30.46   Mean   : 384.4   Mean   : 974.0   Mean   :1451.0  
# 3rd Qu.:3694   3rd Qu.:1186.9   3rd Qu.: 46.91   3rd Qu.: 456.7   3rd Qu.:1192.2   3rd Qu.:1707.5  
# Max.   :5647   Max.   :2330.2   Max.   :108.62   Max.   :1262.2   Max.   :1994.7   Max.   :2273.2  
# X50              X60        
# Min.   : 494.0   Min.   : 145.9  
# 1st Qu.: 940.2   1st Qu.: 398.6  
# Median :1086.2   Median : 491.7  
# Mean   :1152.3   Mean   : 535.1  
# 3rd Qu.:1303.4   3rd Qu.: 685.3  
# Max.   :2072.7   Max.   :1054.1 

g3 <- subset(sampling, ghc == 3 )
summary(g3[2:9])
# MALER         FEMALER          X10             X20            X30            X40            X50      
# Min.   :9855   Min.   :2779   Min.   :126.3   Min.   :1769   Min.   :3538   Min.   :3664   Min.   :2527  
# 1st Qu.:9855   1st Qu.:2779   1st Qu.:126.3   1st Qu.:1769   1st Qu.:3538   1st Qu.:3664   1st Qu.:2527  
# Median :9855   Median :2779   Median :126.3   Median :1769   Median :3538   Median :3664   Median :2527  
# Mean   :9855   Mean   :2779   Mean   :126.3   Mean   :1769   Mean   :3538   Mean   :3664   Mean   :2527  
# 3rd Qu.:9855   3rd Qu.:2779   3rd Qu.:126.3   3rd Qu.:1769   3rd Qu.:3538   3rd Qu.:3664   3rd Qu.:2527  
# Max.   :9855   Max.   :2779   Max.   :126.3   Max.   :1769   Max.   :3538   Max.   :3664   Max.   :2527  
# X60      
# Min.   :1011  
# 1st Qu.:1011  
# Median :1011  
# Mean   :1011  
# 3rd Qu.:1011  
# Max.   :1011 

g4 <- subset(sampling, ghc == 4 )
summary(g4[2:9])
# MALER          FEMALER          X10         X20             X30            X40            X50      
# Min.   :11724   Min.   :3307   Min.   :0   Min.   :751.5   Min.   :2405   Min.   :4960   Min.   :4660  
# 1st Qu.:11724   1st Qu.:3307   1st Qu.:0   1st Qu.:751.5   1st Qu.:2405   1st Qu.:4960   1st Qu.:4660  
# Median :11724   Median :3307   Median :0   Median :751.5   Median :2405   Median :4960   Median :4660  
# Mean   :11724   Mean   :3307   Mean   :0   Mean   :751.5   Mean   :2405   Mean   :4960   Mean   :4660  
# 3rd Qu.:11724   3rd Qu.:3307   3rd Qu.:0   3rd Qu.:751.5   3rd Qu.:2405   3rd Qu.:4960   3rd Qu.:4660  
# Max.   :11724   Max.   :3307   Max.   :0   Max.   :751.5   Max.   :2405   Max.   :4960   Max.   :4660  
# X60      
# Min.   :2104  
# 1st Qu.:2104  
# Median :2104  
# Mean   :2104  
# 3rd Qu.:2104  
# Max.   :2104 


g5 <- subset(sampling, ghc == 5 )
summary(g5[2:9])
# MALER         FEMALER          X10             X20            X30            X40            X50      
# Min.   :9100   Min.   :5818   Min.   :149.2   Min.   :1790   Min.   :4177   Min.   :4475   Min.   :3431  
# 1st Qu.:9100   1st Qu.:5818   1st Qu.:149.2   1st Qu.:1790   1st Qu.:4177   1st Qu.:4475   1st Qu.:3431  
# Median :9100   Median :5818   Median :149.2   Median :1790   Median :4177   Median :4475   Median :3431  
# Mean   :9100   Mean   :5818   Mean   :149.2   Mean   :1790   Mean   :4177   Mean   :4475   Mean   :3431  
# 3rd Qu.:9100   3rd Qu.:5818   3rd Qu.:149.2   3rd Qu.:1790   3rd Qu.:4177   3rd Qu.:4475   3rd Qu.:3431  
# Max.   :9100   Max.   :5818   Max.   :149.2   Max.   :1790   Max.   :4177   Max.   :4475   Max.   :3431  
# X60      
# Min.   :1044  
# 1st Qu.:1044  
# Median :1044  
# Mean   :1044  
# 3rd Qu.:1044  
# Max.   :1044 

g6 <- subset(sampling, ghc == 6 )
summary(g6[2:9])
# MALER         FEMALER          X10         X20             X30            X40            X50      
# Min.   :6792   Min.   :1806   Min.   :0   Min.   :429.9   Min.   :1462   Min.   :2751   Min.   :2579  
# 1st Qu.:6792   1st Qu.:1806   1st Qu.:0   1st Qu.:429.9   1st Qu.:1462   1st Qu.:2751   1st Qu.:2579  
# Median :6792   Median :1806   Median :0   Median :429.9   Median :1462   Median :2751   Median :2579  
# Mean   :6792   Mean   :1806   Mean   :0   Mean   :429.9   Mean   :1462   Mean   :2751   Mean   :2579  
# 3rd Qu.:6792   3rd Qu.:1806   3rd Qu.:0   3rd Qu.:429.9   3rd Qu.:1462   3rd Qu.:2751   3rd Qu.:2579  
# Max.   :6792   Max.   :1806   Max.   :0   Max.   :429.9   Max.   :1462   Max.   :2751   Max.   :2579  
# X60      
# Min.   :1376  
# 1st Qu.:1376  
# Median :1376  
# Mean   :1376  
# 3rd Qu.:1376  
# Max.   :1376 

# 성별과 뉴스 분류와의 피어슨 상관계수
cor(t(target), method = 'pearson')
#           MALER   FEMALER       X10       X20       X30       X40       X50       X60
# MALER   1.0000000 0.8665240 0.5200066 0.7644753 0.9209081 0.9722520 0.9291093 0.8955840
# FEMALER 0.8665240 1.0000000 0.5274502 0.7154192 0.8909313 0.9230416 0.8505617 0.7722968
# X10     0.5200066 0.5274502 1.0000000 0.7890455 0.6407794 0.4670540 0.3106667 0.2744270
# X20     0.7644753 0.7154192 0.7890455 1.0000000 0.8775054 0.6830838 0.5247757 0.4874325
# X30     0.9209081 0.8909313 0.6407794 0.8775054 1.0000000 0.9053029 0.7577385 0.6894087
# X40     0.9722520 0.9230416 0.4670540 0.6830838 0.9053029 1.0000000 0.9430501 0.8790743
# X50     0.9291093 0.8505617 0.3106667 0.5247757 0.7577385 0.9430501 1.0000000 0.9703728
# X60     0.8955840 0.7722968 0.2744270 0.4874325 0.6894087 0.8790743 0.9703728 1.0000000

dbDisconnectAll()
