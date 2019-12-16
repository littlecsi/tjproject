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
df <- df[, c(6, 10:18)]

# 군집화를 할때, Vector 메모리의 부족과,
# 덴드로그램을 그릴때 세션이 Abort되어,
# 적정 수치로 sampling 함
idx <- sample(1:nrow(df), 0.005 * nrow(df))
df <- df[idx, ]

# 30대가 0인 경우는 댓글 수가 100개 미만인 경우,
# 따라서, 통계가 있는 경우만 따로 골라온다.
df <- subset(df, df$X30 != 0)
df <- changeNum(df)

# 뉴스 카테고리를 정수로 표현
# type : 경제(1), IT(2), 생활(3), 정치(4), 사회(5), 세계(6)
df$type <- ifelse(df$cate == 'E', 1,
                    ifelse(df$cate == 'I', 2,
                    ifelse(df$cate == 'L', 3,
                    ifelse(df$cate == 'P', 4,
                    ifelse(df$cate == 'S', 5, 6)))))
table(df$type)
sampling <- df

# smapling의 2 ~ 9 컬럼 : 성별과 나이
target <- sampling[,2:9]
# target된 데이터의 유클라디언 거리 측정
gender_type_dist <- dist(target, 'euclidean')
gender_type_res <- hclust(gender_type_dist , method="ave") # 평균 연결 방법

K <- 6
# 군집화된 데이터 시각화
plot(gender_type_res, hang = -1, main = 'Gender and Category Cluster Dendrogram')
rect.hclust(gender_type_res, k = K, border = rainbow(K))

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

# 위 plot에서 완만해 지는 부분이 7로 보임
# elbow point를 7로 설정
elbow <- 7

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
rect.hclust(gender_type_res_2, k = K, border = rainbow(K))

# 군집수 자르기
ghc <- cutree(gender_type_res, k = K) # 군집 분석 결과를 대상의 6개의 군집수를 지정한다.
# 관측치를 대상으로 6개의 군집수를 지정하여 군집을 의미하는 숫자(1~6)가 출력이 된다.

unique(ghc) # [1] 6

# sampling 데이터 셋에 ghc 컬럼 추가
sampling$ghc <- ghc
table(sampling$ghc ) # 빈도수

head(sampling)

# 요약 통계량 구하기
# 군집 요약 통계량

for(i in c(1:6)) {
    g1 <- subset(sampling, ghc == i)
    print(summary(g1[2:9]))
}

# 성별과 뉴스 분류와의 피어슨 상관계수
cor(t(target), method = 'pearson')
#               MALER     FEMALER         X10         X20        X30
# MALER    1.00000000 -0.99996841 -0.06655713 -0.04931915 -0.1290393
# FEMALER -0.99996841  1.00000000  0.06643965  0.04919487  0.1287953
# X10     -0.06655713  0.06643965  1.00000000  0.76618473  0.2262531
# X20     -0.04931915  0.04919487  0.76618473  1.00000000  0.4771513
# X30     -0.12903925  0.12879525  0.22625310  0.47715127  1.0000000
# X40     -0.12132166  0.12139266 -0.50806863 -0.65122317 -0.1945207
# X50      0.13757468 -0.13728053 -0.49364570 -0.71369442 -0.8652892
# X60      0.22121508 -0.22114887 -0.39481511 -0.56112870 -0.8036456
#               X40        X50         X60
# MALER   -0.12132166  0.1375747  0.22121508
# FEMALER  0.12139266 -0.1372805 -0.22114887
# X10     -0.50806863 -0.4936457 -0.39481511
# X20     -0.65122317 -0.7136944 -0.56112870
# X30     -0.19452069 -0.8652892 -0.80364556
# X40      1.00000000  0.1669426 -0.04892678
# X50      0.16694261  1.0000000  0.83849902
# X60     -0.04892678  0.8384990  1.00000000
