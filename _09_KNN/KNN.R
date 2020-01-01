####################################################################################################
# Library
library(stringr)
library(reshape2)
library(ggplot2)
library(scales)
library(class)
library(progress)

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

# data set import
type <- 'C'
S_training <- getSectionData(sections[5], 'C')
C_training <- getSectionData(sections[3], 'C')

# data preprocessing
S_training$cate <- substr(S_training$NEWSID, 1, 1)
S_training <- S_training[,c(-1,-7)]

C_training$cate <- substr(C_training$NEWSID, 1, 1)
C_training <- C_training[,c(-1,-7)]

tot_training <- rbind(S_training, C_training)
training <- tot_training[, c(6, 10:18)]

# 네이버의 댓글 정책으로 인한 통계가 없는 데이터 삭제
training <- subset(training, training$X30 != 0)
training <- changeNum(training)

training <- training[,c(-1)]

# # training : test : valid -> 3:1:1로 갖춤
idx <- sample(x = c("train", "valid", "test"), size = nrow(training), replace = TRUE, prob = c(3, 1, 1))

# idx에 따라 데이터 나누기
train <- training[idx == "train", ]
valid <- training[idx == "valid", ]
test <- training[idx == "test", ]


# 입력x과 출력y 데이터로 분리

train_x <- train[, -9]
valid_x <- valid[, -9]
test_x <- test[, -9]

train_y <- train[, 9]
valid_y <- valid[, 9]
test_y <- test[, 9]

# knn 알고리즘 적용하기(k = 1)
# k = 1 일 때
knn_1 <- knn(train = train_x, test = valid_x, cl = train_y, k = 1, use.all = F)

# 분류 정확도 계산하기
table(knn_1, valid_y)
# valid_y
# knn_1     L       S
# L         1125    233
# S         216     1946

accuracy_1 <- sum(knn_1 == valid_y) / length(valid_y)
accuracy_1
# [1] 0.8724432

# k = 21 일 때
knn_21 <- knn(train = train_x, test = valid_x, cl = train_y, k = 21)

# 분류 정확도 계산하기
table(knn_21, valid_y)
# valid_y
# knn_21    L       S
# L         1090    38
# S         251     2141

accuracy_21 <- sum(knn_21 == valid_y) / length(valid_y)
accuracy_21
# [1] 0.9099943

# 최적의 k 값 구해보기
# k가 1부터 train 행 수까지 변화할 때 분류 정확도 구하기
## 반복문 for 를 이용하여 k가 1부터 train 행 수까지 변화할 때, 
## 분류 정확도가 몇 % 되는지 그래프를 그려보고 최적의 k를 확인

# 분류 정확도 사전 할당
accuracy_k <- NULL

cnt <- 150
# k가 1부터 train 행 수까지 증가할 때 (반복문)
pb <- progress_bar$new(
    format="[:bar] :current/:total (:percent)", total=cnt
)

for(idx in c(1:cnt)){
    # k가 cnt일 때 knn 적용하기
    knn_k <- knn(train = train_x, test = valid_x, cl = train_y, k = idx)
    # 분류 정확도 계산하기
    accuracy_k <- c(accuracy_k, sum(knn_k == valid_y) / length(valid_y))
    pb$tick(0)
    pb$tick(1)
}

# [=========>----------------------------------------------------] 499/4188 ( 12%)
# Error in knn(train = train_x, test = valid_x, cl = train_y, k = idx) : 
#     too many ties in knn
## 500 이상의 k 값이 에러가 뜨므로, 150까지로 제한해 본다.

# k에 따른 분류 정확도 데이터 생성
valid_k <- data.frame(k = c(1:cnt), accuracy = accuracy_k)
colnames(valid_k)

# k에 따른 분류 정확도 그래프 그리기
plot(formula = accuracy ~ k, data = valid_k, type = "o", pch = 20, main = "validation - optimal k")

# 분류 정확도가 가장 높으면서 가장 작은 k의 값 구하기
sort(valid_k$accuracy, decreasing = T)
maxdata <- max(valid_k$accuracy) # 0.9119555
maxdata
# [1] 0.9196023

min_position <- min(which(valid_k$accuracy == maxdata))
min_position
# [1] 105

# min_position의 값과 그래프에서 확인해 보면 적정 k값을 확인할 수 있다.
# 그럼 이제 k가 min_position의 값을 가질 때 모델이 얼마나 분류가 잘 되는지 test 데이터를 이용해서 표현해보자.

# 최적의 k 값에 test 데이터 적용하기
knn_optimization <- knn(train = train_x, test = test_x, cl = train_y, k = min_position)

# # Confusion Matrix 틀 만들기
result <- matrix(NA, nrow = 2, ncol = 2)
rownames(result) <- paste0("real_", c('S', 'L'))
colnames(result) <- paste0("clsf_", c('S', 'L'))
result

# # Confusion Matrix 값 입력하기
result[1, 1] <- sum(ifelse(test_y == "S" & knn_optimization == "S", 1, 0))
result[2, 1] <- sum(ifelse(test_y == "L" & knn_optimization == "L", 1, 0))
result[1, 2] <- sum(ifelse(test_y == "S" & knn_optimization == "L", 1, 0))
result[2, 2] <- sum(ifelse(test_y == "L" & knn_optimization == "S", 1, 0))

result
# clsf_S clsf_L
# real_S   2226     43
# real_L   1099    261

# 최종 정확도 계산하기
table(prediction=knn_optimization, answer=test_y)
# answer
# prediction    L    S
# L 1099   43
# S  261 2226

# 해석

# 정확도
accuracy <- sum(knn_optimization == test_y) / sum(result)
accuracy
# [1] 0.9162304

dbDisconnectAll()
