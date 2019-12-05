####################################################################################################
# Library
library(stringr)
library(reshape2)
library(ggplot2)
library(scales)
library(class)

source("base/db.R")

### Function

# data frame에 댓글 수가 우세한 성별 수치 추가
## 0 : 남자와 여자의 비율이 같음 
## 1 : 남자가 댓글이 많음
## 2 : 여자가 댓글이 많음
addGender <- function(data) {
    data$gender <- ifelse(data[,1] > data[,2], 1, 
                          ifelse(data[,1] == data[,2], 0, 2))
    return(data)
}

addAge <- function(data) {
    data$age <- 99
    for(i in c(1:nrow(data))) {
        data[i,11] <- which.max(data[i,3:8]) * 10
    }
    return(data)
}

### Main

# data set import
type <- 'C'
S_training <- getSectionData(sections[5], 'C')
C_training <- getSectionData(sections[3], 'C')

S_testing <- getTestData(sections[5], 'C')
C_testing <- getDataWithQuery('select * from 2019_comment_data_life_cult')

# data preprocessing
S_training$cate <- substr(S_training$NEWSID, 1, 1)
S_training <- S_training[,c(-1,-7)]

S_testing$cate <- substr(S_testing$NEWSID, 1, 1)
S_testing <- S_testing[,c(-1,-7)]

C_training$cate <- substr(C_training$NEWSID, 1, 1)
C_training <- C_training[,c(-1,-7)]

C_testing$cate <- 'L'

colnames(C_testing) <- colnames(C_training)

tot_training <- rbind(S_training, C_training)
tot_testing <- rbind(S_testing, C_testing)

training <- tot_training[, c(10:18)]
testing <- tot_testing[, c(10:18)]

# 네이버의 댓글 정책으로 인한 통계가 없는 데이터 삭제
training <- subset(training, training$X30 != 0)
testing <- subset(testing, testing$X30 != 0)

# 성별 컬럼 추가
training <- addGender(training)
testing <- addGender(testing)

training <- addAge(training)
testing <- addAge(testing)

training <- training[,c(9:11)]
test <- testing[,c(9:11)]

# training : test : valid -> 3:1:1로 갖춤
idx <- sample(1:nrow(training), nrow(test))
tIdx <- sample(1:nrow(training), nrow(test) * 3)
train <- training[tIdx, ]
tmp <- training[-tIdx, ]
valid <- na.omit(tmp[idx, ])

# train 산점도 : train data와 test, valid dat와 비슷한 경향을 보임을 알 수 있다. 
plot(formula = gender ~ age, data = train, col = alpha(c('purple', 'blue'), 0.7)[train$cate], main = 'train = Classification Category')

points(formula = gender ~ age, data = valid, pch = 17, cex = 1.2, col = 'red')
points(formula = gender ~ age, data = test, pch = 15, cex = 1.2, col = 'orange')

# 입력x과 출력y 데이터로 분리

train_x <- train[, -1]
valid_x <- valid[, -1]
test_x <- test[, -1]

train_y <- train[, 1]
valid_y <- valid[, 1]
test_y <- test[, 1]

# knn 알고리즘 적용하기(k = 1)
# k = 1 일 때
knn_1 <- knn(train = train_x, test = valid_x, cl = train_y, k = 1, use.all = F)
# Error in knn(train = train_x, test = valid_x, cl = train_y, k = 1, use.all = F) : 
#     too many ties in knn
## KNN을 사용하기에 너무 많은 관계가 있는 알고리즘 에러로 인해
## 뉴스 댓글의 주 연령층, 성별 데이터로 카테고리 예측을 하기에
## 적절하지 않은 알고리즘이라고 결론지을 수 있다. 

dbDisconnectAll()
