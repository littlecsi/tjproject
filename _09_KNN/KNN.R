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

type <- 'C'

S_training <- getSectionData(sections[5], 'C')
C_training <- getSectionData(sections[3], 'C')

S_testing <- getTestData(sections[5], 'C')
C_testing <- getDataWithQuery('select * from 2019_comment_data_life_cult')

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

# 데이터를 training : test(1396개) : valid(1396개)로 갖춤
idx <- sample(1:nrow(training), 1396)
train <- training[-idx, ]
valid <- training[idx, ]


plot(formula = gender ~ age, data = train, col = alpha(c('purple', 'blue'), 0.7)[train$cate], main = 'train = Classification Category')

points(formula = gender ~ age, data = valid, pch = 17, cex = 1.2, col = 'red')
points(formula = gender ~ age, data = test, pch = 15, cex = 1.2, col = 'orange')

legend("topright", c(levels(training$cate), "valid", "test"), pch = c(1, 17, 15), col = c(alpha(c("blue", "green"), 0.7), "red", "orange"), cex = 0.9)


dbDisconnectAll()
