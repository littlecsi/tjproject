####################################################################################################
# Library
library(tree)
library(rpart)
library(party)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caret)

library(stringr)
library(reshape2)
library(ggplot2)
library(progress)

source("base/db.R")

####################################################################################################
# Variable

# Logic :
## 종속 변수 : 카테고리
## 독립 변수 : 성별, 연령

# 과정 :
## 1. 성별과 연령대를 전체 댓글 수에 비례하도록 나눈다.
## 2. 특정 연령, 성별을 넣었을 때, 카테고리가 결정되도록 한다.

####################################################################################################
# Main
type <- 'C'
Edf <- getSectionData(sections[1], type)
Idf <- getSectionData(sections[2], type)
Ldf <- getSectionData(sections[3], type)
Pdf <- getSectionData(sections[4], type)
Sdf <- getSectionData(sections[5], type)
Wdf <- getSectionData(sections[6], type)

df <- rbind(Edf, Idf, Ldf, Pdf, Sdf, Wdf)
df$cat <- substr(df$NEWSID, 1, 1)

# Get rid of some columns in data frame
tree_df <- df[,-c(1, 2, 3, 4, 5, 6, 7)]
# tree_df <- tree_df %>% subset(NEWSRANK %in% c(1,2,3,4,5))

# Get real values of comments
pb <- progress_bar$new(
    format="[:bar] :current/:total (:percent)", total=nrow(tree_df)
)

pb$tick(0)
for(i in c(1:nrow(tree_df))) {
    pb$tick(1)
    # 5 ~ 12
    for(j in c(5:12)) {
        tree_df[i,j] <- round(tree_df[i,1] * tree_df[i,j] / 100, 2)
    }
}

# Extracting Independent Variables
treeData <- tree_df[,-c(1,2,3,4)]

# Create Training and Testing Data
idx <- createDataPartition(y=treeData$cat, p=0.8, list=F)
train <- treeData[idx,]
test <- treeData[-idx,]

##################################################
### tree package
treeMod <- tree(cat ~ ., data=train)
plot(treeMod)
text(treeMod)

cv.trees <- cv.tree(treeMod, FUN=prune.misclass)
plot(cv.trees)

##################################################
### party package
ctreeMod <- ctree(cat ~ ., data=train)
plot(treeMod)

##################################################
### rpart package
rpartMod <- rpart(cat ~ ., data=train, method="class")
rpartMod

confusionMatrix(factor(test$cat), predict(rpartMod, test, type = "class"))

nrow(test)
nrow(train)

table(predict(rpartMod, test, type = "class"), test[, "cat"])



fancyRpartPlot(rpartMod, main="rpart Tree Diagram", type=2, caption = "probability : (E, I, L, P, S, W)")
fancyRpartPlot(rpartMod, main="rpart Tree Diagram", type=5, caption = "probability : (E, I, L, P, S, W)")
