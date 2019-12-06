####################################################################################################
# Library
library(stringr)
library(reshape2)
library(ggplot2)
library(caret)
library(progress)
library(kernlab)

source("base/db.R")

####################################################################################################
# function
getTAcc <- function(PredTable) {
  len <- nrow(PredTable)
  total <- 0
  for(i in c(1:len)) {
    total <- total + PredTable[i,i]
  }
  return(round(total / sum(PredTable) * 100, 2))
}

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
svm_df <- df[,-c(1, 2, 3, 4, 5, 6, 7)]
# tree_df <- tree_df %>% subset(NEWSRANK %in% c(1,2,3,4,5))

# Get real values of comments
pb <- progress_bar$new(
  format="[:bar] :current/:total (:percent)", total=nrow(svm_df)
)

pb$tick(0)
for(i in c(1:nrow(svm_df))) {
  pb$tick(1)
  # 5 ~ 12
  for(j in c(5:12)) {
    svm_df[i,j] <- round(svm_df[i,1] * svm_df[i,j] / 100, 2)
  }
}

# Extracting Independent Variables
svmData <- svm_df[,-c(1,2,3,4)]

# Create Training and Testing Data
idx <- createDataPartition(y=svmData$cat, p=0.8, list=F)
train <- svmData[idx,]
test <- svmData[-idx,]

svmFormula <- cat ~ .

##################################################
# vanilladot
vModel <- ksvm(svmFormula, data=train, kernel="vanilladot")

vPred <- predict(vModel, test)

vTable <- table(vPred, test$cat)
vTable

vAccuracy <- getTAcc(vTable)
vAccuracy

##################################################
# rbfdot
rbfModel <- ksvm(svmFormula, data=train, kernel="rbfdot")

rbfPred <- predict(rbfModel, test)

rbfTable <- table(rbfPred, test$cat)
rbfTable

rbfAccuracy <- getTAcc(rbfTable)
rbfAccuracy

##################################################
# 적절한 파라미터 찾기
param <- tune(svm, Species ~ ., data=train, gamma=2^(-1:1), cost=2^(2:4))

param$best.model
# Call:
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  4 8 16 
# 
# Number of Support Vectors:  49
