####################################################################################################
# Library
library(stringr)
library(reshape2)
library(ggplot2)
library(scales)

source("base/db.R")

### Main

type <- 'C'
Pdf <- getSectionData(sections[4], type)
Sdf <- getSectionData(sections[5], type)

P_testing <- getTestData(sections[4], type)
S_testing <- getTestData(sections[5], type)

colnames(Pdf)

deleteCol <- function(df) {
    res <- df[,c(12:19)]
    return(res)
}

df_p <- deleteCol(Pdf)
df_s <- deleteCol(Sdf)
test_p <- deleteCol(P_testing)
test_s <- deleteCol(S_testing)

# gender : 댓글단 성별의 비율이 많은 쪽을 하나만 선택
## 남성 > 50 : 1
## 여성 > 50 : 2
## 남성 == 여성 : 0
genderFunc <- function(dff) {
    df <- dff
    df$gender <- ifelse(df$MALER > df$FEMALER, 1, 
                        ifelse(df$MALER == df$FEMALER, 0, 2))
    return(df)
}

genderFunc(df_p)

plot(formula = )

dbDisconnectAll()
