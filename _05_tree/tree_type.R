####################################################################################################
# Library
library(stringr)
library(reshape2)
library(ggplot2)

source("base/db.R")

####################################################################################################
# Variable

## Logic :
### 종속 변수 : 성별
### 독립 변수 : 카테고리, 연령

####################################################################################################
# Main
type <- 'C'
Edf <- getSectionData(sections[1], type)
Idf <- getSectionData(sections[2], type)
Ldf <- getSectionData(sections[3], type)
Pdf <- getSectionData(sections[4], type)
Sdf <- getSectionData(sections[5], type)
Wdf <- getSectionData(sections[6], type)

df <- data.frame()
df <- rbind(Edf, Idf, Ldf, Pdf, Sdf, Wdf)

nrow(df)
colnames(df)
df <- df[, -c(3, 4, 5, 7)]
df$category <- 
table(str_sub(df$NEWSID, 1, 1))
