####################################################################################################
# Library
library(openxlsx)
library(dplyr)
library(e1071)
library(klaR)
library(kernlab)

####################################################################################################
# Variable
pathOne <- "D:/GitHub/tjproject/resources/"
sections <- c("econ", "IT", "life_cult", "politics", "soc", "world")
Csections <- c("Economy", "IT", "Life_Cult", "Politics", "Society", "World")

####################################################################################################
# Function
getData <- function(path, n) {
  df <- data.frame()
  for(i in c(1:n)) {
    dfOne <- read.xlsx(path, sheet=i, colNames=T, rowNames=F)
    df <- rbind(df, dfOne)
  }
  return(df)
}
getAllSectionData <- function(section) {
  path1 <- paste(pathOne, section, "/2018_comment_data_", section, ".xlsx", sep="")
  dfOne <- getData(path1, 2)
  
  path2 <- paste(pathOne, section, "/2019_comment_data_", section, ".xlsx", sep="")
  dfTwo <- getData(path2, 10)

  path3 <- paste(pathOne, section, "/2018_view_data_", section, ".xlsx", sep="")
  dfThree <- getData(path3, 2)

  path4 <- paste(pathOne, section, "/2019_view_data_", section, ".xlsx", sep="")
  dfFour <- getData(path4, 10)

  df <- rbind(dfOne[-5], dfTwo[-5], dfThree[-5], dfFour[-5])

  return(df)
}
getcolumns <- function(data, columns) {
    len <- length(columns)
    # cat(len, '\n')
    if(len > 1) {
        df <- data[columns[1]]
        for(i in c(2:len)) {
            df <- cbind(df, data[columns[i]])
        }
    } else { df <- data %>% select(columns) }
    return(df)
}
getTAcc <- function(table) {
    true <- table["TRUE"]
    accuracy <- as.numeric(round(true/sum(table)*100, 2))
    return(accuracy)
}

####################################################################################################
# Main

# Collect Data
Edf <- getAllSectionData(sections[1])
Idf <- getAllSectionData(sections[2])
Ldf <- getAllSectionData(sections[3])
Pdf <- getAllSectionData(sections[4])
Sdf <- getAllSectionData(sections[5])
Wdf <- getAllSectionData(sections[6])

# Get needed columns
reqCol <- c("currCmt","deleted","brokenPolicy","maleRatio","femaleRatio","X10","X20","X30","X40","X50","X60")
Edata <- getcolumns(Edf, reqCol)
Idata <- getcolumns(Idf, reqCol)
Ldata <- getcolumns(Ldf, reqCol)
Pdata <- getcolumns(Pdf, reqCol)
Sdata <- getcolumns(Sdf, reqCol)
Wdata <- getcolumns(Wdf, reqCol)

# Add its own section into a column
Edata$section <- "Economy"; Idata$section <- "IT"; Ldata$section <- "Life_Cult";
Pdata$section <- "Politics"; Sdata$section <- "Society"; Wdata$section <- "World";

# Collect into one massive column
NewsData <- rbind(Edata, Idata, Ldata, Pdata, Sdata, Wdata)
colnames(NewsData)

# Remove NAs
NewsData <- na.omit(NewsData)

# Change all Independent Variables to numeric
NewsData$currCmt <- as.numeric(NewsData$currCmt)
NewsData$deleted <- as.numeric(NewsData$deleted)
NewsData$brokenPolicy <- as.numeric(NewsData$brokenPolicy)

# Data Extract
Elen <- nrow(subset(NewsData, section == "Economy"))
Ilen <- nrow(subset(NewsData, section == "IT"))
Llen <- nrow(subset(NewsData, section == "Life_Cult"))
Plen <- nrow(subset(NewsData, section == "Politics"))
Slen <- nrow(subset(NewsData, section == "Society"))
Wlen <- nrow(subset(NewsData, section == "World"))

Ei <- sample(c(1:Elen), 0.8*Elen); Ii <- sample(c(1:Ilen), 0.8*Ilen); Li <- sample(c(1:Llen), 0.8*Llen);
Pi <- sample(c(1:Plen), 0.8*Plen); Si <- sample(c(1:Slen), 0.8*Slen); Wi <- sample(c(1:Wlen), 0.8*Wlen);

idx <- c(
    Ei,
    Ii + Elen,
    Li + Elen + Ilen,
    Pi + Elen + Ilen + Llen,
    Si + Elen + Ilen + Llen + Plen,
    Wi + Elen + Ilen + Llen + Plen + Slen
)

# Create Training and Testing Data
trData <- NewsData[ idx,]
teData <- NewsData[-idx,]

svmFormula <- section ~ .

##################################################
# vanilladot
vModel <- ksvm(svmFormula, data=trData, kernel="vanilladot")

vPred <- predict(vModel, teData)

vTable <- table(vPred, teData$section)
vTable
# vPred       Economy   IT Life_Cult Politics Society World
# Economy      2418  422       554      363     703   962
# IT            167  818       118       12      48   154
# Life_Cult     132   84       742        8     317   202
# Politics      354   23       132     3232     715   328
# Society       375   69       541      456    2347   276
# World         427  247       332      180     193  1090

vResult <- vPred == teData$section
vResultT <- table(vResult)

vAcc <- getTAcc(vResultT)
vAcc

##################################################
# rbfdot
rbfModel <- ksvm(svmFormula, data=trData, kernel="rbfdot")

rbfPred <- predict(rbfModel, teData)

rbfTable <- table(rbfPred, teData$section)
rbfTable

rbfResult <- rbfPred == teData$section
rbfResultT <- table(rbfResult)

rbfAcc <- getTAcc(rbfResultT)
rbfAcc

##################################################
# 적절한 파라미터 찾기
param <- tune(svm, Species ~ ., data=trData, gamma=2^(-1:1), cost=2^(2:4))

param$best.model
# Call:
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  4 8 16 
# 
# Number of Support Vectors:  49
