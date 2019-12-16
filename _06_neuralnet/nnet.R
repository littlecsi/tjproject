####################################################################################################
# Library
library(openxlsx)
library(dplyr)
library(devtools)
library(nnet)
library(neuralnet)

library(ggplot2)

# 시각화 R 코드 함수 다운로드
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

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
    if(len > 1) {
        df <- data %>% select(columns[1])
        for(i in c(2:len)) {
            col <- columns[i]
            df <- cbind(df, data[col])
        }
    } else { df <- data %>% select(columns) }
    return(df)
}
calcAcc <- function(compTable) {
    len <- nrow(compTable)
    total <- 0
    for(l in c(1:len)) { total <- total + compTable[l,l] }
    accuracy <- round((total / sum(compTable)) * 100, 2)
    return(accuracy)
}
normalization <- function(x){
  return (( x - min(x)) / (max(x) - min(x)))
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
reqCol <- c("currCmt", "deleted", "brokenPolicy", "maleRatio", "femaleRatio", "X10", "X20", "X30", "X40", "X50", "X60")
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

##################################################
# nnet package
# One-Hot Encoding
section.ind <- class.ind(NewsData$section)
NewsDataE <- cbind(NewsData, section.ind)
colnames(NewsDataE)
#  [1] "currCmt"      "deleted"      "brokenPolicy" "maleRatio"    "femaleRatio"
#  [6] "X10"          "X20"          "X30"          "X40"          "X50"
# [11] "X60"          "section"      "Economy"      "IT"           "Life_Cult"
# [16] "Politics"     "Society"      "World"


Elen <- nrow(subset(NewsDataE, section == "Economy"))
Ilen <- nrow(subset(NewsDataE, section == "IT"))
Llen <- nrow(subset(NewsDataE, section == "Life_Cult"))
Plen <- nrow(subset(NewsDataE, section == "Politics"))
Slen <- nrow(subset(NewsDataE, section == "Society"))
Wlen <- nrow(subset(NewsDataE, section == "World"))

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

trData <- NewsDataE[idx,]
teData <- NewsDataE[-idx,]

trX <- trData[,c(1:11)]; trY <- trData[,c(13:18)];
teX <- teData[,c(1:11)]

nnModel <- nnet(x=trX, y=trY, size=15, softmax=T)

plot.nnet(nnModel)

nnPrediction <- predict(nnModel, teX, type="class")
predTable <- table(nnPrediction, teData$section)
predAccuracy <- cat(calcAcc(predTable), "%\n")

df=data.frame(x=c(1:5), y=c(46.72, 50.53, 44.24, 56.92, 50.68))
ggplot(data=df, mapping=aes(x=x, y=y, col=x, fill=x)) +
  geom_col(position="identity", show.legend=F) +
  geom_text(label=paste(df$y, "%"), nudge_y=2, color="black") +
  geom_hline(aes(yintercept=mean(df$y))) +
  geom_text(x=3, y=51, label=paste("mean :", mean(df$y)), check_overlap=T, color="Red") +
  labs(title="nnet result", x="Tries", y="Percentage")

##################################################
# neuralnet package
# Elen <- nrow(subset(NewsData, section == "Economy"))
# Ilen <- nrow(subset(NewsData, section == "IT"))
# Llen <- nrow(subset(NewsData, section == "Life_Cult"))
# Plen <- nrow(subset(NewsData, section == "Politics"))
# Slen <- nrow(subset(NewsData, section == "Society"))
# Wlen <- nrow(subset(NewsData, section == "World"))

# Ei <- sample(c(1:Elen), 0.75*Elen); Ii <- sample(c(1:Ilen), 0.75*Ilen); Li <- sample(c(1:Llen), 0.75*Llen);
# Pi <- sample(c(1:Plen), 0.75*Plen); Si <- sample(c(1:Slen), 0.75*Slen); Wi <- sample(c(1:Wlen), 0.75*Wlen);

# idx <- c(
#     Ei,
#     Ii + Elen,
#     Li + Elen + Ilen,
#     Pi + Elen + Ilen + Llen,
#     Si + Elen + Ilen + Llen + Plen,
#     Wi + Elen + Ilen + Llen + Plen + Slen
# )

# NewsData$section2 <- factor(NewsData[,12], levels=Csections, labels=c(1:6))
# NewsData$section2 <- as.numeric(levels(unlist(NewsData$section2)))[unlist(NewsData$section2)]

# # Data Normalisation
# NewsDataN <- as.data.frame(lapply(NewsData[,-12], normalization))

# trData <- NewsDataN[ idx,]
# teData <- NewsDataN[-idx,]

# nnFormula <- section2 ~ .
# nnModel2 <- neuralnet(formula=nnFormula, data=trData, hidden=5)

# plot(nnModel2)

# nnModel2Result <- compute(nnModel2, teData[,c(1:11)])
# nnPrediction2 <- nnModel2Result$net.result

# nndf <- data.frame(predict=nnPrediction2, real=teData["section"])

# x <- seq(1:length(nnPrediction2))
# plot(x=x, y=nnPrediction2, type="n", xlab="", ylab="value")

# points(x, nnPrediction2, pch=4, col="red")
# points(x, teData["section"], pch=1, col="blue")

# legend("bottomright", legend=c("prediction", "answer"), pch=c(4,1))

# cor(nnPrediction2, teData["section"])
