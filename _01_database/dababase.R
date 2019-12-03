####################################################################################################
# Database
####################################################################################################
library(DBI)
library(RMySQL)
library(stringr)
library(openxlsx)

# host : '%'를 넣으면 외부접속, 내부 접속일 경우는 localhost를 넣는다.
## localhost일 경우 포트를 지정하지 않아도 작동한다.
conn<-dbConnect(MySQL(), user="root", password="1q2w3e4r!", dbname="naverdb",host="localhost")

# Test용 dual 테이블
## 결과가 나오면 정상적으로 작동하는 것을 확인할 수 있다.
query01 <- 'select power(2, 10) from dual'
data <- dbGetQuery(conn, query01)

####################################################################################################
# Initialise Variables
ext <- '.xlsx'
path <- 'resources/'
types <- c('E','I','L','P','S','W')
sections <- c('econ','IT','life_cult','politics','soc','world')
tables <- c('NEWS_ECON', 'NEWS_IT', 'NEWS_LIFE_CULT', 'NEWS_POLITICS', 'NEWS_SOC', 'NEWS_WORLD')
####################################################################################################
# Functions

dbDisconnectAll <- function(){
  ile <- length(dbListConnections(MySQL())  )
  lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n", ile))
}

# clean function
## param : data frame
cleanData <- function(df) {
  df$title <- str_replace_all(df$title, '\"', ' ')
  df$title <- str_replace_all(df$title, ',', ' ')
  df$title <- str_replace_all(df$title, '\'', ' ')
  df$title <- str_replace_all(df$title, '\t', '')
  
  df$subti <- str_replace_all(df$subti, '\"', ' ')
  df$subti <- str_replace_all(df$subti, ',', ' ')
  df$subti <- str_replace_all(df$subti, '\'', ' ')
  df$subti <- str_replace_all(df$subti, '\t', '')
  return(df)
}

dbsend <- function(df, type, section, tab) {
  cat('dbsend()\n')
  len <- nrow(df)
  date <- df$date
  TITLE <- df$title
  SUBTITLE <- df$subti
  SRC <- df$source
  NEWSDATE <- c()
  for(l in c(1:len)) {
    DATE <- paste(str_sub(date[l], 1, 4), '/', str_sub(date[l], 5, 6), '/', str_sub(date[l], 7, 8), sep='')
    NEWSDATE <- c(NEWSDATE, DATE)
  }
  NVIEW <- df$view
  NCOMMENT <- df$cmt
  CURR_CMT <- df$currCmt
  DELETED <- df$deleted
  BROKEN <- df$brokenPolicy
  MALER <- df$maleRatio
  FEMALER <- df$femaleRatio
  X10 <- df$X10; X20 <- df$X20; X30 <- df$X30; X40 <- df$X40; X50 <- df$X50; X60 <- df$X60
  NEWSID <- c()
  NEWSRANK <- df$rank
  if(is.null(NVIEW)) {
    NVIEW <- rep(0, len)
    for(l in c(1:len)) {
      ID <- paste(type, 'C', date[l], NEWSRANK[l], sep='')
      # cat(ID, '\n')
      NEWSID <- c(NEWSID, ID)
    }
  }
  else {
    NCOMMENT <- rep(0, len)
    for(l in c(1:len)) {
      ID <- paste(type, 'V', date[l], NEWSRANK[l], sep='')
      # cat(ID, '\n')
      NEWSID <- c(NEWSID, ID)
    }
  }
  for(i in c(1:len)) {
    if(is.na(X10[i])) {
      MALER[i] <- 0
      FEMALER[i] <- 0
      X10[i] <- 0
      X20[i] <- 0
      X30[i] <- 0
      X40[i] <- 0
      X50[i] <- 0
      X60[i] <- 0
    }
  }
  for(i in c(1:len)) {
    if(is.na(CURR_CMT[i])) {
      CURR_CMT[i] <- 0
      DELETED[i] <- 0
      BROKEN[i] <- 0
    }
  }
  # cat(NEWSID, '\n')
  for(l in c(1:len)) {
    # cat(NEWSID[l], '\n')
    query <- paste("INSERT INTO ", tab, " VALUES(\'", NEWSID[l], '\', ', NEWSRANK[l], ', \'', TITLE[l], '\',\'', SUBTITLE[l], '\',\'',  SRC[l], '\',\'', NEWSDATE[l], '\', ', NVIEW[l], ', ', NCOMMENT[l], ', ', CURR_CMT[l], ', ', DELETED[l], ', ', BROKEN[l], ', ', MALER[l], ', ', FEMALER[l], ', ', X10[l], ', ', X20[l], ', ', X30[l], ', ', X40[l], ', ', X50[l], ', ', X60[l], ")", sep='')
    # cat(query, '\n')
    dbSendQuery(conn, query)
  }
}
####################################################################################################
# Main
for(i in c(1:6)) {
  type <- types[i]
  section <- sections[i]
  tab <- tables[i]
  fpath <- paste(path, section, '/2018_view_data_', section, ext, sep='')
  for(i in c(1:2)) {
    df <- read.xlsx(fpath, sheet=i, colNames=T, rowNames=F)
    df <- cleanData(df)
    dbsend(df, type, section, tab)
    cat('-', i, '-\n')
  }
  fpath <- paste(path, section, '/2018_comment_data_', section, ext, sep='')
  for(i in c(1:2)) {
    df <- read.xlsx(fpath, sheet=i, colNames=T, rowNames=F)
    df <- cleanData(df)
    dbsend(df, type, section, tab)
    cat('-', i, '-\n')
  }
  fpath <- paste(path, section, '/2019_view_data_', section, ext, sep='')
  for(i in c(1:10)) {
    df <- read.xlsx(fpath, sheet=i, colNames=T, rowNames=F)
    df <- cleanData(df)
    dbsend(df, type, section, tab)
    cat('-', i, '-\n')
  }
  fpath <- paste(path, section, '/2019_comment_data_', section, ext, sep='')
  for(i in c(1:10)) {
    df <- read.xlsx(fpath, sheet=i, colNames=T, rowNames=F)
    df <- cleanData(df)
    dbsend(df, type, section, tab)
    cat('-', i, '-\n')
  }
}

# query01 <- 'select * from news_politics'
# data <- dbGetQuery(conn, query01)
# dbGetQuery(conn, "set names utf8mb4")

dbDisconnectAll()
