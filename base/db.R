####################################################################################################
### Library
library(DBI)
library(RMySQL)

####################################################################################################
# Variable

sections <- c("econ", "IT", "life_cult", "politics", "soc", "world")

# host : '%'를 넣으면 외부접속, 내부 접속일 경우는 localhost를 넣는다.
## localhost일 경우 포트를 지정하지 않아도 작동한다.
conn <- dbConnect(MySQL(), user="naver", password="Naver1q2w3e4r!", dbname="naverdb", host="localhost")

####################################################################################################
### Functions

# DB에서 데이터를 가지고 오는 함수 입니다.
getSectionData <- function(section, type, col) {
    query01 <- paste('select * from news_',section,' where newsid like "%',type,'%"', sep = '')
    dfOne <- dbGetQuery(conn, query01)
    return(dfOne)
}

# DB로 데이터 프레임 집어넣기 함수 입니다.
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

# 제목과 부제의 전처리 함수 입니다.
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

# 모든 connection을 닫아 줍니다.
dbDisconnectAll <- function(){
  ile <- length(dbListConnections(MySQL())  )
  lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n", ile))
}
