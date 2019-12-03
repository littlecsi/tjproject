####################################################################################################
# Database
####################################################################################################
Sys.setenv(JAVA_HOME='c:/program files/Java/jre1.8.0_231')
library(rJava)
library(DBI)
library(RJDBC)
library(stringr)
library(openxlsx)

driver <- 'oracle.jdbc.driver.OracleDriver'
jarpath <- 'c:/oraclexe/app/oracle/product/11.2.0/server/jdbc/lib/ojdbc6.jar'

drv <- JDBC(driver, jarpath)

url <- 'jdbc:oracle:thin:@localhost:1521/xe'
id <- 'naver'
pwd <- 'naver'

conn <- dbConnect(drv, url, id, pwd)

####################################################################################################
# functions
dbsend <- function(df, type, section) {
  cat('dbsend()\n')
  
  len <- nrow(df)
  date <- df$date
  
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
    query <- paste("INSERT INTO NEWS_WORLD VALUES(\'", NEWSID[l], '\', ', NEWSRANK[l], ', \'', SRC[l], '\',\'', NEWSDATE[l], '\', ', NVIEW[l], ', ', NCOMMENT[l], ', ', CURR_CMT[l], ', ', DELETED[l], ', ', BROKEN[l], ', ', MALER[l], ', ', FEMALER[l], ', ', X10[l], ', ', X20[l], ', ', X30[l], ', ', X40[l], ', ', X50[l], ', ', X60[l], ")", sep='')
    # cat(query, '\n')
    dbSendUpdate(conn, query)
  }
}

####################################################################################################
# Initialise Variables
ext <- '.xlsx'
path <- 'D:/GitHub/tjproject/resources/'
type <- 'W'
section <- 'world'

# Main
# fpath <- paste(path, section, '/2019_view_data_', section, ext, sep='')
fpath <- paste(path, section, '/2019_comment_data_', section, ext, sep='')
# df <- read.xlsx(fpath, 1, as.data.frame=T, header=T, encoding='UTF-8')
# dbsend(df, type, section)

for(i in c(1:10)) {
  df <- read.xlsx(fpath, sheet=i, colNames=T, rowNames=F)
  dbsend(df, type, section)
  cat('-', i, '-\n')
}
