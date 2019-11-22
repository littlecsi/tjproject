####################################################################################################
# Naver News Page by Most Views
# Category : Politics
####################################################################################################
# Library Import
library(rvest)
library(xlsx)
library(stringr)
library(RSelenium)

####################################################################################################
# Functions
clean <- function(x) { # Function to remove new lines, tabs, etc...
    x <- gsub("\t", "", x)
    x <- gsub("\r", "", x)
    x <- gsub("\n", "", x)
    x <- str_trim(x)
    
    return(x)
}
cleanc <- function(x) { # Function to remove commas in view
    x <- gsub(',', '', x)
    return(x)
}
chkURL <- function(url) {
    out <- tryCatch(
        {
            html <- read_html(url)
            return(html)
        },
        error=function(cond) {
            return(F)
        },
        warning=function(cond) {
            return(F)
        },
        finally={
            cat('chkURL function called\n')
        }
    )
}
chkElem <- function(elem) {
    out <- tryCatch(
        {
            return(remDr$findElement(elem))
        },
        error=function(cond) {
            return(F)
        }
    )
}

####################################################################################################
# Crawling
remDr <- remoteDriver(remoteServerAdd='localhost', port=4445L, browserName='chrome')
remDr$open()

year <- as.character(c(2018:2019))
month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
month_eng <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
day <- c(c('01','02','03','04','05','06','07','08','09'), 10:31)

sleepT <- 1/4

startDate <- '20190101'
stopDate <- '20191031'

finFlag = F
for(y in year) {
    if(finFlag == T) { break }
    if(as.integer(y) < as.integer(str_sub(startDate, 1, 4))) { next }
    
    mcnt <- 0 # Index to iterate month_eng vector
    for(m in month) {
        if(finFlag == T) { break }
        if(as.integer(m) < as.integer(str_sub(startDate, 5, 6))) { next }
        
        mcnt <- mcnt + 1 # Incrementing index 
        
        df <- data.frame(rank=c(), title=c(), subti=c(), source=c(), view=c(), date=c())
        for(d in day) {
            if(finFlag == T) { break }
            if(as.integer(d) < as.integer(str_sub(startDate, 7, 8))) { next }
            # Disgard months with no 31st (except February)
            if(m %in% c('04','06','09','11') & d == 31) { next }
            # Disgard February (special cases)
            if(m %in% c('02') & d >= 30) { next }
            
            # News Information Crawling
            Sys.setenv("http_proxy"="")     # These codes are 
            Sys.setenv("no_proxy"=T)        # To fix some 
            Sys.setenv("no_proxy"=1)        # Proxy problems
            
            url <- 'https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&sectionId=102&date='
            
            dat <- paste(y, m, d, sep='')
            
            print(dat) # Prints out the date that the loop is working on
            
            if(dat == stopDate) { finFlag = T }
            
            url <- paste(url, dat, sep='')
            
            html <- chkURL(url)
            
            # Checks URL
            while(is.list(html) == F) {
                html <- chkURL(url)
            }
            
            list <- html %>% html_nodes('.ranking_list') %>% html_nodes('.ranking_text')
            
            title <- list %>% html_nodes('.ranking_headline') %>% html_nodes('a') %>% html_text()
            subti <- list %>% html_nodes('.ranking_lede') %>% html_text()
            source <- list %>% html_nodes('.ranking_office') %>% html_text()
            view <- list %>% html_nodes('.ranking_view') %>% html_text()
            
            len <- length(title) # number of articles in this page
            
            if(len == 0) { next } # If nothing is crawled, skip
            if(length(view) == 0) { view <- rep(NA, len) }
            
            # Selenium crawling
            urls <- list %>% html_nodes('.ranking_headline') %>% html_nodes('a') %>% html_attr('href')
            iter <- c(1:length(title))
            
            currCmt <- c(); deleted <- c(); brokenPolicy <- c(); maleRatio <- c(); femaleRatio <- c();
            X10 <- c(); X20 <- c(); X30 <- c(); X40 <- c(); X50 <- c(); X60 <- c();
            
            for(i in iter) {
                cat('-----', i, '-----\n')
                
                url <- 'https://news.naver.com/main/ranking/read.nhn?'
                
                u <- urls[i]
                
                oid <- u %>% str_extract('oid=[0-9]+') %>% str_sub(5)
                aid <- u %>% str_extract('aid=[0-9]+') %>% str_sub(5)
                
                url <- paste(url, 'm_view=1&rankingType=popular_day&oid=', oid,'&aid=', aid, '&date=', dat, '&type=1&rankingSectionId=102&rankingSeq=', i, sep='')
                
                remDr$navigate(url)
                
                Sys.sleep(sleepT)
                elm <- remDr$findElements('class','u_cbox_info_txt')
                
                cnt <- 1
                while(length(elm) == 0) { # If nothing is crawled, then refresh the page and wait longer
                    remDr$navigate(url)
                    Sys.sleep(sleepT * 2^cnt)
                    
                    elm <- remDr$findElements('class','u_cbox_info_txt')
                    cat(sleepT * 2^cnt, '\n')
                    cnt <- cnt + 1
                }
                
                currCmt <- cleanc(c(currCmt, as.character(elm[[1]]$getElementText())))
                
                if(is.na(as.integer(currCmt[i]))) { # If current comment is not crawled, change to NA
                    currCmt[i] = NA
                    deleted <- c(deleted, NA)
                    brokenPolicy <- c(brokenPolicy, NA)
                    maleRatio <- c(maleRatio, NA)
                    femaleRatio <- c(femaleRatio, NA)
                    X10 <- c(X10, NA)
                    X20 <- c(X20, NA)
                    X30 <- c(X30, NA)
                    X40 <- c(X40, NA)
                    X50 <- c(X50, NA)
                    X60 <- c(X60, NA)
                    print("No Data - appended NA values")
                    next()
                }
                deleted <- c(deleted, as.character(elm[[2]]$getElementText()))
                brokenPolicy <- c(brokenPolicy, as.character(elm[[3]]$getElementText()))
                
                if(as.integer(currCmt[i]) < 100) { # If current comment count is less than 100, append NA values to the rest
                    maleRatio <- c(maleRatio, NA)
                    femaleRatio <- c(femaleRatio, NA)
                    X10 <- c(X10, NA)
                    X20 <- c(X20, NA)
                    X30 <- c(X30, NA)
                    X40 <- c(X40, NA)
                    X50 <- c(X50, NA)
                    X60 <- c(X60, NA)
                    print("No Data - appended NA values")
                    next()
                }
                
                elm <- remDr$findElements('class','u_cbox_chart_per')
                
                cnt <- 1
                while(length(elm) == 0) { # If nothing is crawled, then refresh the page and wait longer
                    remDr$navigate(url)
                    Sys.sleep(sleepT * 2^cnt)
                    
                    elm <- remDr$findElements('class','u_cbox_chart_per')
                    cat(sleepT * 2^cnt, '\n')
                    cnt <- cnt + 1
                }
                
                maleRatio <- c(maleRatio, as.character(elm[[1]]$getElementText()))
                femaleRatio <- c(femaleRatio, as.character(elm[[2]]$getElementText()))
                X10 <- c(X10, as.character(elm[[3]]$getElementText()))
                X20 <- c(X20, as.character(elm[[4]]$getElementText()))
                X30 <- c(X30, as.character(elm[[5]]$getElementText()))
                X40 <- c(X40, as.character(elm[[6]]$getElementText()))
                X50 <- c(X50, as.character(elm[[7]]$getElementText()))
                X60 <- c(X60, as.character(elm[[8]]$getElementText()))
            }
            
            infoDF <- data.frame(currCmt=currCmt, deleted=deleted, brokenPolicy=brokenPolicy, maleRatio=maleRatio, femaleRatio=femaleRatio, X10=X10, X20=X20, X30=X30, X40=X40, X50=X50, X60=X60)
            
            # pre-processing
            subti <- clean(subti) # removes whitespace, \t, \r, \n
            view <- cleanc(view); view <- clean(view) # removes commas
            
            tdf <- data.frame(rank=c(1:len), title=title, subti=subti, source=source, view=view, date=rep(dat, len))
            
            tdf <- cbind(tdf, infoDF)
            
            df <- rbind(df, tdf)
        }
        sheName <- paste(month_eng[mcnt], sep='')
        file <- paste('D:/GitHub/tjproject/R/resources/', y, '_view_data_soc.xlsx', sep='')
        write.xlsx(df, file, sheetName=sheName, col.names=T, row.names=F, append=T, password=NULL, showNA=T)
    }
}
