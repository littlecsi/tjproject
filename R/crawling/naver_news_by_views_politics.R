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
cleanv <- function(x) { # Function to remove commas in view
    x <- gsub(',', '', x)
    return(x)
}

####################################################################################################
# Crawling
remDr <- remoteDriver(remoteServerAdd='localhost', port=4445L, browserName='chrome')
remDr$open()

year <- as.character(c(2018:2019))
month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
month_eng <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
day <- c(c('01','02','03','04','05','06','07','08','09'), 10:31)

startDate <- '20181101'
stopDate <- '20191031'

finFlag = F
for(y in year) {
    if(finFlag == T) { break }
    if(as.integer(y) < str_sub(startDate, 1, 4)) { next }
    
    mcnt <- 0 # Index to iterate month_eng vector
    for(m in month) {
        if(finFlag == T) { break }
        if(as.integer(m) < str_sub(startDate, 5, 6)) { next }
        
        mcnt <- mcnt + 1 # Incrementing index 
        
        df <- data.frame(rank=0, title=0, subti=0, source=0, cmt=0, date=0)
        for(d in day) {
            if(as.integer(d) < str_sub(startDate, 7, 8)) { next }

            # Disgard months with no 31st (except February)
            if(m %in% c('04','06','09','11') & d == 31) { next }
            # Disgard February (special cases)
            if(m %in% c('02') & d >= 30) { next }
            
            # Main
            Sys.setenv("http_proxy"="")     # These codes are 
            Sys.setenv("no_proxy"=T)        # To fix some 
            Sys.setenv("no_proxy"=1)        # Proxy problems
            
            url <- 'https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&sectionId=100&date='
            
            dat <- paste(y, m, d, sep='')
            
            if(dat == stopDate) { finFlag = T; break }
            
            url <- paste(url, dat, sep='')
            
            html <- read_html(url)
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
            
            currCmt <- NULL; deleted <- NULL; brokenPolicy <- NULL; maleRatio <- NULL; femaleRatio <- NULL;
            X10 <- NULL; X20 <- NULL; X30 <- NULL; X40 <- NULL; X50 <- NULL; X60 <- NULL;
            
            for(i in iter) {
                url <- 'https://news.naver.com'
                url <- paste(url, urls[i], sep='')
                
                remDr$navigate(url) # navigate to the corresponding news article
                
                print('CP1')
                Sys.sleep(5)
                elm <- remDr$findElements('class name','u_cbox_info_txt')
                
                currCmt <- c(currCmt, elm[[1]]$getElementText())
                deleted <- c(deleted, elm[[2]]$getElementText())
                brokenPolicy <- c(brokenPolicy, elm[[3]]$getElementText())
                
                print('CP2')
                Sys.sleep(5)
                elm <- remDr$findElements('class name','u_cbox_chart_per')
                
                maleRatio <- c(maleRatio, elm[[1]]$getElementText())
                femaleRatio <- c(femaleRatio, elm[[2]]$getElementText())
                X10 <- c(X10, elm[[3]]$getElementText())
                X20 <- c(X20, elm[[4]]$getElementText())
                X30 <- c(X30, elm[[5]]$getElementText())
                X40 <- c(X40, elm[[6]]$getElementText())
                X50 <- c(X50, elm[[7]]$getElementText())
                X60 <- c(X60, elm[[8]]$getElementText())
            }
            
            infoDF <- data.frame(currCmt=currCmt, deleted=deleted, brokenPolicy=brokenPolicy, maleRatio=maleRatio, femaleRatio=femaleRatio, X10=X10, X20=X20, X30=X30, X40=X40, X50=X50, X60=X60)
            
            # pre-processing
            subti <- clean(subti) # removes whitespace, \t, \r, \n
            view <- cleanv(view) # removes commas
            
            tdf <- data.frame(rank=c(1:len), title=title, subti=subti, source=source, view=view, date=rep(dat, len))
            
            df <- rbind(df, tdf)
            
            df <- cbind(df, infoDF)
            
            print(dat)
        }
        sheName <- paste(month_eng[mcnt], sep='')
        file <- paste('D:/GitHub/tjproject/R/resources/', y, '_view_data.xlsx', sep='')
        write.xlsx(df, file, sheetName=sheName, col.names=T, row.names=F, append=T, password=NULL, showNA=T)
    }
}
