####################################################################################################
# Naver News Page by Most Views
# Category : Politics
####################################################################################################
# Library Import
library(rvest)
library(xlsx)
library(stringr)

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
year <- as.character(c(2018:2019))
month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
month_eng <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
day <- c(c('01','02','03','04','05','06','07','08','09'), 10:31)

stopDate <- '20191118'

finFlag = F
for(y in year) {
    if(finFlag == T) { break }
    
    mcnt <- 0 # Index to iterate month_eng vector
    for(m in month) {
        if(finFlag == T) { break }
        
        mcnt <- mcnt + 1 # Incrementing index 
        
        df <- data.frame()
        for(d in day) {
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
            list <- html %>% html_nodes('.ranking_list')
            
            title <- list %>% html_nodes('.ranking_headline') %>% html_nodes('a') %>% html_text()
            subti <- list %>% html_nodes('.ranking_lede') %>% html_text()
            source <- list %>% html_nodes('.ranking_office') %>% html_text()
            view <- list %>% html_nodes('.ranking_view') %>% html_text()
            
            len <- length(title) # number of articles in this page
            
            if(len == 0) { next } # If nothing is crawled, skip
            if(length(view) == 0) { view <- rep(NA, len) }
            
            # pre-processing
            subti <- clean(subti) # removes whitespace, \t, \r, \n
            view <- cleanv(view) # removes commas
            
            tdf <- data.frame(rank=c(1:len), title=title, subti=subti, source=source, view=view, date=rep(dat, len))
            
            df <- rbind(tdf, df)
            
            print(dat)
        }
        sheName <- paste(month_eng[mcnt], sep='')
        file <- paste('D:/GitHub/tjproject/R/resources/', y, '_view_data.xlsx', sep='')
        write.xlsx(df, file, sheetName=sheName, col.names=T, row.names=F, append=T, password=NULL, showNA=T)
    }
}
