####################################################################################################
# Naver News Page by Most Comments
# Category : Politics
####################################################################################################
# Library Import
library(rvest)

####################################################################################################
# Functions
clean <- function(x) { # Function to remove new lines, tabs, etc...
    library(stringr)
    x <- gsub("\t", "", x)
    x <- gsub("\r", "", x)
    x <- gsub("\n", "", x)
    x <- str_trim(x)
    
    return(x)
}
cleanc <- function(x) { # Function to remove commas in comments
    x <- gsub(',', '', x)
    return(x)
}

####################################################################################################
# Crawling
year <- as.character(c(2019))
month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
day <- c(c('01','02','03','04','05','06','07','08','09'), 10:31)

flag = F
for(y in year) {
    if(flag == T) {
        break
    }
    df <- data.frame()
    for(m in month) {
        if(flag == T) {
            break
        }
        for(d in day) {
            # Disgard months with no 31st (except February)
            if(m %in% c('04','06','09','11') & d == 31) {
                next
            }
            # Disgard February (special cases)
            if(m %in% c('02') & d >= 30) {
                next
            }
            
            # Main
            stopDate <- '20191114'
            
            url <- 'https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&sectionId=100&date='
            date <- paste(y, m, d, sep='')
            
            if(date == stopDate) {
                flag = T
                break
            }
            
            html <- read_html(paste(url, date, sep=''))
            list <- html %>% html_nodes('.ranking_list')
            
            title <- list %>% html_nodes('.ranking_headline') %>% html_nodes('a') %>% html_text()
            subti <- list %>% html_nodes('.ranking_lede') %>% html_text()
            source <- list %>% html_nodes('.ranking_office') %>% html_text()
            cmt <- list %>% html_nodes('.count_cmt') %>% html_text()
            
            len <- length(title) # number of articles in this page
            
            if(len == 0) { # If nothing is crawled, skip
                next
            }
            
            # pre-processing
            subti <- clean(subti) # removes whitespace, \t, \r, \n
            cmt <- cleanc(view) # removes commas
            
            tdf <- data.frame(rank=c(1:len), title=title, subti=subti, source=source, cmt=cmt, date=rep(date, len))
            
            df <- rbind(tdf, df)
        }
    }
    filename <- paste('cmt_', year, '.csv', sep='')
    write.csv(df, filename)
}
