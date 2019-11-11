################################################################################
# Naver News
################################################################################
# Library imports
library(rvest)

# Functions
get_html <- function(url) {
    html <- read_html(url)
    return(html)
}
check_cat <- function(vec) {
    len <- length(vec)
    if(len < 30) {
        extra <- rep(NA, (30-len))
        return(c(vec, extra))
    } else {
        return(vec)
    }
}
check_url <- function(url) {
    out <- tryCatch(
        {
            read_html(url)
            return(T)
            
        }, error=function(cond) {
            message("URL doesn't exist")
            message(cond)
            return(FALSE)
            
        }, warning=function(cond) {
            message("URL caused a warning")
            message(cond)
            return(FALSE)
            
        }, finally={}
    )
    return(out)
}

# Section - Politics, Economics, Society, Culture, World, Science, Photo, TV
################################################################################
sec_id <- as.character(c(100:105))

year <- c(2018)
month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
day <- c(month, as.character(c(13:31)))

# 2019/11/05 Data
url_1 <- 'https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day'
url_2 <- '&sectionId='
url_3 <- '&date='

df <- data.frame()
for(y in year) {
    for(m in month) {
        for(d in day) {
            # Disgard months with no 31st (except February)
            if(m %in% c('04','06','09','11') & d == 31) {
                next
            }
            # Disgard February (special cases)
            if(m %in% c('02') & d >= 30) {
                next
            }
            
            # Extract all category in this date
            for(id in sec_id) {
                # creates date variable
                date <- paste(y, m, d, sep='')
                print(date) # prints out the date each time to check

                # crates complete url to crawl
                url <- paste(url_1, url_2, id, url_3, date, sep='')
                
                # checks if url is loadable. If not, it jumps to the next one
                if(check_url(url) == FALSE) {
                    next
                }

                # loads the HTML code from the URL
                html <- get_html(url)
                item <- html %>% html_nodes('.ranking_item')
                
                category <- html %>% html_nodes('.is_selected') %>% html_nodes('a') %>% html_text()
                category <- category[2]
                category <- rep(gsub('선택됨', '', category), 30)
                
                title <- item %>% html_nodes('.ranking_headline') %>% html_nodes('a') %>% html_text()
                source <- item %>% html_nodes('.ranking_office') %>% html_text()
                view <- item %>% html_nodes('.ranking_view') %>% html_text()
                
                # if(length(view) == 0) { view <- rep(NA, 30) }
                
                title <- check_cat(title)
                source <- check_cat(source)
                view <- check_cat(view)
                
                date <- rep(date, 30)
                
                temp_df <- data.frame(category, title, source, view, date)
                
                df <- rbind(df, temp_df)
            }
        }
    }
}
write.csv(df, 'news.csv')