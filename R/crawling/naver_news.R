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

# Section - Politics, Economics, Society, Culture, World, Science, Photo, TV
################################################################################
sec_id <- as.character(c(100:105))

# 2019/11/05 Data
url_1 <- 'https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day'
url_2 <- '&sectionId='
url_3 <- '&date=20191105'

df <- data.frame()
for(id in sec_id) {
    # print(id)
    url <- paste(url_1, url_2, id, url_3, sep='')
    html <- get_html(url)
    item <- html %>% html_nodes('.ranking_item')
    
    category <- html %>% html_nodes('.ranking_category_item is_selected') %>% html_nodes('.is_selected')%>% html_nodes('a') %>% html_text()
    
    title <- item %>% html_nodes('.ranking_headline') %>% html_nodes('a') %>% html_text()
    source <- item %>% html_nodes('.ranking_office') %>% html_text()
    view <- item %>% html_nodes('.ranking_view') %>% html_text()
    
    temp_df <- data.frame(category, title, source, view)
    
    df <- rbind(df, temp_df)
}

url <- paste(url_1, url_2, '100', url_3, sep='')
html <- get_html(url)
item <- html %>% html_nodes('.ranking_item')

category <- html %>% html_nodes('.ranking_category_item is_selected') %>% html_nodes('a') %>% html_text()

title <- item %>% html_nodes('.ranking_headline') %>% html_nodes('a') %>% html_text()
source <- item %>% html_nodes('.ranking_office') %>% html_text()
view <- item %>% html_nodes('.ranking_view') %>% html_text()

temp_df <- data.frame(category, title, source, view)