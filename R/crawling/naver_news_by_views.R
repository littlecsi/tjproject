####################################################################################################
# Naver News Page by Most Views
# Category : Politics
####################################################################################################
# Library Import
library(rvest)

####################################################################################################
# Functions
clean <- function(x) {
    library(stringr)
    x <- gsub("\t", "", x)
    x <- gsub("\r", "", x)
    x <- gsub("\n", "", x)
    x <- str_trim(x)
    
    return(x)
}
cleanv <- function(x) {
    x <- gsub(',', '', x)
    return(x)
}

####################################################################################################
# Crawling
url <- 'https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&sectionId=100&date='
date <- 20191112
html <- read_html(paste(url, date, sep=''))
list <- html %>% html_nodes('.ranking_list')

title <- list %>% html_nodes('.ranking_headline') %>% html_nodes('a') %>% html_text()
subti <- list %>% html_nodes('.ranking_lede') %>% html_text()
source <- list %>% html_nodes('.ranking_office') %>% html_text()
view <- list %>% html_nodes('.ranking_view') %>% html_text()

# pre-processing
subti <- clean(subti) # removes whitespace, \t, \r, \n
view <- cleanv(view) # removes commas

df <- data.frame(rank=c(1:length(title)), title, subti, source, view)