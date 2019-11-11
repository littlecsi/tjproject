################################################################################
# Crawling the comments from politics news.
################################################################################
# Library imports
library(rvest)
# library(dplyr)

# Functions
cleans <- function(x) {
    library(stringr)
    x <- gsub("\t", "", x)
    x <- gsub("\r", "", x)
    x <- gsub("\n", "", x)
    x <- str_trim(x)

    return(x)
}

# Initialisation
setwd("D:/GitHub/tjproject/R/crawling")
url_list <- c(
    "https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=022&aid=0003412842",
    "https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=119&aid=0002363203",
    "https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=003&aid=0009549195",
    "https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=003&aid=0009549002",
    "https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=421&aid=0004295202"
)
df <- data.frame()

################################################################################
# Initial coding
url <- "https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=022&aid=0003412842"
html <- read_html(url)

info <- html %>% html_nodes(".article_header") %>% html_nodes(".article_info")

title <- info %>% html_node("h3") %>% html_text() # News Title
# title
date <- info %>% html_node(".sponsor") %>% html_node("span") %>% html_text() # Date of the News written
# date
comment <- info %>% html_node(".article_btns_left") %>% html_node("a") %>% html_node("span") %>% html_text() # Number of views
comment <- cleans(comment)
# comment

body <- html %>% html_node("._article_body_contents") %>% html_text()
# body

################################################################################
# Going through the url_list to crawl.
for(url in url_list) {
    html <- read_html(url)

    info <- html %>% html_nodes(".article_header") %>% html_nodes(".article_info")

    title <- info %>% html_node("h3") %>% html_text() # News Title
    date <- info %>% html_node(".sponsor") %>% html_node("span") %>% html_text() # Date of the News written
    comment <- info %>% html_node(".article_btns_left") %>% html_node("a") %>% html_node("span") %>% html_text() # Number of views
    comment <- cleans(comment)

    body <- html %>% html_node("._article_body_contents") %>% html_text()

    temp_df <- data.frame(title, date, comment, body)

    df <- rbind(df, temp_df)
}
write.csv(df, "news.csv")