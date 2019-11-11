################################################################################
# Crawling the comments from politics news.
################################################################################
# Library imports
library(rvest)

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
    "https://news.naver.com/main/read.nhn?m_view=1&mode=LSD&mid=shm&sid1=100&oid=022&aid=0003412842"
)
df <- data.frame()

################################################################################
# Initial coding
url <- "https://news.naver.com/main/read.nhn?m_view=1&mode=LSD&mid=shm&sid1=100&oid=022&aid=0003412842"

html <- read_html(url)

content <- html %>% html_nodes(".content") # Top content

# title of the news
title <- content %>% html_nodes(".article_header") %>% html_nodes(".article_info") %>% html_nodes("h3") %>% html_nodes("a") %>% html_text()

# Date of the news written
date <- content %>% html_nodes(".article_header") %>% html_nodes(".article_info") %>% html_nodes(".sponsor") %>% html_nodes("span") %>% html_text()
date <- date[1]

cbox <- html %>% html_nodes(".u_cbox")

id <- cbox %>% html_nodes(".u_cobx_info") %>% html_nodes(".u_cbox_nick") %>% html_text()
comment <- cbox %>% html_nodes(".u_cbox_text_wrap") %>% html_nodes("span") %>% html_text()
info <- cbox %>% html_nodes(".u_cbox_info_base") %>% html_nodes(".u_cobx_date") %>% html_text()

id
comment
info


################################################################################
# Going through the url_list to crawl.
for(url in url_list) {

}
write.csv(df, "comments.csv")