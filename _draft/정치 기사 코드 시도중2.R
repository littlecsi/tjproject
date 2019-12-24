library(RJSONIO) 
library(httr)
library(rvest)
library(magrittr)
library(Rcpp)
library(devtools)
library(stringr)

### 연습
url <- 'https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&sectionId=100&date=20191127'
html <- read_html(url)


rankaddress <- html %>% html_nodes('.ranking_text') %>% html_nodes('.ranking_headline') %>% html_node('a') %>% html_attr('href')

rankaddress2 <- paste0('https://news.naver.com', rankaddress)
html2 <- read_html(rankaddress2[1])

commentaddress <- html2 %>% html_nodes('.article_btns') %>% html_nodes('.article_btns_left #articleTitleCommentCount')
commentaddress <- html2 %>% html_nodes('.article_btns') %>% html_nodes('.article_btns_left > div')
commentaddress <- html2 %>% html_nodes('.article_btns') %>% html_nodes('.article_btns_left > a') %>% html_attr('href')  
commentaddress




commentaddress <- html2 %>% html_nodes('.article_btns') %>% html_nodes('.article_btns_left') %>% html_node('a')

# %>% html_nodes('.a') %>% html_attr('href')

commentaddress <- html2 %>% html_node('article_btns_left') %>% html_node('ranking_headline') %>% html_node('a') %>% html_attr('href')  
rankaddress2[2]
rankaddress2_mt <- as.matrix(rankaddress2)
rankaddress2_mt[30]

head(rankaddress2_mt, 1)

# caryear <- html %>% html_nodes('.product-item') %>% html_nodes('.year') %>% html_node('span') %>% html_text()
# carkm <- html %>% html_nodes('.product-item') %>% html_nodes('.km') %>% html_node('span') %>% html_text()

# html %>% html_nodes('img.src') %>% html_attr('alt')













# 날짜 확인
as.Date(Sys.time())-391

### 본

x<-0 
pol_data <- NULL 
for(x in 27:391){  ## 181101 부터 191031 까지 정치 뉴스 url 추출
  dadate<-as.Date(Sys.time())-x  ## 날짜 
  dadate2<-gsub("-","",dadate)
  
  ## 메인페이지에서 1~30위 뉴스 url 추출
  baseurl<-paste0("https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&sectionId=100&date=",dadate2) 
  html <- read_html(baseurl)
  rankaddress <- html %>% html_nodes('.ranking_text') %>% html_nodes('.ranking_headline') %>% html_node('a') %>% html_attr('href')
  rankaddress2 <- paste0('https://news.naver.com', rankaddress)
  rankaddress2_mt <- as.matrix(rankaddress2)
  
  ### 366일치 데이터 묶기
  pol_data <- rbind(pol_data, rankaddress2)
  
  cat("\n",x) 
} 


dim(pol_data) 
head(pol_data) 
##데이터저장
write.csv(pol_data,"pol_data.csv",row.names = F) 

pol_data <- read.csv('pol_data.csv')
# news_url<-base_data2[,2] 

###Selenium을 활용하여 댓글가져오기 
library(Rcpp)
library(RSelenium)
library(wdman)
library(binman)
# cmd 관리자 권한으로 열기
# cd c:\\selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.9.1.jar -port 4445

# rD <- rsDriver(browser="fire",port=1003L) 
# remDr <- rD[["client"]] 
remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445L, browserName = "chrome")

head(news_url)


nrow(pol_data) # 행수 확인 (365일치)
ncol(pol_data) # 열수 확인 (1~30위)

pol_data[365,2]

# webElem <- remDr$findElement(using = 'css', "span.u_cbox_page_more") 
# webElem$clickElement()

pol_review_list<-matrix(nrow=nrow(pol_data), ncol = ncol(pol_data))
pol_review_list<-list()


class(pol_data)
mode(pol_data)

# 저장된 url 가져오기
pol_data <- read.csv('pol_data.csv')

pol_data_ul <- unlist(pol_data)

pol_data_ul_re <- as.data.frame(pol_data_ul)

remDr$open()

pol_review_list2 <- as.list(pol_review_list)
class(pol_review_list)

pol_review_list<-list()

pol_review_list[[4347]]<- NA


# for(i in 1:nrow(pol_data)){ 
  # for(k in 1:ncol(pol_data)){   
for(i in 1:nrow(pol_data_ul_re)){ 
  # for(k in 1:1){   
    
    remDr$navigate(pol_data_ul_re[i,]) 
  
    # 접속 시간 확보(에러 방지)
    Sys.sleep(runif(1)+runif(1)+0.25) 
    ## 댓글 모음 보기
    tryCatch(
      {webElem <- remDr$findElement(using = 'css', "span.lo_txt") },
      error= function(e){
        print("ERROR 발생")
        Sys.sleep(runif(1)+runif(1)+0.25) 
        pol_review_list[[i]]<- NA
        print("NA 처리 완료")
        remDr$navigate(pol_data_ul_re[(i+1),])     
        Sys.sleep(runif(1)+runif(1)+0.25) 
        webElem <- remDr$findElement(using = 'css', "span.lo_txt")
      },
      warning = function(w){
        print("WARNING 발생")
      },
      finally = {
        webElem <- remDr$findElement(using = 'css', "span.lo_txt")
        print("fin")
      }
    )
    
    webElem$clickElement()  ## 클릭
    cat("\t","com") 
    
    ## 순공감순으로 정렬
    Sys.sleep(0.3) 
    tryCatch(
      {webElem <- remDr$findElement(using = 'css', "span.u_cbox_sort_label")},
      error= function(e){
        print("ERROR 발생")
        Sys.sleep(runif(1)+runif(1)+0.25) 
        webElem <- remDr$findElement(using = 'css', "span.u_cbox_sort_label") 
      },
      warning = function(w){
        print("WARNING 발생")
      },
      finally = {
        Sys.sleep(runif(1)+runif(1)+0.25)
        webElem <- remDr$findElement(using = 'css', "span.u_cbox_sort_label")
        print("fin2")
      }
    )

    webElem$clickElement()  ## 클릭
    cat("\t","sort") 
    
    ## 댓글 더 보기
    Sys.sleep(0.3) 
    webElem <- remDr$findElement(using = 'css', "span.u_cbox_page_more") 
    cat("\t","more") 
    webElem$clickElement()  ## 클릭
    
    ## 댓글 더 보기2
    Sys.sleep(0.3) 
    webElem <- remDr$findElement(using = 'css', "span.u_cbox_page_more") 
    webElem$clickElement() ## 클릭
    cat("\t","more2") 
    
    Sys.sleep(0.3) 
    webElem <- remDr$findElement(using = 'css', "div.u_cbox_content_wrap ul.u_cbox_list") 
    rev<-webElem$getElementText() 
    cat("\t","crawl") 
    rev2<-unlist(str_split(unlist(rev),"\n")) 
    review<-rev2[seq(3,length(rev2),by=12)] 

    review2 <- gsub("공감|답글|신고|비공감|댓글모음",'',review)
    review2 <- gsub("[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}\\.",'',review2)
    review2 <- gsub("[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}",'',review2)
    review2 <- gsub("[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}",'',review2)
    review2 <- gsub("[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}",'',review2)
    review2 <- gsub("\\w{4}\\*{4}",'',review2)
    review2 <-  Filter(function(x){ nchar(x) >= 3}, review2)    
    
    
    # pol_review_list[i, k] <- review 
    pol_review_list[[i]]<-review2 
    # if(k %% 500 ==0){ 
      # save(pol_review_list,file="pol_review_list.RData")
    # } 
    # save(review_list,file="review_list.RData") 
    save(pol_review_list, file="pol_review_list.RData") 
    cat("\n",i,"번째 저장 완료") 
  # }
    
} 
writeLines(pol_review_list, 'pol_review_list.txt')

save(pol_review_list, file="pol_review_list.RData") 
pol_review_unli <- unlist(pol_review_list)
nchar(pol_review_unli[1])
length(pol_review_unli)


# 
# review_ex <- str_extract_all(review, "[가-히]{1,}")
# review_ex <- gsub("공감|답글|신고|비공감|댓글모음",'',review)
# review_ex <- gsub("[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}\\.",'',review_ex)
# review_ex <- gsub("[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}",'',review_ex)
# review_ex <- gsub("[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}",'',review_ex)
# review_ex <- gsub("[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}",'',review_ex)
# review_ex <- gsub("\\w{4}\\*{4}",'',review_ex)
# review_ex_f <-  Filter(function(x){ nchar(x) >= 3}, review_ex)
