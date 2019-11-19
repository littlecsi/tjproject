library(RSelenium)

remDr <- remoteDriver(remoteServerAdd='localhost', port=4445L, browserName='chrome')
remDr$open()

url <- 'https://news.naver.com/main/ranking/read.nhn?rankingType=popular_day&oid=015&aid=0004244086&date=20191119&type=1&rankingSectionId=100&rankingSeq=1'

remDr$navigate(url)

elm <- remDr$findElement('class','u_cbox_info_txt')

appData <- elm$getElementText()
appData
