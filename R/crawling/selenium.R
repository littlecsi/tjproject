# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.9.1.jar -port 4445

library(RSelenium)

remDr <- remoteDriver(remoteServerAdd='localhost', port=4445L, browserName='chrome')
remDr$open()

url <- 'https://news.naver.com/main/ranking/read.nhn?rankingType=popular_day&oid=015&aid=0004244086&date=20191119&type=1&rankingSectionId=100&rankingSeq=1'

remDr$navigate(url)

print('waiting')
Sys.sleep(5)

elm <- remDr$findElement('class name','u_cbox_in_view_comment')
elm$clickElement

commentCnt <- elm$getElementText()
commentCnt
