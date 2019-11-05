library(rvest)

url = 'https://datalab.naver.com/keyword/realtimeList.naver?datetime=2019-11-02T22%3A00%3A00&period=now'

html = read_html(url, encoding = 'UTF-8')
head(html)
str(html)
