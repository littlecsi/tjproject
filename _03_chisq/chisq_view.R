####################################################################################################
# Library
library(stringr)
library(reshape2)
library(ggplot2)

source("base/db.R")

####################################################################################################
# Variable

sections <- c("econ", "IT", "life_cult", "politics", "soc", "world")

####################################################################################################
# Function

dbDisconnectAll <- function(){
    ile <- length(dbListConnections(MySQL())  )
    lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
    cat(sprintf("%s connection(s) closed.\n", ile))
}

## param : section and type(Comment or View)
getSectionData <- function(section, type) {
    query01 <- paste('select * from news_',section,' where newsid like "%',type,'%"', sep = '')
    dfOne <- dbGetQuery(conn, query01)
    return(dfOne)
}

getMonthlyView <- function(df) {
    len <- nrow(df)
    # cat("len:", len, "\n")
    x1 <- c(); x2 <- c(); x3 <- c(); x4 <- c(); x5 <- c(); x6 <- c();
    x7 <- c(); x8 <- c(); x9 <- c(); x10 <- c(); x11 <- c(); x12 <- c();
    
    # get NView(7th col)
    for(l in c(1:len)) {
        month <- df[l,6] %>% str_sub(6, 7)
        # cat(month, "\n")
        if(month == "01") { x1 <- c(x1, df[l,7]) }
        if(month == "02") { x2 <- c(x2, df[l,7]) }
        if(month == "03") { x3 <- c(x3, df[l,7]) }
        if(month == "04") { x4 <- c(x4, df[l,7]) }
        if(month == "05") { x5 <- c(x5, df[l,7]) }
        if(month == "06") { x6 <- c(x6, df[l,7]) }
        if(month == "07") { x7 <- c(x7, df[l,7]) }
        if(month == "08") { x8 <- c(x8, df[l,7]) }
        if(month == "09") { x9 <- c(x9, df[l,7]) }
        if(month == "10") { x10 <- c(x10, df[l,7]) }
        if(month == "11") { x11 <- c(x11, df[l,7]) }
        if(month == "12") { x12 <- c(x12, df[l,7]) }
    }
    
    x1 <- sum(as.numeric(x1)); x2 <- sum(as.numeric(x2)); x3 <- sum(as.numeric(x3))
    x4 <- sum(as.numeric(x4)); x5 <- sum(as.numeric(x5)); x6 <- sum(as.numeric(x6))
    x7 <- sum(as.numeric(x7)); x8 <- sum(as.numeric(x8)); x9 <- sum(as.numeric(x9))
    x10 <- sum(as.numeric(x10)); x11 <- sum(as.numeric(x11)); x12 <- sum(as.numeric(x12))
    
    mViewTotal <- c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)
    
    return(mViewTotal)
}

####################################################################################################
# Main
type <- 'V'
Edf <- getSectionData(sections[1], type)
Idf <- getSectionData(sections[2], type)
Ldf <- getSectionData(sections[3], type)
Pdf <- getSectionData(sections[4], type)
Sdf <- getSectionData(sections[5], type)
Wdf <- getSectionData(sections[6], type)

# Get Montly Total Comments of each section
EmViewTotal <- getMonthlyView(Edf)
ImViewTotal <- getMonthlyView(Idf)
LmViewTotal <- getMonthlyView(Ldf)
PmViewTotal <- getMonthlyView(Pdf)
SmViewTotal <- getMonthlyView(Sdf)
WmViewTotal <- getMonthlyView(Wdf)

# Combining above vectors into one data frame
ViewTotaldf <- data.frame(
    Economy=EmViewTotal,
    IT=ImViewTotal,
    Life_Cult=LmViewTotal,
    Politics=PmViewTotal,
    Society=SmViewTotal,
    World=WmViewTotal
)
ViewTotaldf$Month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
# Economy       IT Life_Cult  Politics   Society    World Month
# 1  100360767 24318368  66185551  94377073 224854699 56742533   Jan
# 2   82605339 26571839  60148342  81224742 195040992 52918607   Feb
# 3   92725836 22961125  81840238 102367831 274495875 69620547   Mar
# 4   64376011 26225649  29612244  90189555 171676720 45618439   Apr
# 5   58151423 17556183  30631936  79373214 143327966 49052918   May
# 6   47012168 15552585  36170789  69076368 148542046 54264870   Jun
# 7   74764867 20546391  47437879  80397013 155554583 66601541   Jul
# 8   81484980 31272142  64107155  99438941 149975828 86008202   Aug
# 9   59604523 28324852  58441438 113892992 152266161 63514743   Sep
# 10  56800322 27399146  53852995  90718911 163767600 67452310   Oct
# 11 184035809 36879388  80367084 178246245 383292716 66053212   Nov
# 12 106837908 26320105  88204415  45959082 242464417 74342050   Dec

ViewPlotData1 <- melt(ViewTotaldf, id.vars="Month")
# Month  variable     value
# 1    Jan   Economy 100360767
# 2    Feb   Economy  82605339
# 3    Mar   Economy  92725836
# 4    Apr   Economy  64376011
# 5    May   Economy  58151423
# 6    Jun   Economy  47012168
# ..........................

# Order the x variable into the order we want.
xorder <- c("Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct")
ViewPlotData1$Month <- factor(ViewPlotData1$Month, levels=xorder, labels=xorder)

# Plotting the graph
options("scipen" = 100)
ViewPlot1 <- ggplot(ViewPlotData1, aes(Month, value, col=variable)) +
    geom_point() + 
    geom_line(aes(color=variable, group=variable)) + 
    labs(title="Total Number of Views per Month (2018.11 ~ 2019.10)")
ViewPlot1

# without month columns
chi_df <- ViewTotaldf[, -7]

# init data frame
res_df <- data.frame(
    Economy=c(0), 
    IT=c(0), 
    Life_Cult=c(0), 
    Politics=c(0), 
    Society=c(0), 
    World=c(0))

# chi square test
# 귀무 가설 : 각 카테고리의 조회수는 일년 평균의 조회수와 차이가 없다.
for(i in c(1:6)) {
    testing <- cbind(chi_df[,i]/1000000, rep(sum(chi_df[,i]) / 12, 12)/1000000)
    res_df[i] <- chisq.test(testing, correct = F)$p.value
}

res_df
#               Economy        IT     Life_Cult        Politics
# 1 0.0000000001390498 0.7604182 0.00005840223 0.0000002639635
#                       Society     World
# 1 0.00000000000000000007551522 0.4039921

# 결론
## 카이 제곱 검정 결과를 보아, IT와 세계는 p value가 0.05 이상으로,
## 일년 평균의 조회수와 각 월의 조회수가 서로 차이가 없는것으로 나타았으며,
## 경제, 문화, 정치, 사회는 p value가 0.05 이하로 귀무 가설이 기각되었다.
## 이는 경제, 문화, 정치, 사회 카테고리의 조회수는 월별 차이가 있다고 볼 수 있다.

dbDisconnectAll()
