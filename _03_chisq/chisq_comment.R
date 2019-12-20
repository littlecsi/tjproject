####################################################################################################
# Library
library(stringr)
library(reshape2)
library(ggplot2)

source("base/db.R")

####################################################################################################
# Variable
type <- "C"

####################################################################################################
# Function
getMonthlyCmt <- function(df) {
    len <- nrow(df)
    # cat("len:", len, "\n")
    x1 <- c(); x2 <- c(); x3 <- c(); x4 <- c(); x5 <- c(); x6 <- c();
    x7 <- c(); x8 <- c(); x9 <- c(); x10 <- c(); x11 <- c(); x12 <- c();
    
    # get NCOMMENT(6th col)
    for(l in c(1:len)) {
        month <- df[l,6] %>% str_sub(6, 7)
        # cat(month, "\n")
        if(month == "01") { x1 <- c(x1, df[l,8]) }
        if(month == "02") { x2 <- c(x2, df[l,8]) }
        if(month == "03") { x3 <- c(x3, df[l,8]) }
        if(month == "04") { x4 <- c(x4, df[l,8]) }
        if(month == "05") { x5 <- c(x5, df[l,8]) }
        if(month == "06") { x6 <- c(x6, df[l,8]) }
        if(month == "07") { x7 <- c(x7, df[l,8]) }
        if(month == "08") { x8 <- c(x8, df[l,8]) }
        if(month == "09") { x9 <- c(x9, df[l,8]) }
        if(month == "10") { x10 <- c(x10, df[l,8]) }
        if(month == "11") { x11 <- c(x11, df[l,8]) }
        if(month == "12") { x12 <- c(x12, df[l,8]) }
    }
    
    x1 <- sum(as.numeric(x1)); x2 <- sum(as.numeric(x2)); x3 <- sum(as.numeric(x3))
    x4 <- sum(as.numeric(x4)); x5 <- sum(as.numeric(x5)); x6 <- sum(as.numeric(x6))
    x7 <- sum(as.numeric(x7)); x8 <- sum(as.numeric(x8)); x9 <- sum(as.numeric(x9))
    x10 <- sum(as.numeric(x10)); x11 <- sum(as.numeric(x11)); x12 <- sum(as.numeric(x12))
    
    mCmtTotal <- c(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)
    
    return(mCmtTotal)
}

####################################################################################################
# Main

##################################################
### Comment
Edf <- getSectionData(sections[1], type)
Idf <- getSectionData(sections[2], type)
Ldf <- getSectionData(sections[3], type)
Pdf <- getSectionData(sections[4], type)
Sdf <- getSectionData(sections[5], type)
Wdf <- getSectionData(sections[6], type)


# Get Montly Total Comments of each section
EmCmtTotal <- getMonthlyCmt(Edf)
ImCmtTotal <- getMonthlyCmt(Idf)
LmCmtTotal <- getMonthlyCmt(Ldf)
PmCmtTotal <- getMonthlyCmt(Pdf)
SmCmtTotal <- getMonthlyCmt(Sdf)
WmCmtTotal <- getMonthlyCmt(Wdf)

# Combining above vectors into one data frame
CmtTotaldf <- data.frame(
    Economy=EmCmtTotal,
    IT=ImCmtTotal,
    Life_Cult=LmCmtTotal,
    Politics=PmCmtTotal,
    Society=SmCmtTotal,
    World=WmCmtTotal
)
CmtTotaldf$Month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#     Economy     IT Life_Cult Politics Society  World Month
# Jan  659886 112092    260844  1911107 1506685 305898   Jan
# Feb  515121 148336    194695  1717366 1365131 255383   Feb
# Mar  573704 118948    288874  2218102 1709256 354731   Mar
# Apr  407151 108718    154541  1939494 1088885 206754   Apr
# May  496035  96606    178784  2021505 1113531 271456   May
# Jun  414582  94331    209414  1496470 1026012 262090   Jun
# Jul  774258 110048    286352  1857602 1143347 541489   Jul
# Aug  613186 146976    331226  2443301 1590230 582676   Aug
# Sep  394022 129072    254382  2972514 1950946 337660   Sep
# Oct  436861 126394    277971  2222571 1861683 310950   Oct
# Nov  678359 146599    254832  1785598 1618079 270679   Nov
# Dec  700864 127796    206871  1959943 1463843 300111   Dec

CmtPlotData1 <- melt(CmtTotaldf, id.vars="Month")
#    Month  variable   value
# 1    Jan   Economy  659886
# 2    Feb   Economy  515121
# 3    Mar   Economy  573704
# 4    Apr   Economy  407151
# 5    May   Economy  496035
# 6    Jun   Economy  414582
# 7    Jul   Economy  774258
# 8    Aug   Economy  613186
# 9    Sep   Economy  394022
# 10   Oct   Economy  436861
# 11   Nov   Economy  678359
# 12   Dec   Economy  700864
# 13   Jan        IT  112092
# ..........................

# Order the x variable into the order we want.
xorder <- c("Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct")
CmtPlotData1$Month <- factor(CmtPlotData1$Month, levels=xorder, labels=xorder)

# Plotting the graph
options("scipen" = 100)
CmtPlot1 <- ggplot(CmtPlotData1, aes(Month, value, col=variable)) +
    geom_point() + 
    geom_line(aes(color=variable, group=variable)) + 
    labs(title="Total Number of Comments per Month (2018.11 ~ 2019.10)")
CmtPlot1

# without month columns
chi_df <- CmtTotaldf[, -7]

# init data frame
res_df <- data.frame(
    Economy=c(0), 
    IT=c(0), 
    Life_Cult=c(0), 
    Politics=c(0), 
    Society=c(0), 
    World=c(0))

# chi square test
# 귀무 가설 : 각 카테고리의 댓글 수는 일년 평균의 댓글수와 차이가 없다.
for(i in c(1:6)) {
    testing <- cbind(chi_df[,i]/10000, rep(sum(chi_df[,i]) / 12, 12)/10000)
    res_df[i] <- chisq.test(testing, correct = F)$p.value
}

res_df
#       Economy        IT       Life_Cult          Politics             Society      World
# 1 0.1171186   0.9994975     0.8414351 0.000000005044315   0.0000000007707113    0.07475777

# 결론
## 카이 제곱 검정 결과를 보아, IT와 문화, 경제, 세계는 p value가 0.05 이상으로,
## 일년 평균의 댓글 수와 각 월의 댓글 수가 서로 차이가 없는것으로 나타았으며,
## 정치, 사회는 p value가 0.05 이하로 귀무 가설이 기각되었다.
## 이는 정치와 사회 카테고리의 댓글 수는 월별 차이가 있다고 볼 수 있다.
## 그 이유는 선거철과 관련이 있다고 유추할 수 있다.

dbDisconnectAll()
