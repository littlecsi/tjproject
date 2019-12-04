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
Edf <- getSectionData(sections[1], 'C')
Idf <- getSectionData(sections[2], 'C')
Ldf <- getSectionData(sections[3], 'C')
Pdf <- getSectionData(sections[4], 'C')
Sdf <- getSectionData(sections[5], 'C')
Wdf <- getSectionData(sections[6], 'C')

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

# 귀무 가설 : '정치'와 'IT' 댓글 수의 의존관계는 서로 없다. 
chisq.test(x = PmCmtTotal, y = ImCmtTotal)
PmCmtTotal
ImCmtTotal
WmCmtTotal
SmCmtTotal

# 귀무 가설 : '정치'와 '사회'간의 분포의 모양이 동질적이다.
fisher.test(x=ImCmtTotal, y=SmCmtTotal)

fisher.test(x=EmCmtTotal, y=WmCmtTotal)

dbDisconnectAll()
