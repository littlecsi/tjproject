####################################################################################################
# Library
library(openxlsx)
library(stringr)
library(reshape2)
library(ggplot2)

####################################################################################################
# Variable
pathOne <- "D:/GitHub/tjproject/resources/"
sections <- c("econ", "IT", "life_cult", "politics", "soc", "world")

####################################################################################################
# Function
getData <- function(path, n) {
  df <- data.frame()
  for(i in c(1:n)) {
    cat("-", i, "-\n")
    dfOne <- read.xlsx(path, sheet=i, colNames=T, rowNames=F)
    dfOne <- dfOne[,-c(2,3)]
    # cat("ncol(df):", ncol(df), ", ncol(dfOne):", ncol(dfOne), "\n")
    df <- rbind(df, dfOne)
  }
  return(df)
}
getSectionData <- function(section) {
  path1 <- paste(pathOne, section, "/2018_comment_data_", section, ".xlsx", sep="")
  # cat(path1, '\n')
  dfOne <- getData(path1, 2)
  
  path2 <- paste(pathOne, section, "/2019_comment_data_", section, ".xlsx", sep="")
  # cat(path2, '\n')
  dfTwo <- getData(path2, 10)
  
  df <- rbind(dfOne, dfTwo)

  return(df)
}
getMonthlyCmt <- function(df) {
  len <- nrow(df)
  # cat("len:", len, "\n")
  x1 <- c(); x2 <- c(); x3 <- c(); x4 <- c(); x5 <- c(); x6 <- c();
  x7 <- c(); x8 <- c(); x9 <- c(); x10 <- c(); x11 <- c(); x12 <- c();

  for(l in c(1:len)) {
    month <- df[l,4] %>% str_sub(5, 6)
    # cat(month, "\n")
    if(month == "01") { x1 <- c(x1, df[l,3]) }
    if(month == "02") { x2 <- c(x2, df[l,3]) }
    if(month == "03") { x3 <- c(x3, df[l,3]) }
    if(month == "04") { x4 <- c(x4, df[l,3]) }
    if(month == "05") { x5 <- c(x5, df[l,3]) }
    if(month == "06") { x6 <- c(x6, df[l,3]) }
    if(month == "07") { x7 <- c(x7, df[l,3]) }
    if(month == "08") { x8 <- c(x8, df[l,3]) }
    if(month == "09") { x9 <- c(x9, df[l,3]) }
    if(month == "10") { x10 <- c(x10, df[l,3]) }
    if(month == "11") { x11 <- c(x11, df[l,3]) }
    if(month == "12") { x12 <- c(x12, df[l,3]) }
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
Edf <- getSectionData(sections[1])
Idf <- getSectionData(sections[2])
Ldf <- getSectionData(sections[3])
Pdf <- getSectionData(sections[4])
Sdf <- getSectionData(sections[5])
Wdf <- getSectionData(sections[6])

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


# 귀무 가설 : '정치'와 '사회'간의 분포의 모양이 동질적이다.
var.test(x=PmCmtTotal, y=SmCmtTotal)
# 	F test to compare two variances
# 
# data:  PmCmtTotal and SmCmtTotal
# F = 1.5308, num df = 11, denom df = 11, p-value = 0.4917
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#  0.440669 5.317370
# sample estimates:
# ratio of variances 
#           1.530752

# p-value(0.4917) > 0.05 이므로 귀무 가설 채택.
# '정치'와 '사회'간의 분포 형태가 동질하다고 볼 수 있다.

# 귀무 가설 : '경제'와 '세계'간의 분포의 모양이 동질적이다.
var.test(x=EmCmtTotal, y=WmCmtTotal)
# 	F test to compare two variances
# 
# data:  EmCmtTotal and WmCmtTotal
# F = 1.297, num df = 11, denom df = 11, p-value = 0.6737
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#  0.3733877 4.5055139
# sample estimates:
# ratio of variances 
#           1.297036

# p-value(0.6737) > 0.05 이므로 귀무 가설 채택.
# '경제'와 '세계'간의 분포 형태가 동질하다고 볼 수 있다.
