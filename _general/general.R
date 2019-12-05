####################################################################################################
# Library
library(stringr)
library(reshape2)
library(ggplot2)

source("base/db.R")

####################################################################################################
# Variable
type <- "V"

####################################################################################################
# Function
getMonthlyView <- function(df) {
    len <- nrow(df)
    # cat("len:", len, "\n")
    x1 <- c(); x2 <- c(); x3 <- c(); x4 <- c(); x5 <- c(); x6 <- c();
    x7 <- c(); x8 <- c(); x9 <- c(); x10 <- c(); x11 <- c(); x12 <- c();
    
    # get NCOMMENT(6th col)
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

##################################################
### Comment
Edf <- getSectionData(sections[1], type)
Idf <- getSectionData(sections[2], type)
Ldf <- getSectionData(sections[3], type)
Pdf <- getSectionData(sections[4], type)
Sdf <- getSectionData(sections[5], type)
Wdf <- getSectionData(sections[6], type)
