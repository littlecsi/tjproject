################################################################################
# view count analysis
################################################################################
df <- data.frame()
for(year in c(2007:2019)) {
    filename <- paste('view_', year, '.csv', sep='')
    data <- read.csv(filename, stringsAsFactors=F)
    data <- data[,c(2, 5, 6, 7)]
    
    df <- rbind(df, data)
}

dim(df)
colnames(df)
head(df)

library(dplyr)

top <- df %>% subset(rank == 1)

