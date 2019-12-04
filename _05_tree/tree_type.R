####################################################################################################
# Library
library(stringr)
library(reshape2)
library(ggplot2)

source("base/db.R")

####################################################################################################
# Variable



####################################################################################################
# Main
Edf <- getSectionData(sections[1], 'C')
Idf <- getSectionData(sections[2], 'C')
Ldf <- getSectionData(sections[3], 'C')
Pdf <- getSectionData(sections[4], 'C')
Sdf <- getSectionData(sections[5], 'C')
Wdf <- getSectionData(sections[6], 'C')

