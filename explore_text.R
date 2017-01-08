# Setup
setwd("D:/Users/jonat_000/repos/CraftyPika")

# Libraries
library(tm)


# Task 1: Get data
if (!file.exists("Coursera-SwiftKey.zip")) {
  file_url <- 
    "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(file_url, "Coursera-SwiftKey.zip")
}

if (!file.exists("final/en_US")) {
  unzip("Coursera-SwiftKey.zip")
}

# Get file lines
getFiles <- function(x){
  inFile <- paste0("final/en_US/en_US.", x, ".txt")
  inCon <- file(inFile, open = "rb")
  vText <- readLines(inCon, skipNul = TRUE)
  close(inCon)
  vText <- gsub("\032", "", vText)
}

docType <- c("blogs", "news", "twitter")
listLines <- sapply(docType, getFiles)
nLines <- sapply(listLines, length)
outBreaks <- round(nLines/10, digits = 0)




