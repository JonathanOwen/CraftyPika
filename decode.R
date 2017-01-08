# SETUP
setwd("D:/Users/jonat_000/repos/CraftyPika")
set.seed(042416)
library(data.table)
library(dplyr)
library(quanteda)
library(stringi)

findWords <- function(x){
  x <- tolower(x)
  x <- gsub("[^abcdefghijklmnopqrstuvwxyz']", " ", x, perl = TRUE)
  w2 <- stri_extract_last_words(x)
  w1 <- stri_extract_last_words(gsub("\\s[abcdefghijklmnopqrstuvwxyz']*$", "", x, perl = TRUE))
  ptW2 <-match(w2, lmNews[[1]]$w)
  ptW1 <-match(w1, lmNews[[1]]$w)
  child3 <- which(lmNews[[2]]$w == ptW2 & lmNews[[2]]$child == ptW1)
  hits <- data.table(lmNews[[3]][which(lmNews[[3]]$child == child3)])
  topPtW3 <- arrange(hits, desc(pKN))[1:3, 1, with = FALSE]
  topW3 <- lmNews[[1]]$w[unlist(topPtW3)]
  return(topW3)
}
