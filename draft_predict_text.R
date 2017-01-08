library(tm)
library(data.table)

procFiles <- function(x){
  inFile <- paste0("final/en_US/en_US.", x, ".txt")
  inCon <- file(inFile, open = "rb")
  vText <- readLines(inCon, skipNul = TRUE)
  close(inCon)
  vText <- gsub("\032", "", vText)
}

docType <- c("blogs", "news", "twitter")
lstLines <- sapply(docType, procFiles)
nLines <- sapply(lstLines, length)
lstLines <- tolower(lstLines)
procLines <- function(x){unlist(strsplit(lstLines[[x]], split = " "))}
lstWords <- sapply(c(1:3), procLines)

tblText <- data.table(unlist(lstLines))
tblText <- tolower(tblText)
dfs <- DataframeSource(tblText)
corpEnglish <- PCorpus(dfs, readerControl = list(reader = readPlain, 
                                                 language = "en_US",
                                                 load = TRUE),
                            dbControl = list(useDb = TRUE,
                                             dbName = "dbEnglish",
                                             dbtype = "DB1"))


toSpace <- content_transformer(function(x, pattern){return (gsub(pattern, " ", x))})

english <- tm_map(english, content_transformer(tolower))
english <- tm_map(english, toSpace, "[^abcdefghijklmnopqrstuvwxyz]")
english <- tm_map(english, removeWords, stopwords("english"))
english <- tm_map(english, stripWhitespace)

profanity <- readLines("profanity.txt")
profanity <- paste0(profanity, collapse = "|")
replaceProfanity <-content_transformer(function(x, pattern){return (gsub(pattern, "xxxxx", x))})
english <- tm_map(english, replaceProfanity, profanity)