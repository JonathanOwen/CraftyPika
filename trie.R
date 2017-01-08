# SETUP
set.seed(042416)
require(quanteda)
require(data.table)
require(dplyr)
require(stringi)

conProfanity <- file("profanity.txt")
profanity <- readLines(conProfanity, skipNul = TRUE, ok = TRUE)
close(conProfanity)

rawNews <- dfm(news, ngrams = 1, toLower = FALSE)
rawF <- features(rawNews)
cutNews <- dfm(news, ngrams = 1, toLower = FALSE, ignoredFeatures = profanity)
cutF <- features(cutNews)
profanity <- rawF[which(!rawF %in% cutF)]
rm(rawF); rm(cutF); rm(cutNews); rm(rawNews); rm(conProfanity)

oneLetter <- letters[c(2:8, 10:26)]


makeTrie <- function(x, n){
  Trie <- vector("list", n)
  for (i in 1:n){
    dfmXi <- dfm(x, ngrams = i, toLower = FALSE, ignoredFeatures = c(profanity, oneLetter, stopwords("english")), concatenator = " ")
    if (i == n){dfmXi <- trim(dfmXi, minCount = 2)}
    countNi <- colSums(dfmXi)
    dtNi <- data.table("f" = names(countNi), "countF" = countNi, keep.rownames = FALSE)
#    dtNi$f <- gsub("_", " ", dtNi$f, perl = TRUE)
    Trie[[i]] <- dtNi
    if (i > 1){
      Trie[[i]]$w <- stri_extract_last_words(Trie[[i]]$f)
      Trie[[i]]$child <- gsub("\\s[abBcCdeEfFgGhHiIjklmnNoOpPqrRsStTuUvVwWxXyzZ']*$", "", Trie[[i]]$f, perl = TRUE)
    }
    rm(dfmXi)
    rm(countNi)
  }
  return(Trie)
}
  
  
getPointer <- function(x){
  n <- length(x)
  for (i in 1:(n-1)){
    x[[i+1]]$ptChild <- match(x[[i+1]]$child, x[[i]]$f)
    x[[i+1]] <- x[[i+1]][,-4, with = FALSE]
  }
  return(x)
}



cleanTrie <- function(x){
  n <- length(x)
  names(x[[1]]) <- c("w", "countF")
  for (i in 1:n){
    if (i > 1){
      x[[i]]$ptW <- match(x[[i]]$w, x[[1]]$w)
      x[[i]] <- x[[i]][,c(5, 4, 2), with = FALSE]
      names(x[[i]]) <- c("w", "child", "countF")
    } 
  }
  return(x)
}

rm(news); rm(profanity); rm(trainNews)
rm(devNews)

