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

# Load data as corpus
# (corpusEN <- Corpus(DirSource("final/en_US"),
#                    readerControl = list(reader = readPlain,
#                                         language = "en_US",
#                                         load = FALSE)))
# Other languages
# (corpusDE <- Corpus(DirSource("D:/Users/jonat_000/repos/CraftyPika/final/de_DE"),
#                     readerControl = list(reader = readPlain,
#                                          language = "de_DE",
#                                          load = FALSE)))
#
# (corpusFI <- Corpus(DirSource("D:/Users/jonat_000/repos/CraftyPika/final/fi_FI"),
#                     readerControl = list(reader = readPlain,
#                                          language = "fi_FI",
#                                          load = FALSE)))
# 
# (corpusRU <- Corpus(DirSource("D:/Users/jonat_000/repos/CraftyPika/final/ru_RU"),
#                     readerControl = list(reader = readPlain,
#                                          language = "ru_RU",
#                                          load = FALSE)))

# Load data as character vectors, rewrite to text files
conBlogs <- file("final/en_US/en_US.blogs.txt", open = "rb")
vBlogs <- readLines(conBlogs, skipNul = TRUE)
close(conBlogs)
conBlogs <- file("en_US/blogs.txt", open = "wt")
writeLines(vBlogs, conBlogs)
close(conBlogs)

conNews <- file("final/en_US/en_US.news.txt", open = "rb")
vNews <- readLines(conNews, skipNul = TRUE)
close(conNews)
vNews <- gsub("\032", "", x = vNews)
conNews <- file("en_US/news.txt", open = "wt")
writeLines(vNews, conNews)
close(conNews)

conTweets <- file("final/en_US/en_US.twitter.txt", open = "rb")
vTweets <- readLines(conTweets, skipNul = TRUE)
close(conTweets)
conTweets <- file("en_US/twitter.txt", open = "wt")
writeLines(vTweets, conTweets)
close(conTweets)

# Read new text files into 3 element corpus
corpusEn <- Corpus(DirSource("en_US"))




#-------------------------------------------------------------------------------
# Quizzes
# Quiz 1
# 1.3 
lBlogs <- nchar(vBlogs)
max(lBlogs)

lNews <- nchar(vNews)
max(lNews)

lTweets <- nchar(vTweets)
max(lTweets)

# 1.4
length(grep("love", vTweets, ignore.case = FALSE))/
 length(grep("hate", vTweets, ignore.case = FALSE))

# 1.5
vTweets[grep("biostats", vTweets, ignore.case = FALSE)]

# 1.6
vTweets[grep(
  "^A computer once beat me at chess, but it was no match for me at kickboxing$", 
  vTweets, ignore.case = FALSE)]


