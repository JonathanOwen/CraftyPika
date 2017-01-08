# SETUP
setwd("D:/Users/jonat_000/repos/CraftyPika")
require(data.table)
require(dplyr)
require(quanteda)
require(stringi)


# PRACTICE ON BLOGS
# CREATE BLOG CORPUS
sourceBlogs <- textfile(list.files(path = "train/clean", pattern = "blogs.txt$", 
                                   full.names = TRUE, recursive = TRUE), 
                        cache = FALSE)
blogs <- corpus(sourceBlogs)

# CREATE DOCUMENT-FEATURE MATRIX OF NGRAMS, N = 4
dfmBlogs <- dfm(blogs, toLower = FALSE, ngrams = 4)
save(dfmBlogs, file = "save_object/dfmBlogs.Rdata")

# CONVERT TO DATA TABLE
countBlogs <- colSums(dfmBlogs)
tblBlogs <- data.table("feature" = names(countBlogs), "countFeature" = countBlogs)
rm(dfmBlogs)
rm(countBlogs)
save(tblBlogs, file = "save_object/tblBlogs.Rdata")
tblBlogs$feature <- gsub("_", " ", tblBlogs$feature)
tblBlogs$word4 <- stri_extract_last_words(tblBlogs$feature)
tblBlogs$gram3 <- stri_replace_last_fixed(tblBlogs$feature, tblBlogs$word4, "")
tblBlogs$gram3 <- stri_trim(tblBlogs$gram3)
save(tblBlogs, file = "save_object/tmpBlogs.Rdata")
tblBlogs <- tblBlogs[,c(4, 3, 2), with = FALSE]


# GROUP BY QUADGRAMS & COUNT
fBlogs <- tblBlogs %>%
          group_by(gram3, word4) %>%
          summarise(countWord4 = sum(countFeature)) 

fBlogs <- fBlogs %>%
  group_by(gram3, word4, countWord4) %>%
  summarise(countGram3 = sum(countWord4)) 

# Calculate term frequencies
dtTwitter <- dtTwitter %>%
  group_by(feature) %>%
  summarise(countFeature = n())
dtTwitter <- arrange(dtTwitter, desc(countFeature))
dtTwitter$freqFeature <- dtTwitter$countFeature/sum(dtTwitter$countFeature)
dtTwitter <- arrange(dtTwitter, desc(freqFeature))


















# CREATE NEWS CORPUS
sourceNews <- textfile(list.files(path = "clean", pattern = "news\\d\\.txt$", 
                                  full.names = TRUE, recursive = TRUE),
                       cache = FALSE)
news <- corpus(sourceNews)

# CREATE DOCUMENT FEATURE MATRIX
dfmNews <- dfm(news, toLower = FALSE, ngrams = 5)
save(dfmNews, file = "dfmNewsSaved.Rdata")

# CONVERT TO DATA TABLE
countNews <- colSums(dfmNews)
dtNews <- data.table("feature" = names(countNews), "countFeature" = countNews)
rm(dfmNews)
rm(countNews)
save(dtNews, file = "dtNewsSaved.Rdata")
dtNews$feature <- gsub("_", " ", dtNews$feature)
dtNews$word5 <- stri_extract_last_words(dtNews$feature)
save(dtNews, file = "dt4NewsSaved.Rdata")
dtNews$gram4 <- stri_replace_last_fixed(dtNews$feature, dtNews$word5, "")
dtNews$gram4 <- stri_trim(dtNews$gram4)
save(dtNews, file = "dt4NewsSaved.Rdata")
tNews <- dtNews[,c(4, 3, 2), with = FALSE]
save(tNews, file = "tNewsSaved.Rdata")
rm(dtNews)

# GROUP BY QUADRIGRAMS & COUNT
tNews <- arrange(tNews, gram4, word5)
fNews <- with(tNews, aggregate(x = countFeature, by = list(gram4, word5), sum))





