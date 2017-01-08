#-------------------------------------------------------------------------------
# OLD STUFF

# Process files
getInfo <- function(x){
  cmdText <- paste0("E:/Rtools/bin/wc -l -w -L final/en_US/en_US.", x, ".txt")
  fileInfo <- system(cmdText, intern = TRUE)
  unlist(strsplit(fileInfo, split = " +", fixed = FALSE))
}

# Read and write documents as vectors, returns number of lines
processChunks <- function(x, y){
  inFile <- paste0("final/en_US/en_US.", x, ".txt")
  inCon <- file(inFile, open = "rb", blocking = FALSE)
  for (i in 1:10) {
    vText <- readLines(inCon, y, skipNul = TRUE, ok = TRUE)
    vText <- gsub("\032", "", vText)
    outFile <- paste0("en_US/", x, i-1, ".txt")
    outCon <- file(outFile, open = "wt")
    writeLines(vText, outCon)
    close(outCon)
  }
  close(inCon)
}

fileProperties <- sapply(docType, getInfo)
fileChunks <- data.table(cbind(docType, 
                               "chunkSize" = round(as.numeric(fileProperties[2,])/10,0)))
mapply(processChunks, fileChunks$docType, fileChunks$chunkSize)


# Create corpus
english <- PCorpus(DirSource("en_US/"), 
                   readerControl = list(reader = readPlain, 
                                        language = "en_US",
                                        load = TRUE),
                   dbControl = list(useDb = TRUE,
                                    dbName = "dbEnglish",
                                    dbtype = "DB1"))

# Regular expression testing
testBlog[9997] <- gsub("([[:punct:]])([[:punct:]]+)", "\\1", testBlog[9997], perl = TRUE)
testBlog[9997] <- gsub("[?!]", "\n", testBlog[9997], perl = TRUE)
testBlog[9997] <- gsub("([\\.] )([[:upper:]])", "\n\\2", testBlog[9997], perl = TRUE)
testBlog[9997] <- gsub("â€™", "'", testBlog[9997], perl = TRUE)
testCon <- file("testBlog9997.txt", open ="w+")
writeLines(as.character(testBlog[9997]), testCon, sep = "\n")
newTestBlog11 <- readLines(testCon, skipNul = TRUE)
close(testCon)

# Might be useful
asc <- function(x){strtoi(charToRaw(x), 16L)}

# Test on full testBlog
testBlog <- gsub("â€™", "'", testBlog, perl = TRUE)
testBlog <- gsub("â€\\“", "\n", testBlog, perl = TRUE)
testBlog <- gsub("([[:punct:]])([[:punct:]]+)", "\\1", testBlog, perl = TRUE)
testBlog <- gsub("[?!();:,]", "\n", testBlog, perl = TRUE)
testBlog <- gsub("(\\. )([[:upper:]])", "\n\\2", testBlog, perl = TRUE)
testBlog <- gsub("(â€œ)(\\w+)(â€)", "\n\\2\n", testBlog, perl = TRUE)
testBlog <- tolower(testBlog)
testBlog <- gsub("\\d", "N", testBlog, perl = TRUE)
testBlog <- gsub("[^abcdefghijklmnNopqrstuvwxyz'\\r\\s]", " ", testBlog, perl = TRUE)
testBlog <- gsub("N+", "N", testBlog, perl = TRUE)
testBlog <- gsub("\\h+", " ", testBlog, perl = TRUE)
testBlog <- gsub("(\\n )|( \\n)", "\n", testBlog, perl = TRUE)
testBlog <- gsub("^\\h|\\h$", "", testBlog, perl = TRUE)
testBlog <- gsub("\\n$", "", testBlog, perl = TRUE)
testCon <- file("testBlog.txt", open ="w+")
writeLines(as.character(testBlog), testCon, sep = "\n")
newTestBlog <- readLines(testCon, skipNul = TRUE)
close(testCon)

# Clean corpus
toSpace <- content_transformer(function(x, pattern){return (gsub(pattern, " ", x))})
english <- tm_map(english, content_transformer(tolower))
english <- tm_map(english, toSpace, "[^abcdefghijklmnopqrstuvwxyz']")
# english <- tm_map(english, removeWords, stopwords("english"))
english <- tm_map(english, stripWhitespace)

# Write cleaned corpus
writeCorpus(english, "clean/")


# Get cleaned corpus files
# read clean corpus documents as vectors
readClean <- function(x){
  inFile <- paste0("clean/", x, ".txt.txt")
  inCon <- file(inFile, open = "rb")
  vClean <- readLines(inCon, skipNul = TRUE)
  close(inCon)
  vClean
}

lstClean <- sapply(docType, readClean)
lstClean <- strsplit(unlist(lstClean), split = " ")
wordBlogs <- unlist(lstClean[1:nLines[1]])
wordNews <- unlist(lstClean[(1+nLines[1]):(nLines[1]+nLines[2])])
wordTwitter <- unlist(lstClean[(1+nLines[1]+nLines[2]):(nLines[1]+nLines[2]+nLines[3])])
wordBlogs <- data.table("term" = wordBlogs)
wordNews <- data.table("term" = wordNews)
wordTwitter <- data.table("term" = wordTwitter)


# Calculate term frequencies
freqBlogs <- wordBlogs %>%
  group_by(term) %>%
  summarise(frequency = n())
freqBlogs <- arrange(freqBlogs, desc(frequency))
freqBlogs <- freqBlogs[-46,]
freqBlogs$count <- freqBlogs$frequency
freqBlogs$frequency <- freqBlogs$count/sum(freqBlogs$count)

freqNews <- wordNews %>%
  group_by(term) %>%
  summarise(frequency = n())
freqNews <- arrange(freqNews, desc(frequency))
freqNews <- freqNews[-17,]
freqNews$count <- freqNews$frequency
freqNews$frequency <- freqNews$count/sum(freqNews$count)

freqTwitter <- wordTwitter %>%
  group_by(term) %>%
  summarise(frequency = n())
freqTwitter <- arrange(freqTwitter, desc(frequency))
freqTwitter <- freqTwitter[-23,]
freqTwitter$count <- freqTwitter$frequency
freqTwitter$frequency <- freqTwitter$count/sum(freqTwitter$count)


# Print table and plot data
htmlTable(dfEnglish[2:4], header = c( "..# of lines..", " ..# of words.. ", "words per line"))
pBlogs <- ggplot(freqBlogs[1:50], aes(row_number(frequency), frequency)) +
  geom_bar(stat="identity") +
  ggtitle("50 most frequently occurring words in blogs text") +
  scale_x_continuous(name = "term", breaks = c(1:50), 
                     labels = freqBlogs$term[50:1]) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x  = element_text(angle = 90, hjust = 1))
pBlogs
pNews <- ggplot(freqNews[1:50], aes(row_number(frequency), frequency)) +
  geom_bar(stat="identity") +
  ggtitle("50 most frequently occurring words in news text") +
  scale_x_continuous(name = "term", breaks = c(1:50), 
                     labels = freqNews$term[50:1]) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x  = element_text(angle = 90, hjust = 1))
pNews
pTwitter <- ggplot(freqTwitter[1:50], aes(row_number(frequency), frequency)) +
  geom_bar(stat="identity") +
  ggtitle("50 most frequently occurring words in Twitter text") +
  scale_x_continuous(name = "term", breaks = c(1:50), 
                     labels = freqTwitter$term[50:1]) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x  = element_text(angle = 90, hjust = 1))
pTwitter

# Read test document
testCon <- file("clean/news5.txt", open = "rt")
testNews <-readLines(testCon, skipNul = TRUE)
close(testCon)

# WEIRD
# tmpText <- gsub("\\d", "N", tmpText, perl = TRUE)
# tmpText <- gsub("N+", "N", tmpText, perl = TRUE)