
# REMINDERS ---------------------------------------------------------------

# 1. Fix a.m. amd p.m.


# =========================================================================
# SETUP
# =========================================================================

# Load libraries ----------------------------------------------------------
library(data.table)
library(foreach)
library(doParallel)
library(rbenchmark)
#library(dplyr)
#library(quanteda)
#library(stringi)


# Set options -------------------------------------------------------------
set.seed(42)
usable_cores <- detectCores()-1


# Define parameters -------------------------------------------------------
data_dir        <- "data"
doc_types       <- c("blogs", "news", "twitter")
drop_all_text   <- TRUE
drop_text_lines <- TRUE
language        <- "en_US"
test_fraction   <- 0.05
url_path  <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset"
zip_name  <- "Coursera-SwiftKey.zip"

abbreviations <- c("ave", "assn",
                   "blvd", "bros",
                   "capt", "col", "com",
                   "dr",
                   "etc",
                   "gen", "gov",
                   "inc",
                   "jr",
                   "lt",
                   "mr", "mrs", "ms",
                   "net",
                   "org",
                   "pp", "prof", "prop",
                   "rep", "rev",
                   "sen",
                   "rd",                    
                   "sgt", "sr", "st",
                   "vs")

months <- c("Jan",
            "Feb",
            "Mar",
            "Apr",
            "May",
            "Jun",
            "Jul",
            "Aug",
            "Sep", "Sept",
            "Oct",
            "Nov",
            "Dec")

days <- c("Mon",
          "Tue", "Tues",
          "Wed",
          "Thur", "Thurs",
          "Fri")

states <- c("AL", "AK", "AR", "AZ", 
            "CA", "Calif", "CO", "Colo" , "CT", 
            "DC", "DE",
            "FL", "Fla",
            "GA",
            "HI",
            "IA", "ID", "IL", "IN",
            "KS", "KY",
            "LA",
            "MA", "MD", "ME", "MI", "Mich", "MN", "MO", "MS", "MT", 
            "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY",
            "OH", "OK", "OR",
            "PA",
            "RI",
            "SC", "SD",
            "TN", "Tenn", "TX", 
            "UT",
            "VA", "VT", 
            "WA", "WI", "Wis", "WV", "WY")

number_separators <- data.frame('pattern' = c('(\\d)(\\. ?)(\\d)',
                                              '(\\d)(,)(\\d{3})',
                                              '(\\d)(\\: ?)(\\d)',
                                              '(\\d)(\\/ ?)(\\d)',
                                              '(\\d)(\\-)(\\d)',
                                              '(\\d)( ?\\%)',
                                              '(\\$ ?)(\\d)',
                                              '(\\-)(\\d)',
                                              '(\\+)(\\d)'
                                              ),
                                'text'    = c('\\1 POINT \\3',
                                              '\\1\\3',
                                              '\\1 COLON \\3',
                                              '\\1 SLASH \\3',
                                              '\\1 RANGE \\3',
                                              '\\1 PERCENT',
                                              'USD \\2',
                                              'NEGATIVE \\2',
                                              'POSITIVE \\2'
                                              ),
                                stringsAsFactors=FALSE)

clean_numbers <- data.frame("pattern" = c("0", 
                                           "1", 
                                           "2", 
                                           "3", 
                                           "4", 
                                           "5", 
                                           "6", 
                                           "7", 
                                           "8", 
                                           "9"),
                            "text"    = c("ZERO ", 
                                           "ONE ", 
                                           "TWO ", 
                                           "THREE ", 
                                           "FOUR ", 
                                           "FIVE ", 
                                           "SIX ", 
                                           "SEVEN ",
                                           "EIGHT ", 
                                           "NINE "),
                            stringsAsFactors=FALSE)



regex_patterns  <- c("â€™",
                     "â€\\“",
                     "[?!();:]",
                     "(\\. )([[:upper:]])",
                     "(â€œ)(\\w+)(â€)",
                     "([[:punct:]])([[:punct:]]+)")
new_texts       <- c("", 
                     "\n", 
                     "\r", 
                     "\n\\2",
                     "\n\\2\n", 
                     "\\1")
clean_text_parameters <- data.frame(regex_patterns, new_texts)
                                    
# =========================================================================
# FUNCTIONS
# =========================================================================

#   _______________________________________________________________________
#   function    read_file_lines(file_name, read_dir)
#   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
read_file_lines <- function(file_name, read_dir) {
    read_file  <- paste0(read_dir, "/", file_name)
    read_con   <- file(read_file, open = "rb")
    file_lines <- readLines(read_con, skipNul = TRUE, ok = TRUE)
    close(read_con)
    file_lines
}
#   summary     ?
#               ?
#
#   parameters  file_name: character
#                 Name of text file to be read.
#               read_dir: character
#                 Name of directory containing file to be read.
#
#   returns     file_lines: list of characters
# _________________________________________________________________________                  


#   _______________________________________________________________________
#   function    split_data(dataset, n_splits=20)
#   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
split_data <- function(dataset, n_splits=20){
    dataset_id <- ceiling(runif(n=nrow(dataset), min=0, max=n_splits))
    dataset <- cbind(dataset, dataset_id)
}
#   summary     ?
#               ?
#
#   parameters  dataset: string
#                 ?
#               n_splits: int
#                 Number of splits applied to dataset
#                     
#   returns     dataset:                            
#   
#   example      
#   _______________________________________________________________________


#   _______________________________________________________________________
#   function    clean_text(text_lines, pattern, new_text="")
#   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
clean_text <- function(text_lines, pattern, new_text="", ...) {
    text_lines <- gsub(pattern, new_text, text_lines, ...)
}
#   summary     ?
#
#
#   parameters  text_lines: character
#      
#               regex_pattern: character
#                     
#               new_text: character
#
#   returns     text_lines                           
#   
#   example      
#   _______________________________________________________________________


#   _______________________________________________________________________
#   function    par_clean_text(text_lines, regex_pattern, new_text="", 
#                              n_cores=2)
#   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
par_clean_text <- function(text_docs, pattern, new_text="",
                           n_cores=2) {
    tmp_cluster <- makeCluster(n_cores)
    foreach(d=doc_types) %do% {
        text_docs[[d]]$text_line <- parSapply(cl=tmp_cluster, 
                                              text_docs[[d]]$text_line, 
                                              clean_text, 
                                              pattern=pattern, 
                                              new_text=new_text)
    }
    stopCluster(tmp_cluster)
    text_docs
}
#   summary     ?
#               ?
#
#   parameters  text_docs: list
#      
#               regex_pattern: character
#                     
#               new_text: character, default=""
#
#               n_cores: int, default=usable_cores
#
#   returns     text_lines                           
#   
#   
#   example      
#   _______________________________________________________________________  


#   _______________________________________________________________________
#   function    write_text_table(text_table, file_name, write_dir)
#   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
write_text_table <- function(text_table, file_name, write_dir) {
    write_file  <- paste0(write_dir, "/", file_name)
    write_con   <- file(write_file, open = "wt")
    write.table(text_table, write_con, row.names=FALSE)
    close(write_con)
}
#   summary     ?
#
#
#   parameters  write_lines, character
#                 Lines of text file to be written
#               file_name: character
#                 Name of text file to be written
#               write_dir: character
#                 Name of directory where file will be written
#
#   returns     NULL
#   _______________________________________________________________________  



#   _______________________________________________________________________
#   function    find_strings(text_lines, pattern, ...)
#   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
find_strings <- function(text_lines, pattern, ...) {
    matches <- gregexpr(pattern, text_lines, ...)
    strings <- regmatches(text_lines, matches)
    strings <- unlist(strings)
    strings <- tolower(strings)
    strings <- as.data.frame(table(strings), stringsAsFactors=FALSE)
    strings <- strings[order(strings$Freq, decreasing = TRUE),]
    strings$strings <- gsub("\\.", "", strings$strings)
    strings
}
#   summary     ?
#
#
#   parameters  write_lines, character
#                 Lines of text file to be written
#               file_name: character
#                 Name of text file to be written
#               write_dir: character
#                 Name of directory where file will be written
#
#   returns     NULL
#   _______________________________________________________________________ 

    # fileLines <- gsub("\032", "", fileLines)
    # 
    # 
    # remLines <- data.table("fileLines" = tblLines$fileLines[which(!tblLines$fileLines %in% trainLines$fileLines)])
    # testLines <- sample_frac(remLines, 0.5, replace = FALSE)
    # devLines <- data.table("fileLines" = remLines$fileLines[which(!remLines$fileLines %in% testLines$fileLines)])
    # outFile <- paste0("train/", x, ".txt")
    # outCon <- file(outFile, open = "wt")
    # outLines <- as.character(trainLines$fileLines)
    # writeLines(outLines, outCon, sep = "\n")
    # close(outCon)
    # outFile <- paste0("test/", x, ".txt")
    # outCon <- file(outFile, open = "wt")
    # outLines <- as.character(testLines$fileLines)
    # writeLines(outLines, outCon, sep = "\n")
    # close(outCon)
    # outFile <- paste0("develop/", x, ".txt")
    # outCon <- file(outFile, open = "wt")
    # outLines <- as.character(devLines$fileLines)
    # writeLines(outLines, outCon, sep = "\n")
    # close(outCon)
    # checkLines <- c(nrow(trainLines), nrow(testLines), nrow(devLines))



# DATA --------------------------------------------------------------------

#   Download & unzip data -------------------------------------------------
zip_file <- paste0(data_dir, "/", zip_name)
if (!file.exists(zip_file)) {
    file_url <- paste0(url_path, "/", zip_name)
    download.file(file_url, zip_file)
}
lang_dir  <- paste0(data_dir, "/final/", language)
if (length(dir(lang_dir))==0) {unzip(zip_file, exdir=data_dir)}


#   Read files line by line -----------------------------------------------
text_lines <- lapply(paste0(language, ".", doc_types,".txt"), 
                     read_file_lines, read_dir=lang_dir)


#   Convert to list of data tables ----------------------------------------
all_text <- lapply(text_lines, as.data.table)
all_text <- lapply(all_text, setnames, old="V1", new="text_line")
names(all_text) <- doc_types
all_text_counts <- sapply(all_text, nrow)

if (drop_text_lines==TRUE) {rm(text_lines)}


#   Remove control characters and double backslashes ----------------------
all_text <- par_clean_text(all_text, pattern="[[:cntrl:]]", 
                           new_text="", n_cores=usable_cores)
all_text <- par_clean_text(all_text, pattern="\\\\", 
                           new_text="", n_cores=usable_cores)


#   Split data ------------------------------------------------------------
#       Randomly assign lines to subsets
all_text <- lapply(all_text, split_data, n_splits=(1/test_fraction))

#       Select subsets for testing and training
test <- foreach(d=doc_types) %do% {
    all_text[[d]][all_text[[d]]$dataset_id==(1/test_fraction),]
}
names(test) <- doc_types

train <- foreach(d=doc_types) %do% {
    all_text[[d]][all_text[[d]]$dataset_id!=(1/test_fraction),]
}
names(train) <- doc_types

if (drop_all_text==TRUE) {rm(all_text)}

#           Check line counts
test_line_counts  <- sapply(test, nrow)
train_line_counts <- sapply(train, nrow)
all_text_counts == train_line_counts + test_line_counts


#   Write test and train datasets to file ---------------------------------
test_dir <- paste0(data_dir, "/", language, "/", "test")
foreach(d=doc_types) %do% {
    write_text_table(test[[d]], paste0(d, ".txt"), test_dir)
}

train_dir <- paste0(data_dir, "/", language, "/", "train")
foreach(d=doc_types) %do% {
    write_text_table(train[[d]], paste0(d, ".txt"), train_dir)
}

# =========================================================================
# PREPROCESS 
# =========================================================================

# Clean lines -------------------------------------------------------------
debug_text <- train[["news"]][train[["news"]]$dataset_id==1, 1]

registerDoParallel(usable_cores)

#    Fix apostrophes ------------------------------------------------------
debug_text$text_line <- par_clean_text(debug_text$text_line,
                                       pattern="â€™",
                                       new_text="'",
                                       n_cores=usable_cores)
debug_text$text_line <- clean_text(debug_text$text_line, 
                                   pattern="(\\w)(â)(\\w) ",
                                   new_text="\\1'\\3 ",
                                   perl=TRUE,
                                   ignore.case=TRUE)


#    Remove periods -------------------------------------------------------

#        after abbreviations ----------------------------------------------
foreach(abbr=abbreviations) %dopar% {
    debug_text$text_line <- clean_text(debug_text$text_line,
                                       pattern=paste0("\\s", abbr, "\\.\\s"),
                                       new_text=paste0(" ", abbr, " "),
                                       perl=TRUE,
                                       ignore.case=TRUE)
}

#        after months -----------------------------------------------------
foreach(m=months) %dopar% {
    debug_text$text_line <- clean_text(debug_text$text_line,
                                       pattern=paste0("\\s", m, "\\.\\s"),
                                       new_text=paste0(" ", m, " "),
                                       perl=TRUE,
                                       ignore.case=TRUE)
}

#        after days -------------------------------------------------------
foreach(d=days) %dopar% {
    debug_text$text_line <- clean_text(debug_text$text_line,
                                       pattern=paste0("\\s", d, "\\.\\s"),
                                       new_text=paste0(" ", d, " "),
                                       perl=TRUE,
                                       ignore.case=TRUE)
}

period_pattern <- "[a-z']+\\."
before_periods <- find_strings(debug_text$text_line, period_pattern,
                               ignore.case=TRUE, perl=TRUE)

#    Handle numbers -------------------------------------------------------
foreach(num_sep=iter(number_separators, by='row')) %do% {
    debug_text$text_line <- clean_text(debug_text$text_line,
                                       pattern=num_sep$pattern,
                                       new_text=num_sep$text,
                                       perl=TRUE)
}



# for (i in 1:nrow(clean_text_parameters)){
#     debug_clean_text$text_line <- sapply(debug_clean_text$text_line,
#                                          clean_text,
#                                          regex_pattern=clean_text_parameters[i,1],
#                                          new_text=clean_text_parameters[i,2],
#                                          perl=TRUE,
#                                          ignore.case=TRUE)
# }




# CLEAN FILES
cleanText <- function(x, y){
    inFile <- paste0(y, "/", x, ".txt")
    inCon <- file(inFile, open = "rt")
    tmpText <-readLines(inCon, skipNul = TRUE)
    close(inCon)
#tmpText <- gsub("\\sst\\.\\s", " st ", tmpText, perl = TRUE)
#tmpText <- gsub("\\sdr\\.\\s", " dr ", tmpText, perl = TRUE)
#tmpText <- gsub("\\sprof\\.\\s", " prof ", tmpText, perl = TRUE)
#tmpText <- gsub("\\smr\\.\\s", " mr ", tmpText, perl = TRUE)
#tmpText <- gsub("\\smrs\\.\\s", " mrs ", tmpText, perl = TRUE)
#tmpText <- gsub("\\sms\\.\\s", " ms ", tmpText, perl = TRUE)
# tmpText <- gsub("\\slt\\.\\s", " lt ", tmpText, perl = TRUE)
# tmpText <- gsub("\\ssgt\\.\\s", " sgt ", tmpText, perl = TRUE)
# tmpText <- gsub("\\scol\\.\\s", " col ", tmpText, perl = TRUE)
# tmpText <- gsub("\\sgen\\.\\s", " gen ", tmpText, perl = TRUE)
# tmpText <- gsub("\\save\\.\\s", " ave ", tmpText, perl = TRUE)
# tmpText <- gsub("\\srd\\.\\s", " rd ", tmpText, perl = TRUE)
# tmpText <- gsub("\\sblvd\\.\\s", " blvd ", tmpText, perl = TRUE)
#tmpText <- gsub("â€™", "'", tmpText, perl = TRUE)
# tmpText <- gsub("â€\\“", "\n", tmpText, perl = TRUE)
# tmpText <- gsub("([[:punct:]])([[:punct:]]+)", "\\1", tmpText, perl = TRUE)
# tmpText <- gsub("[?!();:]", "\r", tmpText, perl = TRUE)
# tmpText <- gsub("(\\. )([[:upper:]])", "\n\\2", tmpText, perl = TRUE)
# tmpText <- gsub("(â€œ)(\\w+)(â€)", "\n\\2\n", tmpText, perl = TRUE)
tmpText <- tolower(tmpText)
# tmpText <- gsub("0", "ZERO ", tmpText, perl = TRUE)
# tmpText <- gsub("1", "ONE ", tmpText, perl = TRUE)
# tmpText <- gsub("2", "TWO ", tmpText, perl = TRUE)
# tmpText <- gsub("3", "THREE ", tmpText, perl = TRUE)
# tmpText <- gsub("4", "FOUR ", tmpText, perl = TRUE)
# tmpText <- gsub("5", "FIVE ", tmpText, perl = TRUE)
# tmpText <- gsub("6", "SIX ", tmpText, perl = TRUE)
# tmpText <- gsub("7", "SEVEN ", tmpText, perl = TRUE)
# tmpText <- gsub("8", "EIGHT ", tmpText, perl = TRUE)
# tmpText <- gsub("9", "NINE ", tmpText, perl = TRUE)
tmpText <- gsub("(\\. ?)([[:upper:]])", "POINT \\2", tmpText, perl = TRUE)
tmpText <- gsub("([[:upper:]] ?)\\%", "\\1 PERCENT", tmpText, perl = TRUE)
tmpText <- gsub("[^abBcCdeEfFgGhHiIjklmnNoOpPqrRsStTuUvVwWxXyzZ'\\r\\s]", 
                " ", tmpText, perl = TRUE)
tmpText <- gsub("\\h+", " ", tmpText, perl = TRUE)
#tmpText <- gsub("(\\n )|( \\n)", "\n", tmpText, perl = TRUE)
tmpText <- gsub("^\\h|\\h$", "", tmpText, perl = TRUE)
tmpText <- gsub("\\n$", "", tmpText, perl = TRUE)
tmpText <- gsub("^", "BOS \\1", tmpText, perl = TRUE)
tmpText <- gsub("$", "\\1 EOS", tmpText, perl = TRUE)
tmpText <- gsub("\\n", "EOS\nBOS", tmpText, perl = TRUE)
tmpText <- gsub("(\\w)EOS", "\\1 EOS", tmpText, perl = TRUE)
tmpText <- gsub("BOS(\\w)", "BOS \\2", tmpText, perl = TRUE)
outFile <- paste0(y, "/clean/", x, ".txt")
outCon <- file(outFile, open ="w+")
writeLines(as.character(tmpText), outCon, sep = "\n")
close(outCon)
}




dataSet <- "train"
cleanText(docType[2], dataSet)

dataSet <- "develop"
cleanText(docType[2], dataSet)

dataSet <- "test"
cleanText(docType[2], dataSet)

# CREATE CORPUS
trainNews <- textfile(list.files(path = "train/clean", pattern = "news.txt$", 
                                 full.names = TRUE, recursive = TRUE), cache = FALSE)
devNews <- textfile(list.files(path = "develop/clean", pattern = "news.txt$", 
                               full.names = TRUE, recursive = TRUE), cache = FALSE)

news <- corpus(trainNews) 
dNews <- corpus(trainNews) + corpus(devNews)




