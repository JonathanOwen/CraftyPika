
# REMINDERS ---------------------------------------------------------------

# 1. Some apostrophes are not being fixed.
#    e.g. train['news'] subset 1, line 935
#         "The real problem we have is weâre taking in too little money and 
#          weâre spending too much, and thatâs not going to be solved..."
#         Only the â is replaced by an apostrophe.
# 2. Cases when apostrophe should end the word will not be found. This is 
#    expected behavior of the regex pattern "(\\w)(â|â€™|â)(\\w) ".
#    e.g. "the bankâs money" will be fixed to "the bank's money" but
#         "the banksâ money" will not be fixed.


# =========================================================================
# SETUP
# =========================================================================

# Load required packages --------------------------------------------------
# manage_package
# see "Elegant way to check for missing packages and install them?"
#     (http://stackoverflow.com/questions/4090169)
#     Solution proposed by Simon O'Hanlon 
#     (http://stackoverflow.com/users/1478381/simon-ohanlon)
#     Modified by removing for loop in so that manage_package, aka foo in 
#     original solution, takes a single package as its argument.
#     All packages are checked, installed when missing, and loaded by using 
#     lapply of managed_package on required_packages list.
manage_package <- function(package){
    if(!require(package, character.only=TRUE)){
        install.packages(p, dependencies=TRUE)
    }
    require(package, character.only=TRUE, quietly=TRUE)
}

required_packages <- c("data.table",
                       "foreach",
                       "doParallel",
                       "rbenchmark")
invisible(lapply(required_packages, manage_package))
#library(dplyr)
#library(quanteda)
#library(stringi)


# Set options -------------------------------------------------------------
set.seed(42)
data_dir        <- "data"
doc_types       <- c("blogs", "news", "twitter")
drop_all_text   <- TRUE
drop_text_lines <- TRUE
language        <- "en_US"
object_dir      <- "object"
test_fraction   <- 0.05
url_path  <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset"
zip_name  <- "Coursera-SwiftKey.zip"


# Create directories (if needed) ------------------------------------------
if (!dir.exists(object_dir)) {dir.create(object_dir)} # saved R objects
if (!dir.exists(data_dir)) {dir.create(data_dir)}     # all data files
extract_dir  <- paste0(data_dir, "/extracted")
if (!dir.exists(lang_dir)) {dir.create(lang_dir)}     # extracted data
lang_dir  <- paste0(data_dir, "/", language)
if (!dir.exists(lang_dir)) {dir.create(lang_dir)}     # language data

# =========================================================================
# PARAMETERS
# =========================================================================

#   Regex patterns and replacement texts ----------------------------------
abbreviations <- data.frame(
    pattern=paste0("(\\s)(",
                   c("ave", "assn",
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
                     "vs"),  
                   ")(\\.\\s)"),
    replacement=" \\2 ",
    stringsAsFactors=FALSE)

apostrophes <- data.frame(
    pattern=    c("(?<=\\w)(â€™)(?=\\w)",
                  "(?<=\\w)(â)(?=\\w)"),
    replacement="'",
    stringsAsFactors=FALSE)

control_chars <- data.frame(
    pattern=    "[[:cntrl:]]",
    replacement="",
    stringsAsFactors=FALSE)

double_backslashes <- data.frame(
    pattern=    "\\\\",
    replacement="",
    stringsAsFactors=FALSE)


months <- data.frame(
    pattern=paste0("(\\s)(",
                   c("J[aA][nN]",
                     "F[eE][bB]",
                     "M[aA][rR]",
                     "A[pP][rR]",
                     "M[aA][yY]",
                     "J[uU][nN]",
                     "J[uU][lL]",
                     "A[uU][gG]",
                     "S[eE][pP][tT]?",
                     "O[cC][tT]",
                     "N[oO][vV]",
                     "D[eE][cC]"),
                   ")(\\.\\s)"),
    replacement=" \\2 ",
    stringsAsFactors=FALSE)

days <- data.frame(
    pattern=paste0("(\\s)(",
                   c("M[oO][nN]",
                     "T[uU][eE][sS]?",
                     "W[eE][dD]",
                     "T[hH][uU][rR][sS]?",
                     "F[rR][iI]",
                     "S[aA][tT]",
                     "S[uU][nN]"),
                   ")(\\.\\s)"),
    replacement=" \\2 ",
    stringsAsFactors=FALSE)

states <- data.frame(
    pattern=paste0("(\\s)(",
                   c("AL", "AK", "AR", "AZ", 
                     "CA", "Calif", "CO", "Colo" , "CT", 
                     "DC", "DE",
                     "FL", "Fla",
                     "GA",
                     "HI",
                     "IA", "ID", "IL", "IN",
                     "KS", "KY",
                     "LA",
                     "MA", "MD", "ME", "MI", "Mich", "MN", 
                     "MO", "MS", "MT", 
                     "NC", "ND", "NE", "NH", "NJ", "NM", 
                     "NV", "NY",
                     "OH", "OK", "OR",
                     "PA",
                     "RI",
                     "SC", "SD",
                     "TN", "Tenn", "TX", 
                     "UT",
                     "VA", "VT", 
                     "WA", "WI", "Wis", "WV", "WY"),
                   ")(\\.\\s)"),
    replacement=" \\2 ",
    stringsAsFactors=FALSE)

numeric_separators <- data.frame(
    pattern=    c("(\\d)(\\. ?)(\\d)",
                  "(\\d)(,)(\\d{3})",
                  "(\\d)(\\: ?)(\\d)",
                  '(\\d)(\\/ ?)(\\d)',
                  '(\\d)(\\-)(\\d)',
                  '(\\d)( ?\\%)',
                  '(\\$ ?)(\\d)',
                  '(\\-)(\\d)',
                  '(\\+)(\\d)'),
    replacement=c('\\1 POINT \\3',
                  '\\1\\3',
                  '\\1 COLON \\3',
                  '\\1 SLASH \\3',
                  '\\1 RANGE \\3',
                  '\\1 PERCENT',
                  'USD \\2',
                  'NEGATIVE \\2',
                  'POSITIVE \\2'),
    stringsAsFactors=FALSE)

numbers <- data.frame(
    pattern=    c("0", 
                  "1", 
                  "2", 
                  "3", 
                  "4", 
                  "5", 
                  "6", 
                  "7", 
                  "8", 
                  "9"),
    replacement=c("ZERO ", 
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
#   function    make_clean_text(pattern, new_text="")
#   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
make_clean_text <- function(pattern, replacement, ...) {
    function(text_line) {
        gsub(pattern, replacement, text_line, perl=TRUE, ...)
    }
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
#   function    make_parallel(text_lines, regex_pattern, new_text="", 
#                              n_cores=2)
#   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
make_parallel <- function(fn) {
    function(...) {
        parSapply(cl=tmp_cluster, fn(...))
    }
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

full_clean <- function(text_lines, clean_text_type, ...){
    tmp_cluster <- makeCluster(detectCores()-1)
    foreach(i=iter(clean_text_type, by='row')) %do% {
        print(i$pattern)
        print(i$replacement)
        parSapply(cl=tmp_cluster,
                  text_lines,
                  gsub,
                  pattern=i$pattern,
                  new_text=i$text,
                  perl=TRUE,
                  ...)
    }
    stopCluster(tmp_cluster)
}
#   _______________________________________________________________________
#   function    write_text_table(text_table, file_name, write_dir)
#   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
write_text_table <- function(text_table, file_name, write_dir) {
    if (!dir.exists(write_dir)) {dir.create(write_dir)}
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

# Download zipped data from file_url location (if needed) -----------------
zip_file <- paste0(data_dir, "/", zip_name)
if (!file.exists(zip_file)) {
    file_url <- paste0(url_path, "/", zip_name)
    download.file(file_url, zip_file)
}


# Unzip text files to /final/lang_dir (if needed) -------------------------
final_lang_dir  <- paste0(data_dir, "/extracted/final/", language)
if (length(dir(final_lang_dir))==0) {unzip(zip_file, exdir=extract_dir)}


# Read files line by line -------------------------------------------------
text_lines <- lapply(paste0(language, ".", doc_types,".txt"), 
                     read_file_lines, read_dir=lang_dir)


# Convert to list of data tables ------------------------------------------
all_text <- lapply(text_lines, as.data.table)
all_text <- lapply(all_text, setnames, old="V1", new="text_line")
names(all_text) <- doc_types
all_text_counts <- sapply(all_text, nrow)

if (drop_text_lines==TRUE) {rm(text_lines)}


# Remove control characters and double backslashes ------------------------
# tmp_cluster <- makeCluster(usable_cores)
# foreach(d=doc_types) %do% {
#   all_text[[d]]$text_line <- parSapply(cl=tmp_cluster, 
#                                        all_text[[d]]$text_line, 
#                                        clean_text, 
#                                        pattern="[[:cntrl:]]",
#                                        new_text="",
#                                        perl=TRUE)
# }
# foreach(d=doc_types) %do% {
#   all_text[[d]]$text_line <- parSapply(cl=tmp_cluster, 
#                                        all_text[[d]]$text_line, 
#                                        clean_text, 
#                                        pattern="\\\\",
#                                        new_text="",
#                                        perl=TRUE)
# }
# stopCluster(tmp_cluster)


# Split data --------------------------------------------------------------

#   Randomly assign lines to subsets --------------------------------------
all_text <- lapply(all_text, split_data, n_splits=(1/test_fraction))
save(all_text, file=paste0(object_dir,"/all_text.rda"))

#   Select subsets for testing and training -------------------------------
test <- foreach(d=doc_types) %do% {
    all_text[[d]][all_text[[d]]$dataset_id==(1/test_fraction),]
}
names(test) <- doc_types
save(test, file=paste0(object_dir,"/test.rda"))

train <- foreach(d=doc_types) %do% {
    all_text[[d]][all_text[[d]]$dataset_id!=(1/test_fraction),]
}
names(train) <- doc_types
save(train, file=paste0(object_dir,"/train.rda"))

#   Check line counts
test_line_counts  <- sapply(test, nrow)
train_line_counts <- sapply(train, nrow)
all_text_counts == train_line_counts + test_line_counts

if (drop_all_text==TRUE) {rm(all_text)}


#   Write test and train datasets to file ---------------------------------
test_dir <- paste0(lang_dir, "/", "test")
foreach(d=doc_types) %do% {
    write_text_table(test[[d]], paste0(d, ".txt"), test_dir)
}
train_dir <- paste0(lang_dir, "/", "train")
foreach(d=doc_types) %do% {
    write_text_table(train[[d]], paste0(d, ".txt"), train_dir)
}

# =========================================================================
# PREPROCESS 
# =========================================================================
clean_text_types <- c("apostrophes", "months", "days", "states")

clean_text_functions <- lapply(setNames(clean_text_types, clean_text_types),
                               make_clean_text)



clean_apostrophe <- make_clean_text(pattern="(?<=\\w)(â|â€™)(?=\\w)",
                                    new_text=rawToChar(as.raw(39)),
                                    perl=TRUE)
par_clean_apostrophe <- make_parallel(fix_apostrophe)
# Clean lines -------------------------------------------------------------
debug_id <- 1
debug_text <- train[["news"]][train[["news"]]$dataset_id==debug_id, 1]

tmp_cluster <- makeCluster(usable_cores)

#    Fix apostrophes ------------------------------------------------------
debug_text$text_line  <- parSapply(cl=tmp_cluster, 
                                   debug_text$text_line,
                                   clean_text,
                                   pattern="(\\w)(â|â€™|â)(\\w) ",
                                   new_text="\\1'\\3 ",
                                   perl=TRUE,
                                   ignore.case=TRUE)

#    Remove foreign characters --------------------------------------------
debug_text$text_line  <- parSapply(cl=tmp_cluster, 
                                   debug_text$text_line,
                                   clean_text,
                                   pattern="[â¦€™]",
                                   new_text="",
                                   perl=TRUE,
                                   ignore.case=TRUE)

#    Remove periods -------------------------------------------------------

#       in initials -------------------------------------------------------
debug_text$text_line <- parSapply(cl=tmp_cluster,
                                  debug_text$text_line,
                                  clean_text,
                                  pattern="(?<!\\w)([a-zA-Z])\\.",
                                  new_text="\\1",
                                  perl=TRUE)

#        after months -----------------------------------------------------
foreach(mo=months) %do% {
  debug_text$text_line <- parSapply(cl=tmp_cluster, 
                                     debug_text$text_line,
                                     clean_text,
                                     pattern=paste0("(\\s)(", mo, ")(\\.\\s)"),
                                     new_text=paste0(" \\2 "),
                                     perl=TRUE)
}
rm(mo)

#        after days -------------------------------------------------------
foreach(da=days) %do% {
  debug_text$text_line <- parSapply(cl=tmp_cluster, 
                                     debug_text$text_line,
                                     clean_text,
                                     pattern=paste0("(\\s)(", da, ")(\\.\\s)"),
                                     new_text=paste0(" \\2 "),
                                     perl=TRUE)
}
rm(da)

#        after states -----------------------------------------------------
foreach(st=states) %do% {
  debug_text$text_line <- parSapply(cl=tmp_cluster, 
                                     debug_text$text_line,
                                     clean_text,
                                     pattern=paste0("(\\s)(", st, ")(\\.\\s)"),
                                     new_text=paste0(" \\2 "),
                                     perl=TRUE)
} 
rm(st)


#        after abbreviations ----------------------------------------------
foreach(ab=abbreviations) %do% {
  debug_text$text_line <- parSapply(cl=tmp_cluster, 
                                    debug_text$text_line,
                                    clean_text,
                                    pattern=paste0("(\\s)(", ab, ")(\\.\\s)"),
                                    new_text=paste0(" \\2 "),
                                    perl=TRUE,
                                    ignore.case=TRUE)
}

period_pattern <- "[a-z']+\\."
before_periods <- find_strings(debug_text$text_line, period_pattern,
                               ignore.case=TRUE, perl=TRUE)

# Break lines into sentences ----------------------------------------------
debug_text$text_line <- parSapply(cl=tmp_cluster,
                                  debug_text$text_line,
                                  clean_text,
                                  pattern="\\.(?=\\s+[A-Z])",
                                  new_text="\n",
                                  perl=TRUE)

# tmpText <- gsub("(\\. )([[:upper:]])", "\n\\2", tmpText, perl = TRUE)

# Convert to lowercase ----------------------------------------------------
save(debug_text, file=paste0(object_dir,"/debug_text_mixed_case.rda"))
debug_text$text_line <- tolower(debug_text$text_line)

# Replace number and associated separators with UPPERCASE words -----------
foreach(ns=iter(number_separators, by='row')) %do% {
  debug_text$text_line <- parSapply(cl=tmp_cluster,
                                    debug_text$text_line,
                                    clean_text,
                                    pattern=ns$pattern,
                                    new_text=ns$text,
                                    perl=TRUE)
}
rm(ns)

foreach(no=iter(numbers, by='row')) %do% {
  debug_text$text_line <- parSapply(cl=tmp_cluster,
                                    debug_text$text_line,
                                    clean_text,
                                    pattern=no$pattern,
                                    new_text=no$text,
                                    perl=TRUE)
}
rm(no)

# Mark Beginnings and Ends of Sentences (clauses) as BOS and EOS ----------


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
# tmpText <- tolower(tmpText)
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
# tmpText <- gsub("(\\. ?)([[:upper:]])", "POINT \\2", tmpText, perl = TRUE)
# tmpText <- gsub("([[:upper:]] ?)\\%", "\\1 PERCENT", tmpText, perl = TRUE)
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




