# =========================================================================
# REMINDERS
# =========================================================================
# 1. Check regex substitution of '_eos_' for '\\.$' is working correctly.
#    In debug_text[examples] substitution appeared to result in `. _eos_`
# 2. Decide on whether to preserve email addresses, e.g., protect '.', '_',
#    etc. and either leave '@` or substitute with '_at_' 
# 3. Combine regex patterns where possible to reduce time in foreach loops
# 4. Create and test list of regex patterns



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
    if(!require(package, character.only=TRUE, quietly=TRUE)){
        install.packages(package, dependencies=TRUE, quiet=TRUE)
    }
    require(package, character.only=TRUE, quietly=TRUE)
}

required_packages <- c("data.table",
                       "foreach",
                       "doParallel",
                       "rbenchmark",
                       "quanteda",
                       "caret")
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
if (!dir.exists(object_dir)) {dir.create(object_dir)}    # saved R objects
if (!dir.exists(data_dir)) {dir.create(data_dir)}        # all data files
extract_dir  <- paste0(data_dir, "/extracted")
if (!dir.exists(extract_dir)) {dir.create(extract_dir)}  # extracted data
lang_dir  <- paste0(data_dir, "/", language)
if (!dir.exists(lang_dir)) {dir.create(lang_dir)}        # language data

# =========================================================================
# PARAMETERS
# =========================================================================

#   Regex patterns and replacement texts ----------------------------------
abbreviations <- data.frame(
    pattern=paste0("(?<=\\s)(",
                   c('am', 'assn', 'ave',
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
                     'pm', "pp", "prof", "prop",
                     "rep", "rev",
                     "sen",
                     "rd",                    
                     "sgt", "sr", "st",
                     "vs"),  
                   ")(\\.)"),
    replacement="\\1",
    stringsAsFactors=FALSE)

acronyms <- data.frame(
    pattern="(?<=\\s\\w)(\\.)(?=\\w)",
    replacement="",
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
    pattern=paste0('(?<=\\s)',
                   c("jan",
                     "feb",
                     "mar",
                     "apr",
                     "may",
                     "jun",
                     "Jul",
                     "aug",
                     "sept?",
                     "oct",
                     "nov",
                     "dec"),
                   "\\."),
    replacement='\\1',
    stringsAsFactors=FALSE)

days <- data.frame(
    pattern=paste0('(?<=\\s)(',
                   c('M[oO][nN]?',
                     "T[hHuU][eEuU]?[rR]?[sS]?",
                     "W[eE][dD]?",
                     "F[rR][iI]?",
                     "S[aAuU][nNtT]?"),
                   ")(\\.)"),
    replacement="\\1 ",
    stringsAsFactors=FALSE)

states <- data.frame(
    pattern=paste0('(?<=\\s)(',
                   c('A[KLRZ]', 
                     "C[AOT](lif|lo)?",
                     "D[CE]",
                     "FLa?",
                     "GA",
                     "HI",
                     "I[ADLN]",
                     "K[SY]",
                     "LA",
                     "M[ADEINOST]", "Mich",
                     "N[CDEHJMVY]", 
                     "O[HKR]",
                     "PA",
                     "RI",
                     "S[CD]",
                     "T[NX]", "Tenn",
                     "UT",
                     "V[AT]",
                     "W[AIVY]", "Wis"),
                   ")(\\.)"),
    replacement=" \\1 ",
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
    replacement=c('\\1 _point_ \\3',
                  '\\1\\3',
                  '\\1 _range_ \\3',
                  '\\1 _slash_ \\3',
                  '\\1 _range_ \\3',
                  '\\1 _percent_',
                  '_usd_\\2',
                  '_negative_ \\2',
                  '_positive_ \\2'),
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
    replacement=paste0("_", 
                       c("zero", 
                         "one", 
                         "two", 
                         "three", 
                         "four", 
                         "five", 
                         "six", 
                         "seven",
                         "eight", 
                         "nine"),
                       "_ "),
    stringsAsFactors=FALSE)

underscores <- data.frame(
    pattern='_',
    replacement=' ',
    stringsAsFactors=FALSE)

begin_end <- data.frame(
    pattern=c('^', '\\.$'),
    replacement=c('_bos_', '_eos_'),
    stringsAsFactors=FALSE)

other <- data.frame(
    pattern='[^a-z\'_]',
    replacement=' ',
    stringsAsFactors=FALSE)

punctuation <- data.frame(
    pattern=c('[\\.\\?\\"\\(\\)\\[\\]\\{\\}\\<\\>\\:\\;\\!\\-]',
              '&'),
    replacement=c('_eos_ _bos_', 'and'),
    stringsAsFactors=FALSE)
                                    
# =========================================================================
# FUNCTIONS
# =========================================================================

#   _______________________________________________________________________
#   function    parallel_clean(text_lines, pattern, replacement)
#   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
parallel_clean <- function(text_lines, pattern, replacement, ...){
    cl <- makeCluster(detectCores()-1)
    registerDoParallel(cl)
    text_lines <- foreach(text_line=text_lines, .combine='c') %dopar% {
        gsub(pattern, replacement, text_line, perl=TRUE, ...)
    }
    stopCluster(cl)
    text_lines
}

#   _______________________________________________________________________
#   function    clean_text(text_lines, regex_table)
#   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
clean_text <- function(text_lines, regex_table, ...){
    foreach(i=1:dim(regex_table)[1]) %do% {
        text_lines <- parallel_clean(text_lines,
                                     regex_table$pattern[i],
                                     regex_table$replacement[i],
                                     ...)
    }
    rm(i)
    text_lines
}


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
                     read_file_lines, read_dir=final_lang_dir)


# Convert to list of data tables ------------------------------------------
all_text <- lapply(text_lines, as.data.table)
all_text <- lapply(all_text, setnames, old="V1", new="text_line")
names(all_text) <- doc_types
all_text_counts <- sapply(all_text, nrow)

if (drop_text_lines==TRUE) {rm(text_lines)}



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


# =========================================================================
# PREPROCESS 
# =========================================================================



# DEBUG -------------------------------------------------------------------

#   DEBUG: Create and load debug_text -------------------------------------
debug_id <- 1
debug <- train[["news"]][train[["news"]]$dataset_id==debug_id, 1]
save(debug, file=paste0(object_dir,'/debug.rda'))
rm(test)
rm(train)


load('object/debug.rda')


#   DEBUG: Create examples ------------------------------------------------
abbrevation_example <- c(877, 925)
apostrophe_example <- c(865, 875, 879, 885, 887, 938, 1000)
backslash_example <- c(843, 853, 863)
character_example <- c(859)
datetime_example <- c(855, 945, 948, 949, 955)
number_example <- c(884, 894, 905, 907, 924, 930, 945, 948, 949, 955)
state_example <- c(869, 887)
examples <- c(843, 850, 853, 855, 857, 859,
              861, 862, 863, 865, 869, 875, 877, 879, 
              884, 885, 887, 894, 
              905, 907, 
              924, 925, 930, 938, 
              945, 948, 949, 955, 
              1000)


#   DEBUG: Clean apostrophes ----------------------------------------------
debug$text_line <- clean_text(debug$text_line, apostrophes)

#   DEBUG: Clean acronyms -------------------------------------------------
debug$text_line <- clean_text(debug$text_line, acronyms)

#   DEBUG: Clean months ---------------------------------------------------
debug$text_line <- clean_text(debug$text_line, months,
                                   ignore.case=TRUE)

#   DEBUG: Clean days -----------------------------------------------------
debug$text_line <- clean_text(debug$text_line, days)

#   DEBUG: Clean abrreviations --------------------------------------------
debug$text_line <- clean_text(debug$text_line, abbreviations,
                                   ignore.case=TRUE)

#   DEBUG: Clean states ---------------------------------------------------
debug$text_line <- clean_text(debug$text_line, states,
                                   ignore.case=TRUE)

#   DEBUG: Clean underscores ----------------------------------------------
debug$text_line <- clean_text(debug$text_line, underscores)

#   DEBUG: Clean numeric separators ---------------------------------------
debug$text_line <- clean_text(debug$text_line, numeric_separators)

#   DEBUG: Clean numbers --------------------------------------------------
debug$text_line <- clean_text(debug$text_line, numbers)

#   DEBUG: Mark beginning and end of documents-----------------------------
debug$text_line <- clean_text(debug$text_line, begin_end)

#   DEBUG: Mark beginning and end of sentences ----------------------------
debug$text_line <- clean_text(debug$text_line, punctuation)

#   DEBUG: Clean other characters -----------------------------------------
debug$text_line <- clean_text(debug$text_line, punctuation)



# =========================================================================
# CREATE CORPUS
# =========================================================================
# trainNews <- textfile(list.files(path = "train/clean", pattern = "news.txt$", 
#                                  full.names = TRUE, recursive = TRUE), cache = FALSE)
# devNews <- textfile(list.files(path = "develop/clean", pattern = "news.txt$", 
#                                full.names = TRUE, recursive = TRUE), cache = FALSE)
# 
# news <- corpus(trainNews) 
# dNews <- corpus(trainNews) + corpus(devNews)

debug_corpus <- corpus(debug$text_line)



