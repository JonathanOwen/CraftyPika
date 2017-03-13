# 0.   REMINDERS ----------------------------------------------------------

# 1.   SETUP --------------------------------------------------------------

# 1.1  [SETUP] Load required packages -------------------------------------

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

required_packages <- c('data.table',
                       'dplyr',
                       'dtplyr',
                       "foreach",
                       "doParallel",
                       "microbenchmark",
                       "quanteda",
                       'stringr',
                       'tidytext')
invisible(lapply(required_packages, manage_package))


# 1.2  [SETUP] Set options ------------------------------------------------
set.seed(42)
data_dir        <- "data"
doc_types       <- c("blogs", "news", "twitter")
drop_docs       <- TRUE
language        <- "en_US"
object_dir      <- "object"
test_fraction   <- 0.05
url_path  <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset"
zip_name  <- "Coursera-SwiftKey.zip"


# 1.3   [SETUP] Create directories (if needed) ----------------------------
if (!dir.exists(object_dir)) {dir.create(object_dir)}    # saved R objects
if (!dir.exists(data_dir)) {dir.create(data_dir)}        # all data files
extract_dir  <- paste0(data_dir, "/extracted")
if (!dir.exists(extract_dir)) {dir.create(extract_dir)}  # extracted data
lang_dir  <- paste0(data_dir, "/", language)
if (!dir.exists(lang_dir)) {dir.create(lang_dir)}        # language data


# 2.    FUNCTIONS ---------------------------------------------------------

#   -----------------------------------------------------------------------
#   function    read_file_lines(file_name, read_dir)
#   -----------------------------------------------------------------------
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


#   -----------------------------------------------------------------------
#   function    split_data(dataset, n_splits=20)
#   -----------------------------------------------------------------------
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
docs <- lapply(paste0(language, ".", doc_types,".txt"), 
                     read_file_lines, read_dir=final_lang_dir)


# Convert to list of data tables ------------------------------------------
docs <- lapply(docs, as.data.table)
names(docs) <- doc_types
docs <- lapply(docs, setnames, old="V1", new="doc")

# Count docs --------------------------------------------------------------
doc_counts <- sapply(docs, nrow)

# Split data --------------------------------------------------------------
#   Randomly assign lines to subsets --------------------------------------
docs <- lapply(docs, split_data, n_splits=(1/test_fraction))

# Convert to single data table --------------------------------------------
docs <- rbindlist(docs)

# Create & bind column vector of doc_type labels --------------------------
doc_type <- foreach(doc_type=doc_types, .combine = 'c') %do% {
    rep(doc_type, doc_counts[doc_type])
}
docs <- cbind(doc_type, docs)
rm(doc_type)

# Save docs data table object ---------------------------------------------
save(docs, file=paste0(object_dir,"/docs.rda"))
if (drop_docs==TRUE) {rm(docs)}
load(paste0(object_dir,"/docs.rda"))

docs$doc <- docs$doc                                                      %>%
    # apostrophes
    str_replace_all(coll('â', ignore.case=TRUE), "'")                     %>%
    # periods in acronyms
    str_replace_all('(?<=\\s\\w)(\\.)(?=\\w)', '')                        %>%
    # periods following month abbreviations
    str_replace_all(
        c('(?<=\\s)([Jj][AaUu][LlNn])\\.' = '\\1',
          '(?<=\\s)([Ff][Ee][Bb])\\.' = '\\1',
          '(?<=\\s)([Mm][Aa][RrYy])\\.' = '\\1',
          '(?<=\\s)([Aa][PpUu][RrGg])\\.' = '\\1',
          '(?<=\\s)([Ss][Ee][Pp][Tt]?)\\.' = '\\1',
          '(?<=\\s)([Oo][Cc][Tt])\\.' = '\\1',
          '(?<=\\s)([Nn][Oo][Vv])\\.' = '\\1',
          '(?<=\\s)([Dd][Ee][Cc])\\.' = '\\1'))                          %>%
    # periods following day of the week abbreviations
    str_replace_all(
        c('(?<=\\s)([Mm][Oo][Nn]?)\\.' = '\\1',
          '(?<=\\s)(T[hHuU][eEuU]?[rR]?[sS]? )\\.' = '\\1',
          '(?<=\\s)(W[eE][dD]?)\\.' = '\\1',
          '(?<=\\s)([Ff][Rr][Ii]?)\\.' = '\\1',
          '(?<=\\s)(S[AaUu][NnTt]?)\\.' = '\\1'))                        %>%
    # periods following common abbreviations
    str_replace_all(
        c('(?<=\\s)([AaPp][MmPp])\\.' = '\\1', 
          '(?<=\\s)([Aa][Ss]+[Nn])\\.' = '\\1',
          '(?<=\\s)([Aa][Vv][Ee])\\.' = '\\1',
          '(?<=\\s)([Bb][Ll][Vv][Dd])\\.' = '\\1', 
          '(?<=\\s)([Bb][Rr][Oo][Ss])\\.' = '\\1',
          '(?<=\\s)([Cc][Aa][Pp][Tt])\\.' = '\\1',
          '(?<=\\s)([Cc][Oo][LlMm])\\.' = '\\1',
          '(?<=\\s)([Dd][Rr])\\.' = '\\1',
          '(?<=\\s)(etc)\\.' = '\\1',
          '(?<=\\s)([Ee][Gg])\\.' = '\\1',
          '(?<=\\s)([Gg][Ee][Nn])\\.' = '\\1',
          '(?<=\\s)([Gg][Oo][Vv])\\.' = '\\1',
          '(?<=\\s)(ie)\\.' = '\\1',
          '(?<=\\s)([Ii][Nn][Cc])\\.' = '\\1',
          '(?<=\\s)([Jj][Rr])\\.' = '\\1',
          '(?<=\\s)([Ll][Tt])\\.' = '\\1',
          '(?<=\\s)([Mm][RrSs]{1,2})\\.' = '\\1',
          '(?<=\\s)([Nn][Ee][Tt])\\.' = '\\1',
          '(?<=\\s)([Oo][Rr][Gg])\\.' = '\\1',
          '(?<=\\s)([Pp][Rr][Oo][FfPp])\\.' = '\\1',
          '(?<=\\s)([RrDd])\\.' = '\\1',
          '(?<=\\s)([Rr][Ee][PpVv])\\.' = '\\1',
          '(?<=\\s)([Ss][Ee][Nn])\\.' = '\\1',
          '(?<=\\s)([Ss][Gg]?[RrTt])\\.' = '\\1',
          '(?<=\\s)([Vv][Ss])\\.' = '\\1'))                              %>%
    # periods following U.S. state abbreviations
    str_replace_all(
        c('(?<=\\s)([Aa][KkLlRrZz])\\.' = '\\1', 
          '(?<=\\s)([Cc][AaOoTt]([Ll][Ii][Ff]|[Ll][Oo])?)\\.' = '\\1',
          '(?<=\\s)([Dd][CcEe])\\.' = '\\1',
          '(?<=\\s)([Ff][Ll][Aa]?)\\.' = '\\1',
          '(?<=\\s)([Gg][Aa])\\.' = '\\1',
          '(?<=\\s)([Hh][Ii])\\.' = '\\1',
          '(?<=\\s)([Ii][AaDdLlNn])\\.' = '\\1',
          '(?<=\\s)([Kk][SsYy])\\.' = '\\1',
          '(?<=\\s)([Ll][Aa])\\.' = '\\1',
          '(?<=\\s)([Mm][AaDdEeNnOoSsTt])\\.' = '\\1',
          '(?<=\\s)([Mm][Ii]([Cc][Hh])?)\\.' = '\\1',
          '(?<=\\s)([Nn][CcDdEeHhJjMmVvYy])\\.' = '\\1', 
          '(?<=\\s)([Oo][HhKkRr])\\.' = '\\1',
          '(?<=\\s)([Pp][Aa])\\.' = '\\1',
          '(?<=\\s)([Rr][Ii])\\.' = '\\1',
          '(?<=\\s)([Ss][CcDd])\\.' = '\\1',
          '(?<=\\s)([Tt]([Nn]|[Ee]?[Xx]|[Ee][Nn]{2}))\\.' = '\\1',
          '(?<=\\s)([Uu][Tt])\\.' = '\\1',
          '(?<=\\s)([Vv][AaT])\\.' = '\\1',
          '(?<=\\s)([Ww][AaVvYy])' = '\\1', 
          '(?<=\\s)([Ww][Ii][Ss]?[Cc]?)\\.' = '\\1'))         %>%
    # convert docs to lowercase
    str_to_lower()                                                        %>%
    # remove angle brackets
    str_replace_all('[<>]', ' ')                                          %>%
    # replace non-alphanumeric characters in urls and email addresses
    str_replace_all(
        c('([a-z0-9._%+-]+)(@)([a-z0-9.-]+)(\\.)([a-z]{2,})' 
          = '\\1 <at> \\3 <dot> \\5',
          '(https?|ftp)(:\\/\\/)([a-z0-9_%+-]+)(\\.)([a-z]{2,})(\\.)([a-z]{2,})'
          = '\\1 <colon> <slash> <slash> \\3 <dot>  \\5 <dot> \\7',
          '(https?|ftp)(:\\/\\/)([a-z0-9_%+-]+)(\\.)([a-z]{2,})'
          = '\\1 <colon> <slash> <slash> \\3 <dot>  \\5',
          '(w{2,3}[0-9]?)(\\.)([a-z0-9_%+-]+)(\\.)([a-z]{2,})(\\.)([a-z]{2,})'
          = '\\1 <dot> \\3 <dot>  \\5 <dot> \\7',
          '(w{2,3}[0-9]?)(\\.)([a-z0-9_%+-]+)(\\.)([a-z]{2,})'
          = '\\1 <dot> \\3 <dot>  \\5'))                                  %>%
    # replace symbols and separators near numbers
    str_replace_all(
        c('(\\d)(\\. ?)(\\d)' = '\\1 <point> \\3',
          '(\\d)(,)(\\d{3})' = '\\1\\3',
          '(\\d)(\\: ?)(\\d)' = '\\1 <range> \\3',
          '(\\d)(\\/ ?)(\\d)'= '\\1 <slash> \\3',
          '(\\d)(\\-)(\\d)'=  '\\1 <range> \\3',
          '(\\d)( ?\\%)' = '\\1 <percent>',
          '(\\$ ?)(\\d)'= '<usd> \\2',
          '(\\-)(\\d)' = '<negative> \\2',
          '(\\+)(\\d)' = '<positive> \\2'))                               %>%
    # replace numbers
    str_replace_all(
        c('0' = 'zero>', 
          '1' = '<one>', 
          '2' = '<two>',  
          '3' = '<three>',  
          '4' = '<four>',  
          '5' = '<five>',  
          '6' = '<six>',
          '7' = '<seven>', 
          '8' = '<eight>',  
          '9' = '<nine>'))                                                %>%
    # replace mark beginning and end of sentences/clauses
    str_replace_all(
        c('^' = '<bos> ',
          '$' = ' <eos>',
          '[.?"()\\[\\]{}:;!]' =  ' <eos> <bos> ',
          '\\-{2,}' =  ' <eos> <bos> ',
          '&' = ' and '))                                                 %>%
    # remove non-ascii characters
    str_replace_all('[^a-z\'<>]', ' ')                                    %>%
    # clean up near apostrophes
    str_replace_all(c("(?<=n)\\'\\s+(?=t)" = "'",
                      "(?<=[id])\\'\\s+(?=ve)" = "'",
                      "(?<=[i])\\'\\s+(?=[dm])" = "'",
                      "(?<=[a-z])\\'\\s+(?=s)" = "'",
                      "(?<=\\s)\\'(?=\\s)" = " ",
                      "(?<!s)\\'(?=\\s)" = ""))                        %>%
    # remove repeated spaces and empty sentences
    str_replace_all(
        c('\\s{2,}' = ' ',
          '(<bos> ){2,}' = '<bos> ',
          '( <eos>){2,}' = ' <eos>',
          '<eos> <bos> <eos>' = '<eos>'))

save(docs, file=paste0(object_dir,"/clean_docs.rda"))







#   Select subsets for testing and training -------------------------------
test <- foreach(d=doc_types) %do% {
    all_docs[[d]][all_docs[[d]]$dataset_id==(1/test_fraction),]
}
names(test) <- doc_types
test_doc_counts  <- sapply(test, nrow)
test <- rbindlist(test, use.names = TRUE)
doc_type <- foreach(doc_type=doc_types, .combine='c') %do% {
    c(rep(doc_type, test_doc_counts[doc_type]))}
test <- cbind(test, doc_type)
save(test, file=paste0(object_dir,"/test.rda"))


train <- foreach(d=doc_types) %do% {
    all_docs[[d]][all_text[[d]]$dataset_id!=(1/test_fraction),]
}
names(train) <- doc_types
train_doc_counts <- sapply(train, nrow)
train <- rbindlist(train, use.names = TRUE)
doc_type <- foreach(doc_type=doc_types, .combine='c') %do% {
    c(rep(doc_type, train_doc_counts[doc_type]))}
train <- cbind(train, doc_type)
save(train, file=paste0(object_dir,"/train.rda"))

#   Check line counts


all_docs_counts == train_doc_counts + test_doc_counts

if (drop_all_docs==TRUE) {rm(all_docs)}


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

debug_corpus <- corpus(debug$doc)
