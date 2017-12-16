# A glimpse of our scrapped dataset

datatable(DF[1:5,])

# Some summary statistics about the data

DF %>%
  select(pub_date, headline, type) %>%
  group_by(type) %>%
  count(sort = TRUE) %>%
  head(10)


# Grouping text by "NEWS" and date
filtered <- DF %>%
  select(pub_date, headline, type) %>%
  filter(type == "News") %>%
  group_by(pub_date) %>%
  summarise(headline = paste(headline, collapse = " "))
filtered$pub_date <- as.Date(filtered$pub_date)

# Text cleanning
# Define extra stop words
STOP_WORDS = c("ax","i","you","edu","s","t","m","subject","can","lines","re","what","low",
               "there","all","we","one","the","a","an","of","or","in","for","by","on","alway",
               "but","is","with","as","was","if","they","are","this","and","it","have","always",
               "from","at","my","be","that","to","com","org","like","likes","so", "get", "enough",
               "also","feel", "lots", "often", "others", "make", "need", "tell","things","still",
               "well","really","take","anyone","two","ive","see","think","here","did","try","until",
               "back","better","day","christians","different","find","first","going","good","least","might",
               "mean","look","long","must","never","new","old","probably","sure","someone",
               "seems","read","thing","using","without","word","years","believe","please","point",
               "right","question","part","help","article","around","order","got","made","replyto","rather",
               "seen","email","though","true","when","post","anything","thought","says","place",
               "looking","usually","its","why","else","will","used","including","next","most",
               "close","does","etc","power","however","how","said","another","actually","allow",
               "any","come","even","just","know","use","want","way","say","now","may","much","writ",
               "something","many", "since", "plus", "beyond", "learned", "current", "would", "given"
)

# Cleaning string text
string_cleaning <- function(string_text) {
  # remove entities
  string_text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", string_text)
  # remove at people
  string_text = gsub("[']", "", string_text)
  # remove punctuation
  string_text = gsub("[[:punct:]]", "", string_text)
  # remove numbers
  string_text = gsub("[[:digit:]]", "", string_text)
  # remove html links
  string_text = gsub("http\\w+", "", string_text)
  # remove unnecessary spaces
  string_text = gsub("[ \t]{2,}", "", string_text)
  string_text = gsub("^\\s+|\\s+$", "", string_text)
  # define "tolower error handling" function
  try.error = function(x) {
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    # result
    return(y)
  }
  # lower case using try.error with sapply
  string_text = sapply(string_text, try.error)
  # remove NAs in string_text
  string_text = string_text[!is.na(string_text)]
  names(string_text) = NULL

  # Remove stop words
  rm_words <- function(string, words) {
    stopifnot(is.character(string), is.character(words))
    spltted <- strsplit(string, " ", fixed = TRUE) # fixed = TRUE for speedup
    vapply(spltted, function(x) paste(x[!tolower(x) %in% c(stopwords("english"), STOP_WORDS)], collapse = " "),
           character(1))
  }

  string_text = rm_words(string_text, stopwords("english"))
  return(string_text)
}


sotu_df <- filtered
sotu_clean <- string_cleaning(sotu_df$headline)
sotu_corpus <- VCorpus(DataframeSource(cbind(sotu_df$pub_date,
                                             sotu_clean)))
sotu_stemmed <- tm_map(sotu_corpus, stemDocument, lazy = TRUE)


sotu_tdm <- TermDocumentMatrix(sotu_stemmed)
# Remove sparsity
# Remove sparsity
sotu_m <- as.matrix(sotu_tdm) # Convert TDM to a matrix
mat <- as.data.frame(t(sotu_m[-c(1:60),]))

data <- cbind(sotu_df$pub_date, mat)
names(data)[1]<-"pub_date"



