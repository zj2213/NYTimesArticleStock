#' Data Project: How could social media news influence the stock returns?
#'
#' The fluctuation in the stock market can be reflected and predicted through the most updated news.
#' This project takes news data as an illustration. Specifically, our goal is to find a method
#' to collect and combine both sturctured (numerical) data from Yahoo Finance and unsturctured (text)
#' data from NY Times together by web scraping data from APIs and further text cleaning.
#'
#' @export
#' @param string_text The raw string text received from NY Times

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