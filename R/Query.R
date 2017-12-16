#' Data Project: How could social media news influence the stock returns?
#'
#' The fluctuation in the stock market can be reflected and predicted through the most updated news.
#' This project takes news data as an illustration. Specifically, our goal is to find a method
#' to collect and combine both sturctured (numerical) data from Yahoo Finance and unsturctured (text)
#' data from NY Times together by web scraping data from APIs and further text cleaning.
#'
#' @export


# Set your API key

key <- Sys.getenv("NYTIMES_AS_KEY")

# Get data frame from NY Time's Archive API
# Note:
# Due to the high volumn of news data, the following function only collects
# headline, publication date, word count, web URL, and the type of each article published on NY Times.

getDF <- function (urls, key){
  DF <- as.data.frame(c())
  for (j in 1:length(urls)) {
    req <- GET(urls[[j]], query = list(api_key = key))
    result <- httr::content(req)
    meta <- result$response$docs
    df <- as.data.frame(c())
    for (i in 1:length(meta)){
      headline <- meta[[i]]$headline$main           # Headline
      pub_date <- meta[[i]]$pub_date                # Publish date
      word_count <- meta[[i]]$word_count            # Word count
      web_url <- meta[[i]]$web_url                  # Web url
      type <- meta[[i]]$type_of_material            # Type of material
      cols <- as.data.frame(cbind(pub_date, headline, word_count, type, web_url))
      df <- rbind(df, cols)
    }
    DF <- rbind(DF, df)
  }
  return(DF)
}


