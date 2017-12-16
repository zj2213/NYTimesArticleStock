#' Data Project: How could social media news influence the stock returns?
#'
#' The fluctuation in the stock market can be reflected and predicted through the most updated news.
#' This project takes news data as an illustration. Specifically, our goal is to find a method
#' to collect and combine both sturctured (numerical) data from Yahoo Finance and unsturctured (text)
#' data from NY Times together by web scraping data from APIs and further text cleaning.
#'
#' @export
#' @param key Your New York Times API AS KEY
#' @begin_year The starting year of article data collections
#' @begin_month The starting month of article data collections
#' @end_year The ending year of article data collections
#' @end_month The ending month of article data collections
#' @examples
#' makeURL()
#' getDF(make(URL))


if (!require("pacman")) install.packages("pacman")
pacman::p_load("httr", "dplyr", "tm", "tidytext", "DT")

# Set your API key

key <- Sys.getenv("NYTIMES_AS_KEY")

# Generate NY Time's Archive API base urls

makeURL <- function(begin_year, begin_month, end_year, end_month){
  base_url <- "https://api.nytimes.com/svc/archive/v1/"
  out <- c()
  for(i in (end_year - begin_year)*12 + (end_month - begin_month)){
    for(yr in begin_year:end_year){
      for(mo in begin_month:end_month){
        if(mo <= 12){
          url_new <- paste0(base_url, yr, '/', mo, '.json')
          out <- c(out, url_new)
        }
        mo = mo + 1
      }
      if(yr < end_year) yr = yr + 1
    }
  }
  return(out)
}

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

# Examples - it may take longer time to collect more data through longer periods.
# In this example, it takes about 5 minutes to collect all articles for 2 months from NY Times (from 2000-1 to 2000-2).

urls <- makeURL(begin_year = 2000, begin_month = 1, end_year = 2000, end_month = 2)
DF <- getDF(urls, key)

# saveRDS(DF, "DF.rds")

