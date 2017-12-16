#' API Data Sample from NY Times Archive API
#'
#' This is a sample dataset that collects all the news from 01/01/2000 to 01/31/2000. Within these two months, 
#' there are 15,282 news reported on NY Times.
#' 
#' @format A sample news data of all news on NY Times from 2000-01-01 to 2000-01-31.
#' \describe{
#'   \item{pub_date}{publication date}
#'   \item{headline}{the headline of each article}
#'   \item{word_count}{number of words in each article}
#'   \item{type}{type of the material}
#'   \item{web_url}{web link of each article}
#' @source \url{https://developer.nytimes.com/archive_api.json}
"api_data_sample"



#' Desired data sample after text cleaning
#'
#' This is a sample dataset that collects all the headlines of each news from 01/01/2000 to 01/31/2000. 
#' Different from the previous api dataset, after text cleaning and transformation, the data is now ready
#' for feature engineering - with 60 observations representing 60 days from 01/01/2000 to 01/31/2000, and 
#' 8874 unique words (features) that appears on all the news headlines from NY times.
#' 
#' @format frequency of words appear on NY Times news for each day between 01/01/2000 and 01/31/2000.
#' @source \url{https://developer.nytimes.com/archive_api.json}
"cleaned_data_sample"
