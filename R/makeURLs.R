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
#' makeURL(2000, 1, 2003, 10)

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
