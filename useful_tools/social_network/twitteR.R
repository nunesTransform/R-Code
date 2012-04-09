#################################################################################
#
# author: Simon Müller
# date: 8.4.2012
#  
#
#################################################################################

# load libraries
require(twitteR)
library(ggplot2)
theme_set(theme_bw())


#################################################################################
#
# R function for getting the hourly count of tweets and tweeter
#
#################################################################################
hour_dist <- function(string, n = 1200, days = 5, lang="de", geocode = NULL) {
  
  date <- c()
  hourly.count <- c()
  user <- c()
  for(i in days:1) {
    tw <- searchTwitter(string, n, lang = lang, since = Sys.Date() - (i + 1), 
                        until = Sys.Date() - i, geocode = geocode)
    if(length(tw) > 0){
      res <- twListToDF(tw)
      hourly.count <- c(hourly.count, sapply(res$created, extract_hour))
      date <- c(date, rep(as.character(Sys.Date() - i), length(res$created)))
      user <- c(user, res$screenName)
    }
  }
  tw <- searchTwitter(string, n, lang = lang, since = Sys.Date(), 
                      geocode = geocode)
  if(length(tw) > 0) {
    res <- twListToDF(tw)
    hourly.count <- c(hourly.count, sapply(res$created, extract_hour))
    date <- c(date, rep(as.character(Sys.Date()), length(res$created)))
    user <- c(user, res$screenName)
  }
  
  return(data.frame(date = date, hourly.count = hourly.count, user = user))
}


#################################################################################
#
# Get the hour from the tweets
#
#################################################################################
extract_hour <- function(x) {
  return(as.numeric(strsplit(strsplit(as.character(x), 
                                      split = " ")[[1]][2], ":")[[1]][1]))
}

#################################################################################
#
# Example with plots and tables
#
#################################################################################
df <- hour_dist("#iran")

ggplot(df) + geom_histogram(aes(x=hourly.count), binwidth=1) + 
  facet_wrap(~date) + xlab("") + ylab("") + xlim(c(0,24))

# top ten over all days
sort(table(df$user), decreasing=T)[1:10]

# top ten per day
by(df$user, df$date, FUN <- function(x) {sort(table(x),decreasing=T)[1:10]})
