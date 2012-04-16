#################################################################################
#
# author: Simon Müller
# date: 8.4.2012
# update: 11.4: added wordcloud
#  
#
#################################################################################

# load libraries
library(twitteR)
library(ggplot2)
library(tm)
library(wordcloud)
theme_set(theme_bw())


#################################################################################
#
# R function for getting the hourly count of tweets and tweeter
#
#################################################################################
hour_dist <- function(string, n = 1200, days = 6, lang = "de", plot = TRUE, 
                      geocode = NULL, min.freq = 3) {
  
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
      if(plot) {
        myDtm <- word_cloud(res)
        plot_wc(myDtm, min.freq)
        X11()
      }
    }
  }
  tw <- searchTwitter(string, n, lang = lang, since = Sys.Date(), 
                      geocode = geocode)
  if(length(tw) > 0) {
    res <- twListToDF(tw)
    hourly.count <- c(hourly.count, sapply(res$created, extract_hour))
    date <- c(date, rep(as.character(Sys.Date()), length(res$created)))
    user <- c(user, res$screenName)
    if(plot) {
      myDtm <- word_cloud(res)
      plot_wc(myDtm, min.freq)
    }
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
# Calculate cloud
#
#################################################################################
word_cloud <- function(data) {

  # calc corpus
  myCorpus <- Corpus(VectorSource(data$text))
  
  # Großbuchstaben -> Kleinbuchstaben
  myCorpus <- tm_map(myCorpus, tolower)
  
  # Lösche Satzzeichen
  myCorpus <- tm_map(myCorpus, removePunctuation)
  
  # Lösche Zahlen
  myCorpus <- tm_map(myCorpus, removeNumbers)
  
  myStopwords <- c(stopwords("german"), "heute", "die", "mehr", "am", "noch", 
                   "neue", "du", "ab", "ersten", "wir", "das")
  
  myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
  
  ###
  #
  # Wortstamm
  #
  ###
  dictCorpus <- myCorpus
  myCorpus <- tm_map(myCorpus, stemDocument, "german")
  myCorpus <- tm_map(myCorpus, stemCompletion, dictionary = dictCorpus)
  
  ###
  #
  # Erstelle Dokumentenmatrix
  #
  ###
  myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
  return(myDtm)
}

#################################################################################
#
# Plot Wordcloud
#
#################################################################################

plot_wc <- function(myDtm, min.freq = 3) {
  v <- sort(rowSums(as.matrix(myDtm)), decreasing = TRUE)
  myNames <- names(v)
  d <- data.frame(word = myNames, freq = v)
  wordcloud(d$word, d$freq, min.freq = min.freq)
}

#################################################################################
#################################################################################
#################################################################################


#################################################################################
#
# Example with plots and tables of top ten tweeter
#
#################################################################################
<<<<<<< HEAD
df <- hour_dist("#syrien", plot = FALSE)
=======
df <- hour_dist("#iran")
>>>>>>> 5ce996e02f29aff0dbc1702baa69cc39462be076

ggplot(df) + geom_histogram(aes(x=hourly.count), binwidth=1) + 
  facet_wrap(~date) + xlab("") + ylab("") + xlim(c(0,24))

dff <- as.data.frame(table(df$user))
names(df) <- c("a", "b")
wordcloud(dff[,1], dff[,2], min.freq = 5)

# top ten over all days
sort(table(df$user), decreasing=T)[1:10]

# top ten per day
by(df$user, df$date, FUN <- function(x) {sort(table(x),decreasing=T)[1:10]})
