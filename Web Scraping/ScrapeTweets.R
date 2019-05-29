require(Quandl)
require(quantmod)
require(quantstrat)
require(twitteR)
require(httr)
require(base64enc)

#Retrieve twitter data from Quandl
apple_twdata <- Quandl("NS1/AAPL_US", api_key="be8bCBYYhY1VyYq7AcA8")

#Retrieve AAPL stock price data and use Adjusted Prices
getSymbols("AAPL", src = "yahoo", from = "2013-01-01", to = "2017-03-22")
apple_stockdata <- AAPL$AAPL.Adjusted

api_key <- "ZIvxFrlEy3ukjOhuTBpjNAZ3A"
api_secret <- ##Personal Key
access_token <- "	844550525492248576-5H9dJiyEZYMpKIvUN5WvYD7qy4SAn7j"
access_token_secret <- ##Personal Key
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
setup_twitter_oauth(api_key, api_secret)

tweets_apple <- searchTwitter('$AAPL', n = 500, resultType = 'recent')

feed_apple <- laply(tweets_apple, function(t) t$getText())

positive_terms <- c('buy', 'good', 'better', 'bull')
negative_terms <- c('sell', 'bad', 'lower', 'bear')

positive_terms <- read.delim(file='C:/Users/Kenneth/Desktop/positive-words.txt', header=FALSE, stringsAsFactors=FALSE)
negative_terms <- read.delim(file='C:/Users/Kenneth/Desktop/negative-words.txt', header=FALSE, stringsAsFactors=FALSE)

score.sentiment <- function(sentences, positive_terms, negative_terms, .progress='none')
{
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, positive_terms, negative_terms) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    #to remove emojis
    sentence <- iconv(sentence, 'UTF-8', 'ASCII')
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, positive_terms)
    neg.matches = match(words, negative_terms)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, positive_terms, negative_terms, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

positive_terms <- read.csv(file='C:/Users/Kenneth/Downloads/Sentiment_Analysis/Positive Terms.csv', 
                           header=FALSE, stringsAsFactors=FALSE))
negative_terms <- read.csv(file='C:/Users/Kenneth/Downloads/Sentiment_Analysis/Negative Terms.csv',
                           header=FALSE, stringsAsFactors=FALSE)

scores_df <- score.sentiment(feed_apple, positive_terms, negative_terms)
head(scores_df)
scores_df <- data.frame(scores_df)

mat= create_matrix(tweet_all, language="english", 
                   removeStopwords=FALSE, removeNumbers=TRUE, 
                   stemWords=FALSE, tm::weightTfIdf)

mat = as.matrix(mat)

classifier = naiveBayes(mat[1:160,], as.factor(sentiment_all[1:160]))
predicted = predict(classifier, mat[161:180,]); predicted

table(sentiment_test, predicted)
recall_accuracy(sentiment_test, predicted)