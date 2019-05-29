require(Quandl)
require(quantmod)
require(quantstrat)
require(twitteR)
require(httr)
require(base64enc)

apple_twdata <- Quandl("NS1/AAPL_US", api_key="be8bCBYYhY1VyYq7AcA8")

getSymbols("AAPL", src = "yahoo", from = "2013-01-01", to = "2017-03-22")
apple_stockdata <- AAPL$AAPL.Adjusted

api_key <- "ZIvxFrlEy3ukjOhuTBpjNAZ3A"
api_secret <- #Personal
access_token <- "	844550525492248576-5H9dJiyEZYMpKIvUN5WvYD7qy4SAn7j"
access_token_secret <- #Personal
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
setup_twitter_oauth(api_key, api_secret)

tweets_apple <- searchTwitter('$AAPL', n = 500, resultType = 'recent')

feed_apple <- laply(tweets_apple, function(t) t$getText())

positive_terms <- read.delim(file='C:/Users/Kenneth/Desktop/positive-words.txt', header=FALSE, stringsAsFactors=FALSE)
negative_terms <- read.delim(file='C:/Users/Kenneth/Desktop/negative-words.txt', header=FALSE, stringsAsFactors=FALSE)

#Sentiment Scorer Function adapted online
score.sentiment <- function(sentences, positive_terms, negative_terms, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, positive_terms, negative_terms) {
    
    
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence <- iconv(sentence, 'UTF-8', 'ASCII')
    sentence = tolower(sentence)
    
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    
    pos.matches = match(words, positive_terms)
    neg.matches = match(words, negative_terms)
    
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, positive_terms, negative_terms, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

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