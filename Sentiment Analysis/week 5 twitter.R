library(twitteR) # fetching the tweets
library(ROAuth) # R authentication
library(plyr) # breaking the data into manageable pieces 
library(stringr) # string processing
library(ggplot2) # plotting results
library(RColorBrewer) # dependency 
library(wordcloud) # for wordcloud
library(NLP) # NAtural Language Processing
library(tm) # text mining
library(Rstem) # word stemming
library(sentiment) # sentiment analysis
library(SnowballC)

## Twitter API credentials
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
api_key <- "e2hAYurbg7h8oCCaK572HGzpb"
api_secret <- "em65eNpyeeBfRhZyNJ2pBW2wJvxxsl1tFyKXZlsdefrOonjElM"
access_token <- "836133291136692224-PXZFKrLmtzffzqEqDFVCLGaXUt35Rod"
access_token_secret <- "HzjHWBnParuUYkxxxcomhPhGajMRTiTbNb1p9AhEiS37R"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")
authenticate <- OAuthFactory$new(consumerKey=api_key, consumerSecret=api_secret, requestURL=reqURL, accessURL=accessURL, authURL=authURL)
#authenticate$handshake(cainfo="cacert.pem")
# save for later use for Windows
save(authenticate, file="twitter authentication.Rdata")

# getting tweets
tweets=searchTwitter("demonetisation+UP+BJP", n=400, lang="en", since='2017-03-01', until='2017-03-22')
class(tweets)
head(tweets)
# Convert the tweets to a text format
tweets_txt=sapply(tweets, function(t) t$getText())
# date
tweets_date=lapply(tweets, function(x) x$getCreated())
tweets_date=sapply(tweets_date,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))
# retweet count
isretweet=sapply(tweets, function(x) x$getIsRetweet())
retweetcount=sapply(tweets, function(x) x$getRetweetCount())
# favourite count
favoritecount=sapply(tweets, function(x) x$getFavoriteCount())

data=as.data.frame(cbind(tweet=tweets_txt, date=tweets_date, isretweet=isretweet, retweetcount=retweetcount, favoritecount=favoritecount))

# importing text files for positive and negative words
hu.liu.pos=scan(file="positive-words.txt", what='character', comment.char=';')
hu.liu.neg=scan(file="negative-words.txt", what='character', comment.char=';')

## SCORING FUNCTION
# define error handling function when trying tolower
tryTolower = function(x)
{
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
cleantweets = function(tweets_txt)
{
  # remove retweet entities
  tweets_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets_txt)
  # remove @people
  tweets_txt = gsub("@\\w+", "", tweets_txt)
  # remove hashtags with space so that words are not joint
  tweets_txt = gsub("#"," ",tweets_txt)
  # remove /n
  tweets_txt = gsub("\n","",tweets_txt)
  # remove punctuation
  tweets_txt = gsub("[[:punct:]]", "", tweets_txt)
  # remove numbers
  tweets_txt = gsub("[[:digit:]]", "", tweets_txt)
  # remove html links
  tweets_txt = gsub("http\\w+", "", tweets_txt)
  # removes all besides the alphabets and numbers 
  tweets_txt = gsub("[^A-Za-z0-9]", " ", tweets_txt)
  # remove unnecessary spaces
  tweets_txt = gsub("[ \t]{2,}", " ", tweets_txt)
  tweets_txt = gsub("^\\s+|\\s+$", "", tweets_txt)
  # use tryTolower with sapply 
  tweets_txt = sapply(tweets_txt, tryTolower)
  # result
  return(tweets_txt)
}
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of positive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  # create a simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # preparing text for analysis
                   sentence = cleantweets(sentence)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}
tweet_score=score.sentiment(tweets_txt, hu.liu.pos, hu.liu.neg, .progress='text')

# tweets score distribution
hist(tweet_score$score)
# ggplot2 alternative (better graphics)
qplot(tweet_score$score)

# Ignore the middle
tweet_score$very.pos=as.numeric(tweet_score$score>=2)
tweet_score$very.neg=as.numeric(tweet_score$score<=-2)
pos.count=sum(tweet_score$very.pos)
neg.count=sum(tweet_score$very.neg)
all.count=pos.count+neg.count
score=round(100*pos.count/all.count)
score

## PERFORM SENTIMENT ANALYSIS USING SENTIMENT PACKAGE
# preparing text for analysis
tweets_txt = cleantweets(tweets_txt)

# classify emotion
class_emo = classify_emotion(tweets_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(tweets_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sentiment_df = data.frame(text=tweets_txt, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)
# sort data frame
sentiment_df = within(sentiment_df, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

## SOME PLOTS OF THE OBTAINED RESULT
# plot distribution of emotions
ggplot(sentiment_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets")
# plot distribution of polarity
ggplot(sentiment_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets")

## SEPARATE THE TEXT BY EMOTIONS AND VISUALIZE THE WORDS WITH A COMPARISON CLOUD
# separating text by emotion
emos = levels(factor(sentiment_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = tweets_txt[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}
# create corpus
corpus = Corpus(VectorSource(emo.docs))
# remove stopwords and word 'demonetisation'
corpus = tm_map(corpus, removeWords, c("demonetisation",stopwords("english")))
# stem the document (so variations of same words can be considered as one)
corpus = tm_map(corpus, stemDocument)
# Form Document Term Matrix
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)

