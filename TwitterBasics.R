#https://sites.google.com/site/miningtwitter/questions/talking-about/wordclouds/wordcloud1
#http://www.rdatamining.com/examples/text-mining
#https://sites.google.com/site/miningtwitter/basics/getting-data/by-twitter
#http://www.r-bloggers.com/r-text-mining-on-twitter-prayformh370-malaysia-airlines/
#twitter authentication - http://thinktostart.com/twitter-authentification-with-r/
#my twitter developer App
#https://apps.twitter.com/app/6536937/keys

library(devtools)
#install_github is package of devtools
install_github("geoffjentry/twitteR", username="geoffjentry")
library(RCurl)
library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(devtools)
library(stringr)



reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

apiKey <-  "MIgAEnO0XHTPKdMv3qiGKr6nu"
apiSecret <- "CMYO2quM7fUzcVuvx8JjALiKjC9cnpXeJFqQLtv2pnECJCCZKz"
access_token <- "69009666-XkI1bcxXtE4qXfOtbRYCgkiJJvpCfsmS0fq4OSq9d"
access_token_secret <- "w89WtxJDAwakPToMqoFtpQYJIfht6YS3a8136hpcyW7eG"

setup_twitter_oauth(apiKey,apiSecret,access_token,access_token_secret)




mach_tweets = searchTwitter("#websummit", n=2000)
#extract the text from tweets in a vector as R easily understands that
#laterI will store the same in file so that I could do the analysis in Java/Python  :) 
#as its easy over there ..hopefully
mach_text = sapply(mach_tweets, function(x) x$getText())
#to avoid the error http://stackoverflow.com/questions/9637278/r-tm-package-invalid-input-in-utf8towcs
mach_text=str_replace_all(mach_text,"[^[:graph:]]", " ") 

#convTweets <- iconv(mach_text, to = "utf-8")
# create a corpus
mach_corpus = Corpus(VectorSource(mach_text))
#tm_map(mach_corpus, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
#tm_map(mach_corpus, function(x) iconv(enc2utf8(x), sub = "byte"))

options(mc.cores=2)
# create document term matrix applying some transformations
tdm = TermDocumentMatrix(mach_corpus,
                         control = list(removePunctuation = TRUE,
                                        stopwords = c("for", "all", "this","your","try","from","its","off",
                                                      "has","well","are","will","hey","let","the","but",
                                                      "that","can","make","open","get","out",stopwords("english")),
                                        removeNumbers = TRUE, tolower = TRUE,
                                        minWordLength=1)
                            )
                         
                         


findFreqTerms(tdm, lowfreq=7)
findAssocs(tdm, 'awesome', 0.30)


# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)


#http://www.r-bloggers.com/word-cloud-in-r/
# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"),freq=6)
pal <- brewer.pal(8, "Dark2")
pal <- pal[-(1:2)]
wordcloud(dm$word, dm$freq,scale=c(1,2),min.freq=10,max.words=100, random.order=F, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))


# save the image in png format
png("summit.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()





##################################Sentiment Analysis################################
#https://sites.google.com/site/miningtwitter/questions/sentiment/sentiment
#http://stackoverflow.com/questions/15194436/is-there-any-other-package-other-than-sentiment-to-do-sentiment-analysis-in-r

install.packages("C:/Users/achoudhary/Downloads/Rstem_0.4-1.zip", repos = NULL, type="source")

install.packages("Rstem", repos = "http://www.omegahat.org/R", type="source")
download.file("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz", "sentiment.tar.gz")
install.packages("sentiment.tar.gz", repos=NULL, type="source")
library(sentiment)

library(twitteR)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(stringr)


reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

apiKey <-  "MIgAEnO0XHTPKdMv3qiGKr6nu"
apiSecret <- "CMYO2quM7fUzcVuvx8JjALiKjC9cnpXeJFqQLtv2pnECJCCZKz"
access_token <- "69009666-XkI1bcxXtE4qXfOtbRYCgkiJJvpCfsmS0fq4OSq9d"
access_token_secret <- "w89WtxJDAwakPToMqoFtpQYJIfht6YS3a8136hpcyW7eG"

setup_twitter_oauth(apiKey,apiSecret,access_token,access_token_secret)

#fetch tweets
nexus = searchTwitter("nexus 6", n=1500)

# get the text
nexus = sapply(nexus, function(x) x$getText())
#Avoid the non utf-8 characters
nexus=str_replace_all(nexus,"[^[:graph:]]", " ") 


#Copy paste of direct cleaning of String
#based on general

# remove retweet entities
nexus = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
nexus = gsub("@\\w+", "", some_txt)
# remove punctuation
nexus = gsub("[[:punct:]]", "", nexus)
# remove numbers
nexus = gsub("[[:digit:]]", "", nexus)
# remove html links
nexus = gsub("http\\w+", "", nexus)
# remove unnecessary spaces
nexus = gsub("[ \t]{2,}", "", nexus)
nexus = gsub("^\\s+|\\s+$", "", nexus)

# define "tolower error handling" function 
try.error = function(x)
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
# lower case using try.error with sapply 
nexus = sapply(nexus, try.error)

# remove NAs in some_txt
nexus = nexus[!is.na(nexus)]
names(nexus) = NULL




# classify emotion
class_emo = classify_emotion(nexus, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(nexus, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]



# data frame with results
sent_df = data.frame(text=nexus, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))



# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets") +
  ggtitle("Google Neus 6 Tweets \n(classification by emotion)"
      )