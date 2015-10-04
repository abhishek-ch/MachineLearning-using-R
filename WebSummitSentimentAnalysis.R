#https://sites.google.com/site/miningtwitter/questions/sentiment/sentiment
#http://stackoverflow.com/questions/15194436/is-there-any-other-package-other-than-sentiment-to-do-sentiment-analysis-in-r

install.packages("/Volumes/work/data/others/Rstem_0.4-1.zip", repos = NULL, type="source")
install.packages("Rstem", repos = "http://www.omegahat.org/R", type="source")
download.file("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz", "sentiment.tar.gz")
install.packages("sentiment.tar.gz", repos=NULL, type="source")
install.packages("devtools")
library("devtools")
library(sentiment)
install_github("geoffjentry/twitteR", username="geoffjentry")

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

#setup_twitter_oauth(apiKey,apiSecret,access_token,access_token_secret)
setup_twitter_oauth("MIgAEnO0XHTPKdMv3qiGKr6nu","CMYO2quM7fUzcVuvx8JjALiKjC9cnpXeJFqQLtv2pnECJCCZKz")

setup_twitter_oauth("pbeGqyMGAUhIbalLv3gxQjjPo","vrWao49o21nFky2mGoFa06jE7VUV7E4YsqNmAEaifSX7A1ZyTX")
#fetch tweets with word
trendword = searchTwitter("#flyhackfly", n=2000)

# get the text
trendword = sapply(trendword, function(x) x$getText())
#Avoid the non utf-8 characters
trendword=str_replace_all(trendword,"[^[:graph:]]", " ") 


#Copy paste of direct cleaning of String
#based on general

# remove retweet entities
trendword = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
trendword = gsub("@\\w+", "", some_txt)
# remove punctuation
trendword = gsub("[[:punct:]]", "", trendword)
# remove numbers
trendword = gsub("[[:digit:]]", "", trendword)
# remove html links
trendword = gsub("http\\w+", "", trendword)
# remove unnecessary spaces
trendword = gsub("[ \t]{2,}", "", trendword)
trendword = gsub("^\\s+|\\s+$", "", trendword)

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
trendword = sapply(trendword, try.error)

# remove NAs in some_txt
trendword = trendword[!is.na(trendword)]
names(trendword) = NULL




# classify emotion
class_emo = classify_emotion(trendword, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(trendword, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]



# data frame with results
sent_df = data.frame(text=trendword, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))



# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="Twitter Sentiments", y="number of tweets (4000) Latest") +
  ggtitle("Tweets on #flyhackfly \n(classification by emotion)"
  )



# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets(4000)") +
  ggtitle("Sentiment Analysis of Tweets about #flyhackfly")





# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = trendword[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

common <- read.csv("/Volumes/work/project/github/machine-learning-R/common.csv",header=FALSE)
# remove generic and custom stopwords
my_stopwords <- c(stopwords('english'), common)


# remove stopwords
emo.docs = removeWords(emo.docs, my_stopwords)
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)
dev.off()


#######################################retweets###########################################
#http://stackoverflow.com/questions/13649019/with-r-split-time-series-data-into-time-intervals-say-an-hour-and-then-plot-t
#http://stackoverflow.com/questions/10317470/simple-analog-for-plotting-a-line-from-a-table-object-in-ggplot2
#http://blog.ouseful.info/2012/02/17/visualising-twitter-user-timeline-activity-in-r/
#find number of retweets
library(ggplot2)

rdmTweets <- searchTwitter('#trendword', n=500)
#Create a dataframe based around the results
df <- do.call("rbind", lapply(trendword, as.data.frame))
#Here are the columns
names(df)
#And some example content
head(df,3)


counts=table(df$retweetCount)
barplot(counts)
dev.off()
#find retweets maximum than 30
retweetSubset =subset(df,retweetCount > 50)
qplot(screenName,  data=retweetSubset, geom="bar",weight=retweetCount,fill=screenName)


#We can also generate barplots showing the distribution of tweet count over time:
MyDatesTable <- table(cut(df$created, breaks="10 mins"))
ggplot(df,aes(x=cut(df$created, breaks="4 mins")))+geom_bar(aes(y = (..count..)))

