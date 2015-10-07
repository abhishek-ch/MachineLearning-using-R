library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(devtools)
install.packages(c("rjson", "bit64", "httr"))
library("devtools")
install_github("geoffjentry/twitteR", username="geoffjentry")
library(httpuv)
download.file("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz", "sentiment.tar.gz")
install.packages("sentiment.tar.gz", repos=NULL, type="source")
library(sentiment)
library(ggplot2)

setup_twitter_oauth("MIgAEnO0XHTPKdMv3qiGKr6nu","CMYO2quM7fUzcVuvx8JjALiKjC9cnpXeJFqQLtv2pnECJCCZKz")

mach_tweets = searchTwitter("startupirl", n=1000, lang="en")

mach_text = sapply(mach_tweets, function(x) x$getText())

trendword = sapply(mach_tweets, function(x) x$getText())

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
emotion[is.na(emotion)] = "Confused"

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
  labs(x="Twitter Sentiments", y="number of tweets (2000) Latest") +
  ggtitle("Tweets on #startupirl \n(classification by emotion)"
  )



# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets(2000)") +
  ggtitle("Sentiment Analysis of Tweets about #startupirl")





# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = trendword[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

common <- read.csv("C:/Users/achoudhary/Desktop/ABC/common.csv",header=FALSE)
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












##############################

# create a corpus
mach_corpus = Corpus(VectorSource(trendword))

# create document term matrix applying some transformations
tdm = TermDocumentMatrix(mach_corpus,
                         control = list(removePunctuation = TRUE,
                                        stopwords = c("Startuo", "irl", stopwords("english")),
                                        removeNumbers = TRUE, tolower = TRUE))







# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(2, "Dark2"))

# save the image in png format
png("C:/tmp/startup.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))