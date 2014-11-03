#https://sites.google.com/site/miningtwitter/questions/talking-about/wordclouds/wordcloud1
#http://www.rdatamining.com/examples/text-mining
#https://sites.google.com/site/miningtwitter/basics/getting-data/by-twitter
#http://www.r-bloggers.com/r-text-mining-on-twitter-prayformh370-malaysia-airlines/
#twitter authentication - http://thinktostart.com/twitter-authentification-with-r/
#my twitter developer App
#https://apps.twitter.com/app/6536937/keys

library(RCurl)
library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(devtools)
library(stringr)

# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))


reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

apiKey <-  "MIgAEnO0XHTPKdMv3qiGKr6nu"
apiSecret <- "CMYO2quM7fUzcVuvx8JjALiKjC9cnpXeJFqQLtv2pnECJCCZKz"
access_token <- "69009666-XkI1bcxXtE4qXfOtbRYCgkiJJvpCfsmS0fq4OSq9d"
access_token_secret <- "w89WtxJDAwakPToMqoFtpQYJIfht6YS3a8136hpcyW7eG"

setup_twitter_oauth(apiKey,apiSecret,access_token,access_token_secret)




mach_tweets = searchTwitter("#Gautam", n=1000)
#extract the text from tweets in a vector as R easily understands that
#laterI will store the same in file so that I could do the analysis in Java/Python  :) 
#as its easy over there ..hopefully
mach_text = sapply(mach_tweets, function(x) x$getText())
#to avoid the error http://stackoverflow.com/questions/9637278/r-tm-package-invalid-input-in-utf8towcs
mach_text=str_replace_all(mach_text,"[^[:graph:]]", " ") 

#convTweets <- iconv(mach_text, to = "utf-8")
# create a corpus
mach_corpus = Corpus(VectorSource(mach_text))
tm_map(mach_corpus, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
tm_map(mach_corpus, function(x) iconv(enc2utf8(x), sub = "byte"))

options(mc.cores=1)
# create document term matrix applying some transformations
tdm = TermDocumentMatrix(mach_corpus,
                         control = list(removePunctuation = TRUE,
                                        stopwords = c("karishma", "gautam", stopwords("english")),
                                        removeNumbers = TRUE, tolower = TRUE))


# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)



# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# save the image in png format
png("BigBoss2.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()


