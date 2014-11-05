#https://sites.google.com/site/miningtwitter/questions/sentiment
#https://sites.google.com/site/miningtwitter/questions/sentiment/analysis
library(twitteR)
library(plyr)
library(stringr)
library(ggplot2)


#The sequence of column in CSV does matter the smoot_spline method to work properly
if(Sys.info()["user"] == 'achoudhary'){
  path = "D:/Work/RWorkSpace/Git/"
}else{
  path = "/Users/abhishekchoudhary/MachineLearning/"
}

# function score.sentiment
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   # remove html links
                   sentence = gsub("http\\w+", "", sentence)
                   # remove unnecessary spaces
                   sentence = gsub("[ \t]{2,}", "", sentence)
                   sentence = gsub("^\\s+|\\s+$", "", sentence)
                   
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
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   
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


# import positive and negative words
posText = paste(path,"positive_words.txt", sep = "")
negText = paste(path,"negative_words.txt", sep = "")
# import positive and negative words
pos = readLines(posText)
neg = readLines(negText)


#Twitter API details

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

apiKey <-  "************************************"
apiSecret <- "************************************"
access_token <- "************************************"
access_token_secret <- "************************************"

setup_twitter_oauth(apiKey,apiSecret,access_token,access_token_secret)


#tweets for phone

iphone = searchTwitter("iphone", n=2000,lang="en")
nexus6 = searchTwitter("nexus6", n=2000,lang="en")
samsung = searchTwitter("Samsung Galaxy", n=2000,lang="en")

#total tweets of each item
totaltweets = c(length(iphone),length(nexus6),length(samsung))

#join all tweets
allcontents = c(iphone,nexus6,samsung)
allcontents = sapply(allcontents, function(x) x$getText())
#to avoid the error http://stackoverflow.com/questions/9637278/r-tm-package-invalid-input-in-utf8towcs
allcontents=str_replace_all(allcontents,"[^[:graph:]]", " ")
all_corpus = Corpus(VectorSource(allcontents))
# apply function score.sentiment
scores = score.sentiment(allcontents, pos, neg, .progress='text')

# add variables to data frame
scores$phones = factor(rep(c("iPhone", "Nexus6", "Samsung"), totaltweets))
scores$very.pos = as.numeric(scores$score >= 2)
scores$very.neg = as.numeric(scores$score <= -2)

# how many very positives and very negatives
numpos = sum(scores$very.pos)
numneg = sum(scores$very.neg)

# global score
global_score = round( 100 * numpos / (numpos + numneg) )


#http://stackoverflow.com/questions/15651084/ggplot2-mapping-variable-to-y-and-using-stat-bin?lq=1
# barplot of average score
meanscore = tapply(scores$score, scores$phones, mean)
df = data.frame(phones=names(meanscore), meanscore=meanscore)
df$drinks <- reorder(df$phones, df$meanscore)

g1<- ggplot(df, aes(y=meanscore)) +
  geom_bar(data=df, aes(x=phones, fill=phones),stat = "identity") +
  scale_fill_manual(values=cols[order(df$meanscore)]) +
  ggtitle("Average Sentiment Score")




# barplot of average very positive
phone_pos = ddply(scores, .(phones), summarise, mean_pos=mean(very.pos))
phone_pos$phones <- reorder(phone_pos$phones, phone_pos$mean_pos)

g2<- ggplot(phone_pos, aes(y=mean_pos)) +
  geom_bar(data=phone_pos, aes(x=phones, fill=phones),stat = "identity") +
  scale_fill_manual(values=cols[order(phone_pos$mean_pos)]) +
  ggtitle("Average Very Positive Sentiment Score")





# barplot of average very negative
phone_neg = ddply(scores, .(phones), summarise, mean_neg=mean(very.neg))
phone_neg$phones <- reorder(phone_neg$phones, phone_neg$mean_neg)

g3<- ggplot(phone_neg, aes(y=mean_neg)) +
  geom_bar(data=phone_neg, aes(x=phones, fill=phones),stat = "identity") +
  scale_fill_manual(values=cols[order(phone_neg$mean_neg)]) +
  ggtitle("Average Very Negative Sentiment Score")


multiplot(g1, g2, g3, cols=2)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}