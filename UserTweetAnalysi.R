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




#file:///Users/abhishekchoudhary/Downloads/9780387981406-c1%20(2).pdf
#http://blog.ouseful.info/2012/02/17/visualising-twitter-user-timeline-activity-in-r/
#http://blog.ouseful.info/2012/02/06/visualising-activity-round-a-twitter-hashtag-or-search-term-using-r/

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
user = userTimeline("@Ubunta",n=2000)
#Create a dataframe based around the results
user.df <- do.call("rbind", lapply(user, as.data.frame))
View(user.df)
qplot(created, screenName, data = user.df, colour = created)

#We can also generate barplots showing the distribution of tweet count over year:
qplot(created, data = user.df, geom = "bar")
qplot(created, data = user.df, geom = "bar",weight=favoriteCount) #if filter based on retweet



#plot based on favourite counts
ggplot(MyDatesDF, aes(user.df$created,user.df$retweetCount)) + geom_line(aes(group = grp))

#label a tweet with the hour
user.df$hour=sapply(user.df$created, function(x) {p=as.POSIXlt(x);p$hour})
#label a tweet with the month number
user.df$month=sapply(user.df$created, function(x) {p=as.POSIXlt(x);p$mon})
#label a tweet with a number corresponding to the day of the week
user.df$wday=sapply(user.df$created, function(x) {p=as.POSIXlt(x);p$wday})
#label a tweet with a number corresponding to the year
user.df$year=sapply(user.df$created, function(x) {p=as.POSIXlt(x);p$year})

g1 <- ggplot(user.df,aes(x=hour))+geom_bar(aes(y = (..count..)),binwidth=1)+
  ggtitle(paste("Hourly Tweets for @", user.df$screenName[1], sep=" "))
g2 <- ggplot(user.df,aes(x=month))+geom_bar(aes(y = (..count..)),binwidth=1)+ 
  ggtitle(paste("Monthly Tweets for @", user.df$screenName[1], sep=" "))
g3 <- ggplot(user.df,aes(x=wday))+geom_bar(aes(y = (..count..)),binwidth=1)+ 
  ggtitle(paste("Weekday Tweets for @", user.df$screenName[1], sep=" "))
g4 <- ggplot(user.df,aes(x=year))+geom_bar(aes(y = (..count..)),binwidth=1)+ 
  ggtitle(paste("Yearly Tweets for @", user.df$screenName[1], sep=" "))

#multiplot
multiplot(g1, g2, g3,g4, cols=2)


####plot of user reply in descending order
#Order the replyToSN factor levels in the order in which they were first created
tw.dfx=ddply(user.df, .var = "replyToSN", .fun = function(x) {return(subset(x, created %in% min(created),select=c(replyToSN,created)))})
tw.dfxa=arrange(tw.dfx,-desc(created))
user.df$replyToSN=factor(user.df$replyToSN, levels = tw.dfxa$replyToSN)

#and plot the result with Month-wise seperation
ggplot(user.df)+geom_point(aes(x=created,y=replyToSN,col=user.df$month))

