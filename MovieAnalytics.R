#http://www.omdbapi.com/
#http://www.r-bloggers.com/top-250-movies-at-imdb/
# Install the Rstem and sentiment packages if not installed.
#https://www.linkedin.com/pulse/article/20141116012757-34768479-9k-tweets-re-12-financial-groups-polarity-and-emotion
#https://gist.github.com/Inpirical-Coder/aa8a93566f9e7404e4fe
#http://docs.ggplot2.org/0.9.3.1/geom_bar.html


# Define the dependency packages we need.
required.packs = c("twitteR",
                   "sentiment",          # Sentiment analysis.
                   "tm",                 # Text mining.
                   "plyr",               # Splitting, plotting, combining data.
                   "ggplot2",            # Plotting.
                   "wordcloud",          # Create wordclouds.
                   "data.table",         # Data tables.
                   "RColorBrewer",      # Palettes for visualisation.      
                   "dplyr"                   
)


# Install the required packages if missing, then load them.
sapply(required.packs, function(pack) {
  if(!(pack %in% installed.packages())) {install.packages(pack)}
  require(pack, character.only=TRUE)
})

library(RJSONIO)
source("/Users/abhishekchoudhary/MachineLearning/Graphmultiplot.R")
#tt <- htmlParse('http://www.omdbapi.com/?t=The+dark+knight&y=&plot=short&r=xml&tomatoes=true')
#document <- fromJSON('http://www.omdbapi.com/?t=The+dark+knight&y=&plot=short&r=json&tomatoes=true')
#View(document)
#http://www.omdbapi.com/?t=Romance+of+Astrea+and+Celadon&y=&r=xml&tomatoes=true



value <- read.csv("/Users/abhishekchoudhary/Google Drive/BIG/Python/name_movie.csv")
View(value)
View(unique(value))

#Filter With Any Movie
filter(value,MOVIE =="Dil Chahta Hai")

movie_table = as.data.frame(table(value$MOVIE))
updated <- movie_table[movie_table$Freq>25,]

#Plot the Most Popular Movies among Friends
gmovie <- ggplot(updated, aes(x = factor(Var1), y = Freq,fill=Var1)) +
  geom_bar(stat = "identity") +
labs(x="Most Popular Movies Among FB friends", y="Movie Occurances")+
ggtitle(" Popular Movies Amongs Friends in facebook")


#####Top Movie Likers
name_table = as.data.frame(table(value$NAME))
updated_name <- name_table[name_table$Freq,]
View(updated_name)
#Plot the Most Popular Movies among Friends
gname <- ggplot(updated_name, aes(x = factor(Var1), y = Freq,fill=Var1)) +
  geom_bar(stat = "identity") +
  labs(x="Movies Like by fb Friends", y="Movie Occurances")+
  ggtitle(" Popular Movies Amongs Friends in facebook")

auto <- c(name_table$Var1,name_table$Freq)
multiplot(gmovie,gname)

##Populate the graph for range , the number of people falls in a range or
#count of movie likes within range
updated_name$group <- cut(updated_name$Freq, breaks = seq(0, max(updated_name$Freq) + 4, by = 3), include.lowest = T)

ggplot(updated_name, aes(x = group,fill=group)) + geom_bar()
