#http://www.omdbapi.com/
#http://www.r-bloggers.com/top-250-movies-at-imdb/
library
library(RJSONIO)
tt <- htmlParse('http://www.omdbapi.com/?t=The+dark+knight&y=&plot=short&r=xml&tomatoes=true')
document <- fromJSON('http://www.omdbapi.com/?t=The+dark+knight&y=&plot=short&r=json&tomatoes=true')
View(document)
