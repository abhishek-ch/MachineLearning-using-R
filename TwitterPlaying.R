#http://www.thisisthegreenroom.com/2009/choropleths-in-r/

# Define the dependency packages we need.
required.packs = c("twitteR",
                   "maps",          # Sentiment analysis.
                   "ggplot2",                 # Text mining.
                   "dplyr"
)

# Install the required packages if missing, then load them.
sapply(required.packs, function(pack) {
  if(!(pack %in% installed.packages())) {install.packages(pack)}
  require(pack, character.only=TRUE)
})