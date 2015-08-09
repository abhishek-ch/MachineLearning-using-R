require(RSQLite)
require(dplyr)
library(wordcloud)
library(tm)
library(RColorBrewer)

# Set up a repeatbale function for making an unlimited number of subreddit wordclouds

makewordcloud <- function(subreddit) {
  
  sub <- subreddit
  max_words <- 75
  
  db <- src_sqlite('../input/database.sqlite', create = F)
  
  db_subset <- db %>% 
    tbl('May2015') %>% 
    filter(subreddit == sub)
  
  db_subset <- data.frame(db_subset)
  
  corp <- Corpus(VectorSource(db_subset$body))
  corp <- tm_map(corp, tolower)
  corp <- tm_map(corp, PlainTextDocument)
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, removeWords, stopwords("english"))
  
  # Removing Common Uninteresting Words Manually
  corp <- tm_map(corp, removeWords, c("like", "dont", "people", "deleted", "think", "cant", "back", "one", "will", "pretty", "better", "need", "got", "time", "thats", "always", "even", "youre", "look", "never", "just", "way", "see", "though", "thing", "still", "new", "best", "sure", "ever", "going", "make", "work", "really", "something", "things", "good", "get", "can", "maybe", "great", "different", "actually", "isnt", "doesnt", "use", "lot", "around", "take", "now", "fuck", "real", "two", "theyre", "mean", "someone", "years", "since", "fucking", "say", "made", "know", "find", "little", "point", "yeah", "said", "day", "getting", "looks", "many", "theres", "yes", "long", "old", "right", "shit", "used", "every", "bad", "first", "want", "can", "man", "probably", "everyone", "much", "buy", "hey", "thanks", "means", "open", "important", "top", "ive", "help", "less", "quite", "least", "also", "send", "tried", "bit", "usually", "havent", "decide", "soon", "youll", "usual"))
  
  
  dtm <- DocumentTermMatrix(corp,
                            control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE), stopwords = TRUE))
  dtm_sparse <- removeSparseTerms(dtm, 0.995)
  post_words <- as.data.frame(as.matrix(dtm_sparse))
  
  total_words <- data.frame(words = colnames(post_words),
                            counts = colSums(post_words))
  
  
  #Set up output so it is in a named png
  
  png(paste(subreddit,".png", sep=""))
  
  wordcloud(words = total_words$words,
            freq=total_words$counts, 
            max.words = max_words,
            color = brewer.pal(8,"Dark2"))
  
  dev.off()
  
  #print(total_words$words)
  print(subreddit)
  
}

# List of Wordclouds to make here


makewordcloud("PlantsVSZombies")
makewordcloud("needforspeed")
makewordcloud("DevilMayCry")
makewordcloud("JC2")
makewordcloud("myst")
makewordcloud("Spore")
makewordcloud("killzone")
makewordcloud("WC3")
makewordcloud("counter_strike")
makewordcloud("Doom")
makewordcloud("Rockband")
makewordcloud("ptcgo")
makewordcloud("AgeofMythology")
makewordcloud("AceAttorney")
makewordcloud("SaintsRow")
makewordcloud("GearsOfWar")
makewordcloud("residentevil")
makewordcloud("kotor")
makewordcloud("silenthill")
makewordcloud("mariokart")
makewordcloud("StreetFighter")
makewordcloud("neopets")
makewordcloud("Madden")
makewordcloud("splatoon")
makewordcloud("TeraOnline")
makewordcloud("Portal")
makewordcloud("dwarffortress")
makewordcloud("Bioshock")
makewordcloud("halo")
makewordcloud("civ")
makewordcloud("zelda")
makewordcloud("starcraft")
makewordcloud("DotA2")
makewordcloud("hearthstone")