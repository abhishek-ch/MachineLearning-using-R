# go to 'https://developers.facebook.com/tools/explorer' to get your access token
access_token <- "CAACEdEose0cBAJKozHiNcJs3iYTCkxx7zltNuWxObsQZBLyCrDIhybYqJVJcwmYn0OmM77MGgmdJepI2ZCtK7Jtz9DTRqqmw7c0KGGk17NGFrcW0MvXBtyWWPVhGq4sBINykZBZBh7DwmoC9iPuHTSJyCWfAjXQYOzUI4nnNu7FV6N9JQ2pwVnxjGcI3aQ1ffZB2CZCZAqEMNYI0kb6A1y9"

require(RCurl)
require(rjson)

facebook <- function( path = "me", access_token, options){
  if( !missing(options) ){
    options <- sprintf( "?%s", paste( names(options), "=", unlist(options), collapse = "&", sep = "" ) )
  } else {
    options <- ""
  }
  if( regexpr("access_token=", path) <= 0 ){
    data <- getURL( sprintf( "https://graph.facebook.com/%s%s?limit=2&access_token=%s&format=json", path, options, access_token ) )
  } else {
    data <- getURL( sprintf(path) )
    
  }
  fromJSON( data )
}

### MY FACEBOOK POSTS

myposts <- list()
i <- 0
next.path <- "me/feed"
myposts[[1]] <- facebook(path=next.path , access_token=access_token)
# download all my posts
while(length(next.path) != 0) {
  i<-i+1
  myposts[[i]] <- facebook(path=next.path , access_token=access_token)
  next.path <- myposts[[i]]$paging$'next'
}
myposts[[i]] <- NULL

# parse the list, extract number of likes and the corresponding text (status)
parse.master <- function(x, f)
  sapply(x$data, f)
parse.likes <- function(x) if(!is.null(x$likes$count)) x$likes$count else 0
mylikes <- unlist(sapply(myposts, parse.master, f=parse.likes))
parse.messages <- function(x) if(!is.null(x$message)) x$message else NA
mymessages <- unlist(sapply(myposts, parse.master, f=parse.messages))

# and the most liked status is...
mymessages[which.max(mylikes)]

### TED FACEBOOK PAGE
# http://www.facebook.com/TED
# TED's Facebook ID 29092950651 can be found on http://graph.facebook.com/TED

ted <- list()
i<-0
next.path <- "29092950651/posts"

# download all TED posts
while(length(next.path)!=0) {
  i<-i+1
  ted[[i]] <- facebook( path=next.path , access_token=access_token)
  next.path <- sub("https://graph.facebook.com/","",ted[[i]]$paging$'next')
}
ted[[i]] <- NULL

# parse just video links posted by TED
parse.count.ted <- function(x) 
  if (x$type=="link" & x$from$id=="29092950651") x$likes$count else NA
parse.link.ted <- function(x) 
  if (x$type=="link" & x$from$id=="29092950651") x$link else NA
ted.counts <- unlist(sapply(ted, parse.master, f=parse.count.ted))
ted.links <- unlist(sapply(ted, parse.master, f=parse.link.ted))

# see three most popular talks
ted.links[order(ted.counts,decreasing=TRUE)][1:3]