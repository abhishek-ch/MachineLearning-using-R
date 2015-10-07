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


#https://github.com/pablobarbera/Rfacebook
#http://mkmanu.wordpress.com/2014/08/12/data-mining-on-facebook-data-basic-tutorial/
#http://www.r-bloggers.com/mining-facebook-data-most-liked-status-and-friendship-network/
#https://developers.facebook.com/tools/explorer?method=GET&path=me%3Ffields%3Did%2Cname&version=unversioned
#https://developers.facebook.com/apps/322432896805/dashboard/
#https://github.com/pablobarbera/Rdataviz/tree/master/code

library(Rfacebook)
library(Rook)
library(RCurl)
library(rjson)
library(ggplot2)

access_token = "facebook access token inside graph api"
myFB = getUsers("me",token = access_token)
myFriends = getFriends(access_token, simplify = FALSE)


totalrows = nrow(myFriends)



relationstatus <- function(n){
  mylist <- list()
  j = 1
  for(x in seq(1,n,100)) {
    y = x+99
      if(y > n){
        y = n
      }
    myFriends_info <- getUsers(myFriends$id[x:y], token=access_token, private_info=TRUE)
    sEOG <- paste("", j, sep="")
    mylist[[sEOG]] <-myFriends_info
    j = j+ 1
  } 
  return(mylist)
}


rslist <- relationstatus(totalrows)
#merge all list of data frame into a single data frame-
mainDF <- do.call("rbind", rslist)
View(mainDF)

#plot the bar chart of all basic details about my account
g1 <- qplot(relationship_status, data=mainDF, geom="bar", fill=relationship_status)+xlab("Relationship Status")+ggtitle("Relationship Frequency")
g2 <- qplot(gender, data=mainDF, geom="bar", fill=gender)
multiplot(g1,g2,cols=1)
##Use table to count elements
location_sum = as.data.frame(table(mainDF$location))

#plot the data based on number of person stay in each location
mainDF$type <- factor(mainDF$location, 
                  levels = c('Bangalore, India', 'Calcutta, India', 'Chennai, Tamil Nadu', 
                             'Hyderabad', 'Mumbai, Maharashtra, India','New Delhi, India','Pune, Maharashtra',
                             'Raipur'))
g3 <- qplot(type, data=mainDF, geom="bar", fill=type)+xlab("Cities having highest no. of ...")+
  ggtitle("Frequency of Friends in each Cities Around the Globe ")
multiplot(g1,g2,g3,col=2)

require(v <- ggplot(location_sum, aes(x=location_sum[location_sum$Freq>15,],)) + 
  geom_bar(stat="identity")

#####################################ANALYTICS###############################################  
 #further analytics
 #http://stackoverflow.com/questions/11852408/frequency-table-with-several-variables-in-r
###############################################  ###############################################  

 #http://blog.revolutionanalytics.com/2013/11/how-to-analyze-you-facebook-friends-network-with-r.html
 library(igraph)
my_network <- getNetwork(access_token, format="adj.matrix")
singletons <- rowSums(my_network)==0 # friends who are friends with me alone
my_graph <- graph.adjacency(my_network[!singletons,!singletons])
layout <- layout.drl(my_graph,options=list(simmer.attraction=0))
layout.new <- layout.fruchterman.reingold(my_graph,params=list(niter=10,start=layout,weights=E(my_graph)$weight,maxdelta=1))
plot(my_graph, vertex.size=2, 
     vertex.label=NA, 
     vertex.label.cex=0.5,
     edge.arrow.size=0, edge.curved=TRUE,layout=layout.new)
mtext("layout.fruchterman.reingold, area = vcount^2", side=1)
dev.off()







###################################FACEBOOK LIKES###################################################
require(RCurl)
require(rjson)
library(Rfacebook)
# Facebook json function copied from original (Romain Francois) post
facebook <-  function( path = "me", access_token, options){
  if( !missing(options) ){
    options <- sprintf( "?%s", paste( names(options), "=", unlist(options), collapse = "&", sep = "" ) )
  } else {
    options <- ""
  }
  writeLines(paste("ABHISHE"))
  print(sprintf( "https://graph.facebook.com/%s%s&access_token=%s", path, options, access_token ))
  data <- getURL( sprintf( "https://graph.facebook.com/%s%s&access_token=%s", path, options, access_token ) )
  fromJSON( data )
}

access_token=""
fbactivities <- facebook(path="me/friends" , access_token=access_token)




