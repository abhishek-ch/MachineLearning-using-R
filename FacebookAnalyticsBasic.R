#https://github.com/pablobarbera/Rfacebook
#http://mkmanu.wordpress.com/2014/08/12/data-mining-on-facebook-data-basic-tutorial/
#http://www.r-bloggers.com/mining-facebook-data-most-liked-status-and-friendship-network/
#https://developers.facebook.com/tools/explorer?method=GET&path=me%3Ffields%3Did%2Cname&version=unversioned
#https://developers.facebook.com/apps/322432896805/dashboard/

library(Rfacebook)
library(Rook)
library(RCurl)
library(rjson)

access_token = "CAACEdEose0cBAFzuf79EpBRtXAYEOsmjXQD02KJlYg8F0nrnZCNgqufJeoN5C93roNJJ9ieFhFo5FnfiZCwbipNNrwvMEPx8EYuiyoIbGksXKGp6xZB6imqUyrLMTPT6eceNI2OKxiUSf6XODBVz0RiDCtZAhmIFAjxZC0vmFE1SpMbi027achD0q7C4f1yeqmtwOkZAdDO02SkZCTgCUIn"
myFB = getUsers("me",token = access_token)
myFriends = getFriends(access_token, simplify = FALSE)


totalrows = nrow(myFriends)



relationstatus <- function(n){
  mylist <- list()
  for(x in seq(1,n,100)) {
    y = x+99
      if(y > n){
        y = n
      }
    myFriends_info <- getUsers(myFriends$id[x:y], token=access_token, private_info=TRUE)
    append(mylist,myFriends_info)
    head(myFriends_info)
  } 
  return(mylist)
}


rslist <- relationstatus(totalrows)
for(i in 1:length(rslist))
{
  print(table(rslist[i]$relationship_status))
}
