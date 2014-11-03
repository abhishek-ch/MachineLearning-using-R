#https://sites.google.com/site/miningtwitter/questions/talking-about/wordclouds/wordcloud1
#http://www.rdatamining.com/examples/text-mining
#https://sites.google.com/site/miningtwitter/basics/getting-data/by-twitter
#http://www.r-bloggers.com/r-text-mining-on-twitter-prayformh370-malaysia-airlines/

#my twitter developer App
#https://apps.twitter.com/app/6536937/keys

library(RCurl)
# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

library(twitteR)
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
apiKey <-  "MIgAEnO0XHTPKdMv3qiGKr6nu"
apiSecret <- "CMYO2quM7fUzcVuvx8JjALiKjC9cnpXeJFqQLtv2pnECJCCZKz"

twitCred <- OAuthFactory$new(
  consumerKey = apiKey, 
  consumerSecret = apiSecret,
  requestURL = reqURL,
  accessURL = accessURL, 
  authURL = authURL
)

twitCred$handshake(
  cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")
)

registerTwitterOAuth(twitCred)