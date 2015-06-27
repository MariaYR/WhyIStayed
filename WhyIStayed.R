#Janaury 20, 2015
#WhyIstayed code 

setwd ("~/Dropbox/#WhyIStayed")
load("~/Dropbox/#WhyIStayed/#WhyIStayed.RData")

#Create new Twitter APP
library(twitteR)
library(streamR)

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL<- "https://api.twitter.com/oauth/access_token"
authURL<- "https://api.twitter.com/oauth/authorize"
consumerKey <- "mQyKfusdTW0qJkwcJHDsYPRVj"
consumerSecret<- "XOk0r1tp7fYR9ibKIkvfmI2x75MW7uIBotw26R0quzw4hSutv4"

twitCred2<- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret,requestURL=reqURL,accessURL=accessURL,authURL=authURL)
OAuthFactory$new()
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
twitCred2$handshake(cainfo="cacert.pem")
registerTwitterOAuth(twitCred2)

## now you can save oauth token for use in future sessions with twitteR or streamR
save(twitCred2, file="my_oauth2.Rdata")


#source('functions_by_pablobarbera_update.r')
getCommonHashtags <- function(text, n=20){
  
  hashtags <- regmatches(text,gregexpr("#(\\d|\\w)+",text))
  
  hashtags <- unlist(hashtags)
  
  tab <- table(hashtags)
  
  return(head(sort(tab, dec=TRUE), n=n))
  
}

## function to obatin the timeline of a user
getTimeline <- function(file.name, screen_name=NULL, n=1000, oauth){
  
  require(jsonlite); require(httr)
  
  ## url to call
  url <- "https://api.twitter.com/1.1/statuses/user_timeline.json"
  
  ## first API call
  params <- list(screen_name = screen_name, count=200, trim_user="false")
  query <- lapply(params, function(x) URLencode(as.character(x)))
  
  # preparing OAuth token for httr
  options("httr_oauth_cache"=FALSE)
  app <- httr::oauth_app("twitter", key = twitCred$consumerKey, 
                         secret = twitCred$consumerSecret)
  sig <- httr::sign_oauth1.0(app, token=twitCred$oauthKey, 
                             token_secret=twitCred$oauthSecret)
  
  url.data <- httr::GET(url, query=query, config(token=sig[["token"]]))
  
  ## trying to parse JSON data
  json.data <- httr::content(url.data)
  if (length(json.data$error)!=0){
    cat(url.data)
    stop("error parsing tweets!")
  }
  ## writing to disk
  conn <- file(file.name, "w")
  invisible(lapply(json.data, function(x) writeLines(jsonlite::toJSON(x, null="null"), con=conn)))
  close(conn)
  ## max_id
  tweets <- length(json.data)
  max_id <- json.data[[tweets]]$id_str
  max_id_old <- "none"
  cat(tweets, "tweets.\n")
  
  while (tweets < n & max_id != max_id_old){
    max_id_old <- max_id
    params <- list(screen_name = screen_name, count=200, max_id=max_id,
                   trim_user="false")
    query <- lapply(params, function(x) URLencode(as.character(x)))
    url.data <- httr::GET(url, query=query, config(token=sig[["token"]]))
    ## trying to parse JSON data
    json.data <- httr::content(url.data)
    if (length(json.data$error)!=0){
      cat(url.data)
      stop("error! Last cursor: ", cursor)
    }
    ## writing to disk
    conn <- file(file.name, "a")
    invisible(lapply(json.data, function(x) writeLines(jsonlite::toJSON(x, null="null"), con=conn)))
    close(conn)
    ## max_id
    tweets <- tweets + length(json.data)
    max_id <- json.data[[length(json.data)]]$id_str
    cat(tweets, "tweets.\n")
  }
}


load("my_oauth2.Rdata")
registerTwitterOAuth(twitCred2)

install.packages("jsonlite")
install.packages("httr")
library("jsonlite")
library("httr")

#collect bevtgooden's tweets
getTimeline(file.name="data/BEV_tweets.json", screen_name="bevtgooden", n=3200, oauth=twitCred2) 
#1199 tweets collected 3/8/15
#1399 tweets collects 3/8/15 afer running a second time to try to up the number of WhyIStayed Tweets
#3235 tweets collected 3/9/15 after updating getTimeline Function

BEV_tweets <- parseTweets("data/BEV_tweets.json")

names(BEV_tweets)
head(BEV_tweets)

##Which of these tweets has received more retweets?
max(BEV_tweets$retweet_count) ##how many times was it retweeted? 4291

##what sequence does the tweet fall into in the data?
which(BEV_tweets$retweet_count == max(BEV_tweets$retweet_count)) ##868

#which(BEV_tweets$text == "WhyIStayed"), didn't work 
#test<-BEV_tweets$retweet_count = 4156, just converts all values of retweet_count to 4156

##what was the text of the tweet?
BEV_tweets$text[which(BEV_tweets$retweet_count == max(BEV_tweets$retweet_count))]
#[1] "RT @SportsNation: A rare pic of Marshawn Lynch's interview from today: http://t.co/tKCIfdNt4c"

##Look at some of the other high retweet tweets
table(BEV_tweets$retweet_count)

#Top 50 hastags used 
getCommonHashtags(BEV_tweets$text, n=50)

##WhyIstayed = 103, #domesticviolence = 19, #DV = 7, #WUWhyIStayed = 8, #NOMORE = 6 (DV org), 
#DomestiveViolence = 6, #dv=4, #NOMOREweek = 4(DV Org), #whyistayed = 5, 

#subset tweets to only include the hastags above, minus NOMORE
#first create an index object from grep
index1<-grep(c("domesticviolence"), BEV_tweets$text)
index2<-grep("DV", BEV_tweets$text)
index3<-grep("WhyIstayed", BEV_tweets$text)
index4<-grep("WUWhyIStayed", BEV_tweets$text)
index5<-grep("DomesticViolence", BEV_tweets$text)
index6<-grep("dv", BEV_tweets$text)
index7<-grep("#whyistayed", BEV_tweets$text)

#create new index object omitting repeats
unique_indexes<-unique(c(index1,index2,index3,index4, index5, index6, index7))

#create new subset
BEV_Final <- BEV_tweets[c(unique_indexes),]
write.csv(BEV_Final, file = "BEV_final.csv")

#get range of dates covered
range (BEV_Final$created_at)

## compute a simple "sentiment" score for BEV_tweets 
install.packages ("plyr")
install.packages ("stringr")

library(plyr)
library(stringr)

source("sentiment.r")

# import positive and negative words from the dictionary included in the lab file 
pos = readLines("opinion_lexicon/positive_words.txt")
neg = readLines("opinion_lexicon/negative_words.txt")

#compute simple sentiment score for each tweet: number of positive words
## minus number of negative words 
BEV_Scores <- score.sentiment(BEV_Final$text,pos, neg)$score
mean(BEV_Scores) #0.3693694
median(BEV_Scores)
sd(BEV_Scores) # 1.367997

#create a graph to show distriubtion of sentiment scores for all tweets 
# Kernel Density Plot, with line for the mean
d <- density(BEV_Scores)  

plot(d, main="Distribution of @BevTGooden Tweets' Sentiment Scores (N= 111)", 
     xlab="Score", ylab="Density",
     abline(v=mean(BEV_Scores), col="red")) # plots the results


#Save the data
save.image("~/Dropbox/#WhyIStayed/#WhyIStayed.RData")

#although I want to do a manual content analysis of the tweets, I also want to try an unsupervised
#machine learning ACA, just to see if the results are interpretable

library(RTextTools)

#load data
excerpts<- read.csv("~/Dropbox/#WhyIStayed/BEV_Final.csv",header = (TRUE))

#create a term document matrix that represents word frequencies in each excerpt
matrix <- create_matrix(cbind(excerpts["text"]), language="english",
                        removeNumbers=TRUE, stemWords=TRUE, removePunctuation =TRUE,
                        removeStopwords=TRUE, stripWhitespace=TRUE)

#check out the matrix
matrix


#try to do LDA
install.packages ("topicmodels")
library ("topicmodels")

k <- length(excerpts)
lda <- LDA(matrix, k)
terms(lda)
topics(lda)

#
#terms(lda)
#Topic 1           Topic 2           Topic 3           Topic 4 
#"hashtag"          "domest"      "bevtgooden"          "factor" 
#Topic 5           Topic 6           Topic 7           Topic 8 
#"lotzlisi"           "great" "hopelineverizon"            "piec" 
#Topic 9          Topic 10          Topic 11          Topic 12 
#"advic"           "advoc"      "bevtgooden"            "abus" 
#Topic 13          Topic 14          Topic 15          Topic 16 
"violenc"            "abus"            "abus"            "abus" 
Topic 17          Topic 18          Topic 19          Topic 20 
"tackledv"        "thedress"    "domesticviol"           "thank" 
Topic 21          Topic 22          Topic 23          Topic 24 
"job"             "amp"        "survivor"            "aclu" 
Topic 25          Topic 26          Topic 27          Topic 28 
"happi"           "dvsux"     "cpedvcoalit"             "amp" 
Topic 29          Topic 30          Topic 31          Topic 32 
"one"        "whyistay"          "domest"   "annetheriault" 
Topic 33          Topic 34          Topic 35          Topic 36 
"domesticviol"    "domesticviol" "advocacyprogram"             "amp" 
Topic 37          Topic 38          Topic 39          Topic 40 
"bevtgooden"        "advantag"           "advic"        "whyistay" 
Topic 41          Topic 42          Topic 43 
"dvnjr"            "awar"            "ndv

