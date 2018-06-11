#STM attempt without shiny app 

#1.1.2018

setwd("~/Dropbox/Publications/TopicModelingVSHumans/stm")

load("~/Dropbox/Publications/TopicModelingVSHumans/stm/stm_paper.RData")

#application following STM vignette 

library(stm)
library(streamR)
library (tidyverse)
library(stm)
library(tidytext)

na_count <-sapply(blm_tweets, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

#remove columns that have na's 
#na_count
#X                                0
#text                             0
#retweet_count                    0
#favorited                        0
#truncated                        0
#id_str                           0
#in_reply_to_screen_name      79536
#source                           0
#retweeted                        0
#created_at                       0
#in_reply_to_status_id_str    80475
#in_reply_to_user_id_str      79536
#lang                             0
#listed_count                     4
#verified                         0
#location                      7496
#user_id_str                      0
#description                   8612
#geo_enabled                      0
#user_created_at                  0
#statuses_count                   0
#followers_count                  0
#favourites_count                 0
#protected                        0
#user_url                     54801
#name                             0
#time_zone                    25566
#user_lang                        0
#utc_offset                   25566
#friends_count                    0
#screen_name                      0

#Ingest

tweets<- read.csv("forSTMfinal.csv", header = TRUE)
head(tweets)

#remove special characters
#reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"


#tidy_tweets <- tweets %>%
#  filter(!str_detect(Text, "^RT")) %>%
#  mutate(text = str_replace_all(Text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
#  unnest_tokens(word, Text, token = "regex", pattern = reg_words) %>%
#  filter(!word %in% stop_words$word,
#         str_detect(word, "[a-z]")) 

#remove emoji characters
#gsub("[^\x01-\x7F]", "", tweets)
tweets$Text <- sapply(tweets$Text,function(row) iconv(row, "latin1", "ASCII", sub=""))

processed <- textProcessor(tweets$Text, metadata = tweets)
#builds a text corpus that converts to lowercase, removes stopwords, numbers, 
#stems words then outputs



#Prepare

plotRemoved(processed$documents, lower.thresh = seq(1,200, by =100))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 15)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
#Removing 2785 of 4372 terms (2785 of 28206 tokens) due to frequency 
#Your corpus now has 3055 documents, 1587 terms and 25421 tokens.

docs<- out$documents
vocab<- out$vocab
meta <- out$meta

#inspect to make sure preprocessing went ok 
head(docs)#this tells you how many words are in what position 
head(vocab)
head(meta)

#Estimate 

#this data set is just the text, so no prevalence to specify 

whyistayedFit <- stm(documents = out$documents, vocab = out$vocab, K= 20, 
                      data = out$meta, init.type = "Spectral")

#to minimize iterations  specificy max.em.its = 75(or some other number of iterations)

#fit another model using spectral and no K specifc to 
#use Lee & Mimno (2014) k selection algorithm

#must install Rtsne, rsvd, and geometry packages in order to do this
library (Rtsne)
library(rsvd)
library(geometry)

whyistayedFit2 <- stm(documents = out$documents, vocab = out$vocab, K= 0, 
                      data = out$meta, init.type = "Spectral")
#algorithm chosen topics = 62

#Evaluate
#go to 100 to see if 90+ works for topics, since manual codes = 97
storage <- searchK(out$documents, out$vocab, K = c(20,30,40,50,60,70,80,90,100), data = meta) 
#hand codes = 97 codes, best fit = 90 topics per graph 

plot(storage)

#lowest held out likelihood = 70 topics
#lowest residual = 60 topics
#highest semantic coherence = 70 topics
#highest lower bound = 60 topics

whyistayedFit3 <- stm(documents = out$documents, vocab = out$vocab, K= 75, 
                      data = out$meta, init.type = "Spectral")

#Understand 
#STM lets us do a couple of things: 
#1: display words associated with topics 

#labelTopics (whyistayedFit, c(3, 7, 20))

#labelTopics (whyistayedFit) #,c(xxx)
#labelTopics (whyistayedFit2)
labelTopics (whyistayedFit3)

#the cbind here is to select the topics I want to look at.
#I can select all the topics therefore 

#convert to dataframe and export to csv 
k75_codes <- make.dt(whyistayedFit3)
write.csv(k75_codes, "topicModelCodesK75_6_10_18.csv")


#1a. Display documents highly associated with particular topics
#example below is of topics 3 and 20 
thoughts3 <- findThoughts(whyistayedFit3, texts=tweets$Text, topics=3, n=2)
thoughts20 <- findThoughts(whyistayedFit3, texts=tweets$Text, topics=20, n=2)
par(mfrow = c(1,2), mar=c(.5, .5, 1, .5))
plotQuote(thoughts3$docs[[1]], main = "Topic 3")
plotQuote(thoughts20$docs[[1]], main = "Topic 20")

#2. Estimate the relationship between metadata and topics/topical content 
estimateEffect()

#3. Calculate topic correlations 
corStayed20 <- topicCorr(whyistayedFit, method = "simple", cutoff = 0.01, verbose = TRUE)
corStayed62 <- topicCorr(whyistayedFit2, method = "simple", cutoff = 0.01, verbose = TRUE)
corStayed75 <- topicCorr(whyistayedFit3, method = "simple", cutoff = 0.01, verbose = TRUE)

#output
#posadj	
#K by K adjacency matrix where an edge represents positive correlation selected by the model.
#poscor	
#K by K correlation matrix. It takes values of zero where the correlation is either negative or the edge is unselected by the model selection procedure.
#cor	
#K by K correlation matrix element-wise multiplied by the adjacency matrix. Note that this will contain significant negative correlations as well as positive correlations.

#must install igraph before graphing correlations 
library(igraph)
plot (corStayed20,  cex = 2.0)
plot (corStayed62, cex = 2.0)
par(mar=c(0,0,1,0))
plot (corStayed75, cex = .05, 
      main = "Topic Correlations (k = 75)")

#plot (corStayed60, main = "Topic Correlations, Model 2", cex = 2.0)

#Visualize 
#reset par mfrow
par(mfrow=c(1,1))

#expected proportion of corpus that each topic belongs to
#problems plotting? rstudio bug fix = dev.off()

plot.STM(whyistayedFit,type="summary", xlim=c(0, .3))
plot.STM(whyistayedFit2,type="summary", xlim=c(0, .3))
plot.STM(whyistayedFit3,type="summary", xlim=c(0, .3))
#For paper, just top 20 
plot.STM(whyistayedFit3,type="summary", xlim=c(0, .3), ylim=c(55,75))


#show the topics that proportionally make up the most of the corpus
#Model 1 
thoughts17 <- findThoughts(whyistayedFit, texts=tweets$Text, topics=17, n=2)
thoughts14 <- findThoughts(whyistayedFit, texts=tweets$Text, topics=14, n=2)
thoughts13 <- findThoughts(whyistayedFit, texts=tweets$Text, topics=13, n=2)
thoughts16 <- findThoughts(whyistayedFit, texts=tweets$Text, topics=16, n=2)
thoughts19 <- findThoughts(whyistayedFit, texts=tweets$Text, topics=19, n=2)
thoughts12 <- findThoughts(whyistayedFit, texts=tweets$Text, topics=12, n=2)
par(mfrow = c(1,2), mar=c(.5, .5, 1, .5))
plotQuote(thoughts17$docs[[1]], main = "Topic 17")
plotQuote(thoughts14$docs[[1]], main = "Topic 14")
plotQuote(thoughts13$docs[[1]], main = "Topic 13")
plotQuote(thoughts16$docs[[1]], main = "Topic 16")
plotQuote(thoughts19$docs[[1]], main = "Topic 19")
plotQuote(thoughts12$docs[[1]], main = "Topic 12")

#Model 2
model2_thoughts39 <- findThoughts(whyistayedFit2, texts=tweets$Text, topics=39, n=2)
model2_thoughts58 <- findThoughts(whyistayedFit2, texts=tweets$Text, topics=58, n=2)
model2_thoughts30 <- findThoughts(whyistayedFit2, texts=tweets$Text, topics=30, n=2)
model2_thoughts23 <- findThoughts(whyistayedFit2, texts=tweets$Text, topics=23, n=2)
model2_thoughts51 <- findThoughts(whyistayedFit2, texts=tweets$Text, topics=51, n=2)
model2_thoughts8 <- findThoughts(whyistayedFit2, texts=tweets$Text, topics=8, n=2)
par(mfrow = c(1,2), mar=c(.5, .5, 1, .5))
plotQuote(model2_thoughts34$docs[[1]], main = "Model 2 Topic 34")
plotQuote(model2_thoughts58$docs[[1]], main = "Model 2 Topic 58")
plotQuote(model2_thoughts30$docs[[1]], main = "Model 2 Topic 30")
plotQuote(model2_thoughts23$docs[[1]], main = "Model 2 Topic 23")
plotQuote(model2_thoughts51$docs[[1]], main = "Model 2 Topic 51")
plotQuote(model2_thoughts8$docs[[1]], main = "Model 2 Topic 8")

#Model 3
model3_thoughts66 <- findThoughts(whyistayedFit3, texts=tweets$Text, topics=66, n=2)
model3_thoughts3 <- findThoughts(whyistayedFit3, texts=tweets$Text, topics=3, n=2)
model3_thoughts9 <- findThoughts(whyistayedFit3, texts=tweets$Text, topics=9, n=2)
model3_thoughts62 <- findThoughts(whyistayedFit3, texts=tweets$Text, topics=62, n=1)
model3_thoughts20 <- findThoughts(whyistayedFit3, texts=tweets$Text, topics=20, n=2)
model3_thoughts55 <- findThoughts(whyistayedFit3, texts=tweets$Text, topics=55, n=2)
par(mfrow = c(1,2), mar=c(.5, .5, 1, .5))
plotQuote(model3_thoughts66$docs[[1]], main = "Topic 66")
plotQuote(model3_thoughts3$docs[[1]], main = "Topic 3")
plotQuote(model3_thoughts9$docs[[1]], main = "Topic 9")
plotQuote(model3_thoughts62$docs[[1]], main = "Topic 62")
plotQuote(model3_thoughts20$docs[[1]], main = "Topic 20")
plotQuote(model3_thoughts55$docs[[1]], main = "Topic 55")

#reset par mfrow
par(mfrow=c(1,1))
#produce a word cloud of words in the above propportional topics 

#Model 1
cloud(whyistayedFit, topic = 17, scale = c(2, .25))
cloud(whyistayedFit, topic = 14, scale = c(2, .25))
cloud(whyistayedFit, topic = 13, scale = c(2, .25))
cloud(whyistayedFit, topic = 16, scale = c(2, .25))
cloud(whyistayedFit, topic = 19, scale = c(2, .25))
cloud(whyistayedFit, topic = 12, scale = c(2, .25))

#Model 2
##39, 58, 30, 23, 51, 8
require(RColorBrewer)
cloud(whyistayedFit2, topic = 39, scale = c(2, .25))
cloud(whyistayedFit2, topic = 58, scale = c(2, .25))
cloud(whyistayedFit2, topic = 30, scale = c(2, .25))
cloud(whyistayedFit2, topic = 23, scale = c(2, .25))
cloud(whyistayedFit2, topic = 51, scale = c(2, .25))
cloud(whyistayedFit2, topic = 8, scale = c(2, .25))

#Model 3
##66, 3, 9, 62, 20, 55
require(RColorBrewer)
cloud(whyistayedFit3, topic = 66, scale = c(2, .25))
cloud(whyistayedFit3, topic = 3, scale = c(2, .25))
cloud(whyistayedFit3, topic = 9, scale = c(2, .25))
cloud(whyistayedFit3, topic = 62, scale = c(2, .25))
cloud(whyistayedFit3, topic = 20, scale = c(2, .25))
cloud(whyistayedFit3, topic = 55, scale = c(2, .25))

#plot word clouds with sample tweets
par(mfrow = c(1,2), mar=c(.5, .5, 1, .5))

plotQuote(model3_thoughts66$docs[[1]], main = "Topic 66")
cloud(whyistayedFit3, topic = 66, scale = c(2, .25))

plotQuote(model3_thoughts3$docs[[1]], main = "Topic 3")
cloud(whyistayedFit3, topic = 3, scale = c(2, .25))

plotQuote(model3_thoughts9$docs[[1]], main = "Topic 9")
cloud(whyistayedFit3, topic = 9, scale = c(2, .25))

plotQuote(model3_thoughts62$docs[[1]], main = "Topic 62")
cloud(whyistayedFit3, topic = 62, scale = c(2, .25))

plotQuote(model3_thoughts20$docs[[1]], main = "Topic 20")
cloud(whyistayedFit3, topic = 20, scale = c(2, .25))

plotQuote(model3_thoughts55$docs[[1]], main = "Topic 55")
cloud(whyistayedFit3, topic = 55, scale = c(2, .25))


#UNC presentation 
#these two topics are related to each other 
model2_thoughts39 <- findThoughts(whyistayedFit2, texts=tweets$Text, topics=39, n=2)
par(mfrow = c(1,2), mar=c(.5, .5, 1, .5))
plotQuote(model2_thoughts34$docs[[1]], main = "Model 2 Topic 39")

model2_thoughts10 <- findThoughts(whyistayedFit2, texts=tweets$Text, topics=10, n=3)
par(mfrow = c(1,2), mar=c(.5, .5, 1, .5))
plotQuote(model2_thoughts10$docs[[1]], main = "Model 2 Topic 10")

cloud(whyistayedFit2, topic = 39 scale = c(2, .25))
cloud(whyistayedFit2, topic = 10, scale = c(2, .25))

##############################################################
#For presentation purposes, pair word clouds with quote plots#
##############################################################
par(mfrow = c(1,2), mar=c(.5, .5, 1, .5))

#Model 1

#plotQuote(thoughts17$docs[[1]], main = "Topic 17")
#something about the utf-8 encodings. I have to go back and figure out what't up 
#use library(stringi) and stri_enc_toutf8(thoughts17), but you lose variables. 
cloud(whyistayedFit, topic = 17, scale = c(2, .25))

plotQuote(thoughts14$docs[[1]], main = "Topic 14")
cloud(whyistayedFit, topic = 14, scale = c(2, .25))

plotQuote(thoughts13$docs[[1]], main = "Topic 13")
cloud(whyistayedFit, topic = 13, scale = c(2, .25))

plotQuote(thoughts16$docs[[1]], main = "Topic 16")
cloud(whyistayedFit, topic = 16, scale = c(2, .25))

plotQuote(thoughts19$docs[[1]], main = "Topic 19")
cloud(whyistayedFit, topic = 19, scale = c(2, .25))

plotQuote(thoughts12$docs[[1]], main = "Topic 12")
cloud(whyistayedFit, topic = 12, scale = c(2, .25))

#Model 2

plotQuote(model2_thoughts39$docs[[1]], main = "Model 2 Topic 39")
cloud(whyistayedFit2, topic = 39, scale = c(2, .25))

plotQuote(model2_thoughts58$docs[[1]], main = "Model 2 Topic 58")
cloud(whyistayedFit2, topic = 58, scale = c(2, .25))

plotQuote(model2_thoughts30$docs[[1]], main = "Model 2 Topic 30")
cloud(whyistayedFit2, topic = 30, scale = c(2, .25))

plotQuote(model2_thoughts23$docs[[1]], main = "Model 2 Topic 23")
cloud(whyistayedFit2, topic = 23, scale = c(2, .25))

plotQuote(model2_thoughts51$docs[[1]], main = "Model 2 Topic 51")
cloud(whyistayedFit2, topic = 51, scale = c(2, .25))

plotQuote(model2_thoughts8$docs[[1]], main = "Model 2 Topic 8")
cloud(whyistayedFit2, topic = 8, scale = c(2, .25))


#reset par mfrow
par(mfrow=c(1,1))
################################################################

#Extenisons
#stmBrowswer is for showing covariates(metadata) and their relationship to topics
#stmCorrViz generates an interactive, full-model HTML visualization of topic hierarchies for a fitted STM model. 


#to compare qualitiative codes with topic codes use LSA


