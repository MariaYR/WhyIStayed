# Final code for paper published in Journal of Techonology in Human Services 
#A Computational Social Science Perspective on Qualitative Darta Exploration: 
#Using topic models for the descriptive analysis of social media text 

# DOI: https://www.tandfonline.com/doi/full/10.1080/15228835.2019.1616350
library(dplyr)
library(tm)
library(ggplot2)
library(RWeka)
library(gplots)
library(corrplot)
library(RColorBrewer)
library(stm)
library(streamR)
library (tidyverse)
library(tidytext)
library(stminsights)
library(stringr)
library (Rtsne)
library(rsvd)
library(geometry)
library(igraph)

#Dedoose produces co-occureence matrix, but doesn't allow for visualization
#visualize here 
whyileft_viz <- read.csv("~/Dropbox/Publications/Heather&Maria/TopicModelingVSHumans/JTHS/stm/JTHS_R&R/whyileft_cooccurence.csv")
whyileft_viz
names(whyileft_viz)
#whyileft_viz <- whyileft_viz[order(whyileft_viz$WhyILeft),]

#make row names code names 
row.names(whyileft_viz) <- whyileft_viz$Code

#remove code column 
whyileft_viz <- whyileft_viz[,2:13]

#convert to matrix 
whyileft_matrix <- data.matrix(whyileft_viz)

#heatmap.2 (ggplot2)

par(mar=c(7,4,4,2)+0.1)
heatmap.2(whyileft_matrix, 
          dendrogram = "none", Colv = FALSE, Rowv = FALSE,
          scale = "none", col = brewer.pal(5, "Oranges"),
          key = TRUE, density.info = "none", key.title = NA, key.xlab = "Frequency",
          cexRow = 1, 
          cexCol = 1, 
          margins= c(12,10),
          trace = "none",
          main = "#WhyILeft Code Co-Occurrence")

#repeat for #whyIstayed
whyistayed_viz <- read.csv("~/Dropbox/Publications/Heather&Maria/TopicModelingVSHumans/JTHS/stm/JTHS_R&R/whyistayed_cooccurence.csv")
whyistayed_viz
names(whyistayed_viz)
#whyileft_viz <- whyileft_viz[order(whyileft_viz$WhyILeft),]

#make row names code names 
row.names(whyistayed_viz) <- whyistayed_viz$Code

#remove code column 
whyistayed_viz <- whyistayed_viz[,2:13]

#convert to matrix 
whyistayed_matrix <- data.matrix(whyistayed_viz)

#heatmap.2 (ggplot2)
par(mar=c(7,4,4,2)+0.1)
heatmap.2(whyistayed_matrix, 
          dendrogram = "none", Colv = FALSE, Rowv = FALSE,
          scale = "none", col = brewer.pal(5, "Blues"),
          key = TRUE, density.info = "none", key.title = NA, key.xlab = "Frequency",
          cexRow = 1, 
          cexCol = 1, 
          margins= c(12,12),
          trace = "none",
          main = "#WhyIStayed Code Co-Occurrence")


#STM 

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


# construct indicator for tweets that are #Whyistayed and another for #Whyileft 

table(as.integer(str_detect(tweets$Text,fixed("whyileft", ignore_case=TRUE))))
#0    1 
#1964 1091 
# where 1 = whyileft

tweets$hashtag_indicator <- as.integer(str_detect(tweets$Text,fixed("whyileft", ignore_case=TRUE)))
table(tweets$hashtag_indicator)

#see if they co-occur
rowIndx <- as.numeric(grepl('whyileft', tweets$Text, ignore.case = TRUE) & grepl('whyistayed', tweets$Text, ignore.case = TRUE))
table(rowIndx)
#rowIndx
#0    1 
#2370  685 

#index only where whyileft occurs by itself 
whyileft_only <-as.numeric(!grepl('whyistayed', tweets$Text, ignore.case = TRUE))
table(whyileft_only)

#index only where whyistayed occurs by itself 
whyistayed_only <-as.numeric(!grepl('whyileft', tweets$Text, ignore.case = TRUE))
table(whyistayed_only)

#make new variables 
tweets$whyileft_only <- as.numeric(!grepl('whyistayed', tweets$Text, ignore.case = TRUE))
table(tweets$whyileft_only)

tweets$whyistayed_only <- as.numeric(!grepl('whyileft', tweets$Text, ignore.case = TRUE))
table(tweets$whyistayed_only)

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

plotRemoved(processed$documents, lower.thresh = seq(1,100, by = 10))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 10)
#Removing 3956 of 4372 terms (7234 of 28206 tokens) due to frequency 
#Removing 3 Documents with No Words 
#Your corpus now has 3052 documents, 416 terms and 20972 tokens.

docs<- out$documents
vocab<- out$vocab
meta <- out$meta

#inspect to make sure preprocessing went ok 
head(docs)#this tells you how many words are in what position 
head(vocab)
head(meta)

#Estimate 

#fit a model that uses the same number of topics as the qual coders used. 
#qual topics = 97 
whyistayedFit_qualcomp <- stm(documents = out$documents, vocab = out$vocab, K= 97, 
                      data = out$meta, init.type = "Spectral")

#to minimize iterations  specificy max.em.its = 75(or some other number of iterations)

#Now use new variable whyileft_only as prevalence variable, qual_comparision
set.seed(01238)
whyistayedFit_qual_prev <- stm(documents = out$documents, vocab = out$vocab, K= 97, 
                               prevalence = ~whyileft_only + whyistayed_only, data = out$meta, init.type = "Spectral")

labelTopics (whyistayedFit_noK)#41 topics
labelTopics (whyistayedFit_qualcomp)#97 topics 

#now see what happens with noK
set.seed(01238)
whyistayedFit_noK <- stm(documents = out$documents, vocab = out$vocab, K= 0, 
                      data = out$meta, init.type = "Spectral")
#k=39
set.seed(01238)
whyistayedFit_noK_prev <- stm(documents = out$documents, vocab = out$vocab, K= 0, 
                         prevalence = ~whyileft_only + whyistayed_only, data = out$meta, init.type = "Spectral")

labelTopics (whyistayedFit_noK_prev)#39 topics
labelTopics (whyistayedFit_qual_prev)#97 topics 

#optimal number of topics based on diagnostics - k65
set.seed(01238)
whyistayedFit_k65 <- stm(documents = out$documents, vocab = out$vocab, K= 65, 
                              prevalence = ~whyileft_only + whyistayed_only, data = out$meta, init.type = "Spectral")

labelTopics (whyistayedFit_k65)
#top 7 
labelTopics (whyistayedFit_k65, n=7)

#Evaluate
#go to 120 to see if 90+ works for topics, since manual codes = 97
storage <- searchK(out$documents, out$vocab, K = c(20,30,40,50,60,70,80,90,100,110,120), data = meta) 
#hand codes = 97 codes, best fit = 65 topics per graph 

plot(storage)

#lowest held out likelihood 
#lowest residual
#highest semantic coherence 
#highest lower bound

#Understand 
#must install Rtsne, rsvd, and geometry packages in order to do this

#STM lets us do a couple of things: 
#1: display words associated with topics 

labelTopics (whyistayedFit_noK)
labelTopics (whyistayedFit_qualcomp)
labelTopics (whyistayedFit_noK_prev)
labelTopics (whyistayedFit_qual_prev)
labelTopics (whyistayedFit_k65)

#convert to dataframe and export to csv 
k65_codes <- make.dt(whyistayedFit_k65)
write.csv(k65_codes, "topicModelCodesK65.csv")

plot.STM(whyistayedFit_noK,type="summary", xlim=c(0, .3))
plot.STM(whyistayedFit_qualcomp,type="summary", xlim=c(0, .3))
plot.STM(whyistayedFit_noK_prev,type="summary", xlim=c(0, .3))
plot.STM(whyistayedFit_qual_prev,type="summary", xlim=c(0, .3))
plot.STM(whyistayedFit_k65,type="summary", xlim=c(0, .3))

#Rstudio isn't producing my plots in the graphics window
#not sure what the deal is 
png(filename="~/Dropbox/Publications/Heather&Maria/TopicModelingVSHumans/JTHS/stm/JTHS_R&R/k65_toptopics.png")
plot.STM(whyistayedFit_k65,type="summary", xlim=c(0, .3), ylim=c(45,65))
dev.off()
#########

#41, 39, 97, 65
#For paper, just top 20 
plot.STM(whyistayedFit_noK,type="summary", xlim=c(0, .3), ylim=c(21,41))
plot.STM(whyistayedFit_qualcomp,type="summary", xlim=c(0, .3), ylim=c(77,97))
plot.STM(whyistayedFit_noK_prev,type="summary", xlim=c(0, .3), ylim=c(19,39))
plot.STM(whyistayedFit_qual_prev,type="summary", xlim=c(0, .3), ylim=c(77,97))
plot.STM(whyistayedFit_k65,type="summary", xlim=c(0, .3), ylim=c(45,65))

#Estimate the relationship between metadata and topics/topical content 
#effect of #whyILeft and #whyistayed hashtags
#number of simulations default = 25 

#first qualcomp
qualcomp_prep_all <- estimateEffect(1:97 ~ whyileft_only + whyistayed_only, whyistayedFit_qual_prev, meta = meta, uncertainty = "Global")
summary(qualcomp_prep_all)

#NoK_prev
noK_prep_all <- estimateEffect(1:39 ~ whyileft_only + whyistayed_only, whyistayedFit_noK_prev, meta = meta, uncertainty = "Global")
summary(noK_prep_all)

#k65
k65_prep_all <- estimateEffect(1:65 ~ whyileft_only + whyistayed_only, whyistayedFit_k65, meta = meta, uncertainty = "Global")
summary(k65_prep_all)

#plot estimated effect of #whyILeft and #whyistayed
plot(qualcomp_prep_all, "whyileft_only", model=whyistayedFit_qual_prev, method="pointestimate",
     width = 5, main ="Estimated effect of #WhyIStayed vs #WhyIleft on topic proportions")

#k65
plot(k65_prep_all, "whyileft_only", model=whyistayedFit_k65, method="pointestimate",
     width = 5, main ="Estimated effect of #WhyIStayed vs #WhyIleft on topic proportions:K=65")


#plot top 5 topics for k65 and qualcomp 
top_qualcomp <- estimateEffect(c(89, 83, 28, 47, 1) ~ whyileft_only + whyistayed_only, whyistayedFit_qual_prev, meta = meta, uncertainty = "Global")
plot(top_qualcomp, "whyileft_only", model=whyistayedFit_qual_prev, method="pointestimate",
     width = 5, main ="Estimated effect of #WhyIStayed vs #WhyIleft on Top Topics: Qualitative Comparision")

top_k65 <- estimateEffect(c(47, 18, 45, 43, 5) ~ whyileft_only + whyistayed_only, whyistayedFit_k65, meta = meta, uncertainty = "Global")
plot(top_noK, "whyileft_only", model=whyistayedFit_noK_prev, method="pointestimate",
     width = 5, main ="Estimated effect of #WhyIStayed vs #WhyIleft on Top Topics: K=65")

#3. Calculate topic correlations 
corStayed_k65 <- topicCorr(whyistayedFit_k65, method = "simple", cutoff = 0.01, verbose = TRUE)
corStayed_qualcomp <- topicCorr(whyistayedFit_qual_prev, method = "simple", cutoff = 0.01, verbose = TRUE)

#remove old stms
rm(whyistayedFit, whyistayedFit2, whyistayedFit3, whyistayedFit4)

########
#Topic Correlations - best visualized in stminsights
#########

#output
#posadj	
#K by K adjacency matrix where an edge represents positive correlation selected by the model.
#poscor	
#K by K correlation matrix. It takes values of zero where the correlation is either negative or the edge is unselected by the model selection procedure.
#cor	
#K by K correlation matrix element-wise multiplied by the adjacency matrix. Note that this will contain significant negative correlations as well as positive correlations.

#must install igraph before graphing correlations 

plot (corStayed_k65,  cex = 2.0)
plot (corStayed_qualcomp, cex = 2.0)
#par(mar=c(0,0,1,0))

#remove previous thoughts, models_thoughts, and other extras
#commented out to prevent running by accident again =) 
#rm(list = ls(pattern = "^model"))
#rm(list = ls(pattern = "^thoughts"))
#rm(list = ls(pattern = "^cor"))
#rm(top_k75)

#visualize tweets in topics
#qualcomp
#qualcomp = 89, 83, 28, 47, 1
names(meta)
thoughts89 <- findThoughts(whyistayedFit_qual_prev, texts=meta$Text, topics=89, n=2)
thoughts83 <- findThoughts(whyistayedFit_qual_prev, texts=meta$Text, topics=83, n=2)
thoughts28 <- findThoughts(whyistayedFit_qual_prev, texts=meta$Text, topics=28, n=2)
thoughts47 <- findThoughts(whyistayedFit_qual_prev, texts=meta$Text, topics=47, n=2)
thoughts1 <- findThoughts(whyistayedFit_qual_prev, texts=meta$Text, topics=1, n=2)

#k65
#k65 = 47, 18, 45, 43, 5

k65_thoughts47 <- findThoughts(whyistayedFit_k65, texts=meta$Text, topics=47, n=2)
k65_thoughts18 <- findThoughts(whyistayedFit_k65, texts=meta$Text, topics=18, n=2)
k65_thoughts45 <- findThoughts(whyistayedFit_k65, texts=meta$Text, topics=45, n=2)
k65_thoughts43 <- findThoughts(whyistayedFit_k65, texts=meta$Text, topics=43, n=2)
k65_thoughts5 <- findThoughts(whyistayedFit_k65, texts=meta$Text, topics=5, n=2)

##############################################################
#For presentation purposes, pair word clouds with quote plots#
##############################################################
par(mfrow = c(1,2), mar=c(.5, .5, 1, .5))

#QualComp
#qualcomp = 89, 83, 28, 47, 1
plotQuote(thoughts89$docs[[1]], main = "Topic 89")
cloud(whyistayedFit_qual_prev, topic = 89, scale = c(2, .25))

plotQuote(thoughts83$docs[[1]], main = "Topic 83")
cloud(whyistayedFit_qual_prev, topic = 83, scale = c(2, .25))

plotQuote(thoughts28$docs[[1]], main = "Topic 28")
cloud(whyistayedFit_qual_prev, topic = 28, scale = c(2, .25))

plotQuote(thoughts47$docs[[1]], main = "Topic 47")
cloud(whyistayedFit_qual_prev, topic = 47, scale = c(2, .25))

plotQuote(thoughts1$docs[[1]], main = "Topic 1")
cloud(whyistayedFit_qual_prev, topic = 1, scale = c(2, .25))

#k65
#k65 = 47, 18, 45, 43, 5

plotQuote(k65_thoughts47$docs[[1]], main = "K=65, Topic 47")
cloud(whyistayedFit_k65, topic = 47, scale = c(2, .25))

plotQuote(k65_thoughts18$docs[[1]], main = "K=65, Topic 18")
cloud(whyistayedFit_k65, topic = 18, scale = c(2, .25))

plotQuote(k65_thoughts45$docs[[1]], main = "K=65, Topic 45")
cloud(whyistayedFit_k65, topic = 45, scale = c(2, .25))

plotQuote(k65_thoughts43$docs[[1]], main = "K=65, Topic 43")
cloud(whyistayedFit_k65, topic = 43, scale = c(2, .25))

plotQuote(k65_thoughts5$docs[[1]], main = "K=65, Topic 5")
cloud(whyistayedFit_k65, topic = 5, scale = c(2, .25))

#reset par mfrow
par(mfrow=c(1,1))


#########
#Run STMInsights to visualize rest
########

#for STM_insights, change fit names to fit in plot 

k65 <- whyistayedFit_k65
noK <- whyistayedFit_noK
noK_Prev <- whyistayedFit_noK_prev
quacomp_Prev <- whyistayedFit_qual_prev
qualcomp <- whyistayedFit_qualcomp

run_stminsights()