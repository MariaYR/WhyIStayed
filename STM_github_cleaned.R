# =============================================================================
# A Computational Social Science Perspective on Qualitative Data Exploration:
# Using topic models for the descriptive analysis of social media text
#
# Journal of Technology in Human Services
# DOI: https://www.tandfonline.com/doi/full/10.1080/15228835.2019.1616350
#
# Expects the following files in a local "data/" folder:
#   - whyileft_cooccurence.csv
#   - whyistayed_cooccurence.csv
#   - forSTMfinal.csv
# Figures are written to a local "output/" folder.
# =============================================================================

library(dplyr)
library(tm)
library(ggplot2)
library(caret)       # formerly RWeka; substituted 2026-07-23 due to unstable Java workflow
library(gplots)
library(corrplot)
library(RColorBrewer)
library(stm)
library(streamR)
library(tidyverse)
library(tidytext)
library(stminsights)
library(stringr)
library(Rtsne)        # required by stm for visualization (Understand section)
library(rsvd)          # required by stm for visualization (Understand section)
library(geometry)      # required by stm for visualization (Understand section)
library(igraph)         # required for plotting topic correlation networks

# =============================================================================
# Visualize code co-occurrence matrices
# (Co-occurrence matrices were produced in Dedoose, which doesn't support
# visualization, so they're plotted here instead.)
# =============================================================================

## ---- #WhyILeft ----
whyileft_viz <- read.csv("data/whyileft_cooccurence.csv")
whyileft_viz
names(whyileft_viz)

# make row names the code names, then drop the now-redundant Code column
row.names(whyileft_viz) <- whyileft_viz$Code
whyileft_viz <- whyileft_viz[, 2:13]

# convert to matrix for heatmap.2
whyileft_matrix <- data.matrix(whyileft_viz)

par(mar = c(7, 4, 4, 2) + 0.1)
heatmap.2(whyileft_matrix,
          dendrogram = "none", Colv = FALSE, Rowv = FALSE,
          scale = "none", col = brewer.pal(5, "Oranges"),
          key = TRUE, density.info = "none", key.title = NA, key.xlab = "Frequency",
          cexRow = 1,
          cexCol = 1,
          margins = c(12, 10),
          trace = "none",
          main = "#WhyILeft Code Co-Occurrence")

## ---- #WhyIStayed ----
whyistayed_viz <- read.csv("data/whyistayed_cooccurence.csv")
whyistayed_viz
names(whyistayed_viz)

# make row names the code names, then drop the now-redundant Code column
row.names(whyistayed_viz) <- whyistayed_viz$Code
whyistayed_viz <- whyistayed_viz[, 2:13]

# convert to matrix for heatmap.2
whyistayed_matrix <- data.matrix(whyistayed_viz)

par(mar = c(7, 4, 4, 2) + 0.1)
heatmap.2(whyistayed_matrix,
          dendrogram = "none", Colv = FALSE, Rowv = FALSE,
          scale = "none", col = brewer.pal(5, "Blues"),
          key = TRUE, density.info = "none", key.title = NA, key.xlab = "Frequency",
          cexRow = 1,
          cexCol = 1,
          margins = c(12, 12),
          trace = "none",
          main = "#WhyIStayed Code Co-Occurrence")

# =============================================================================
# Structural Topic Model (STM)
# =============================================================================

## ---- Ingest ----
tweets <- read.csv("data/forSTMfinal.csv", header = TRUE)
head(tweets)

# Sanity check: NA counts by column (recorded here for reference; the source
# data isn't included in this repo, so this is left informational rather than
# filtering on it directly)
na_count <- sapply(tweets, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

## ---- Construct hashtag indicators ----

# indicator = 1 when the tweet contains #whyileft
table(as.integer(str_detect(tweets$Text, fixed("whyileft", ignore_case = TRUE))))
tweets$hashtag_indicator <- as.integer(str_detect(tweets$Text, fixed("whyileft", ignore_case = TRUE)))
table(tweets$hashtag_indicator)

# check whether the two hashtags co-occur in the same tweet
rowIndx <- as.numeric(grepl("whyileft", tweets$Text, ignore.case = TRUE) &
                         grepl("whyistayed", tweets$Text, ignore.case = TRUE))
table(rowIndx)

# indicator for tweets where only #whyileft appears (i.e. #whyistayed absent)
tweets$whyileft_only <- as.numeric(!grepl("whyistayed", tweets$Text, ignore.case = TRUE))
table(tweets$whyileft_only)

# indicator for tweets where only #whyistayed appears (i.e. #whyileft absent)
tweets$whyistayed_only <- as.numeric(!grepl("whyileft", tweets$Text, ignore.case = TRUE))
table(tweets$whyistayed_only)

## ---- Clean text ----

# strip non-ASCII characters (emoji, curly quotes, etc.)
tweets$Text <- sapply(tweets$Text, function(row) iconv(row, "latin1", "ASCII", sub = ""))

# builds a text corpus: lowercases, removes stopwords/numbers, stems words
processed <- textProcessor(tweets$Text, metadata = tweets)

## ---- Prepare ----
plotRemoved(processed$documents, lower.thresh = seq(1, 100, by = 10))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 10)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# inspect to confirm preprocessing went as expected
head(docs)
head(vocab)
head(meta)

## ---- Estimate ----

# fit a model using the same number of topics as the qualitative coders used (K = 97)
whyistayedFit_qualcomp <- stm(documents = out$documents, vocab = out$vocab, K = 97,
                               data = out$meta, init.type = "Spectral")

# same K = 97 model, now with hashtag prevalence covariates
set.seed(01238)
whyistayedFit_qual_prev <- stm(documents = out$documents, vocab = out$vocab, K = 97,
                                prevalence = ~ whyileft_only + whyistayed_only,
                                data = out$meta, init.type = "Spectral")

labelTopics(whyistayedFit_qual_prev)  # 97 topics, with hashtag prevalence
labelTopics(whyistayedFit_qualcomp)   # 97 topics, no prevalence

# let stm choose K automatically
set.seed(01238)
whyistayedFit_noK <- stm(documents = out$documents, vocab = out$vocab, K = 0,
                          data = out$meta, init.type = "Spectral")

set.seed(01238)
whyistayedFit_noK_prev <- stm(documents = out$documents, vocab = out$vocab, K = 0,
                               prevalence = ~ whyileft_only + whyistayed_only,
                               data = out$meta, init.type = "Spectral")

labelTopics(whyistayedFit_noK_prev)
labelTopics(whyistayedFit_qual_prev)
labelTopics(whyistayedFit_noK)
labelTopics(whyistayedFit_noK_prev)

# K = 65, chosen based on diagnostics below (searchK)
set.seed(01238)
whyistayedFit_k65 <- stm(documents = out$documents, vocab = out$vocab, K = 65,
                          prevalence = ~ whyileft_only + whyistayed_only,
                          data = out$meta, init.type = "Spectral")

labelTopics(whyistayedFit_k65)
labelTopics(whyistayedFit_k65, n = 7)  # top 7 terms per topic

## ---- Evaluate ----

# compare fit across a range of K (manual/qualitative coding used 97 codes)
storage <- searchK(out$documents, out$vocab,
                    K = c(20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120),
                    data = meta)
# best fit by diagnostics: K = 65 (lowest held-out likelihood, lowest residual,
# highest semantic coherence, highest lower bound)

plot(storage)

## ---- Understand ----
# Requires the Rtsne, rsvd, and geometry packages (loaded above).

# 1: display words associated with each topic
labelTopics(whyistayedFit_noK)
labelTopics(whyistayedFit_qualcomp)
labelTopics(whyistayedFit_noK_prev)
labelTopics(whyistayedFit_qual_prev)
labelTopics(whyistayedFit_k65)

# export K = 65 topic-word table
k65_codes <- make.dt(whyistayedFit_k65)
write.csv(k65_codes, "output/topicModelCodesK65.csv")

# summary plots (proportion of corpus per topic) for each model
plot.STM(whyistayedFit_noK, type = "summary", xlim = c(0, .3))
plot.STM(whyistayedFit_qualcomp, type = "summary", xlim = c(0, .3))
plot.STM(whyistayedFit_noK_prev, type = "summary", xlim = c(0, .3))
plot.STM(whyistayedFit_qual_prev, type = "summary", xlim = c(0, .3))
plot.STM(whyistayedFit_k65, type = "summary", xlim = c(0, .1))

# top 7 topics for K = 65, saved to file
png(filename = "output/k65_toptopics.png")
plot.STM(whyistayedFit_k65, type = "summary", n = 7, xlim = c(0, .3), ylim = c(45, 65))
dev.off()

# for the paper: top 20 topics per model (41, 39, 97, 65 total topics respectively)
plot.STM(whyistayedFit_noK, type = "summary", xlim = c(0, .3), ylim = c(21, 41))
plot.STM(whyistayedFit_qualcomp, type = "summary", xlim = c(0, .3), ylim = c(77, 97))
plot.STM(whyistayedFit_noK_prev, type = "summary", xlim = c(0, .3), ylim = c(19, 39))
plot.STM(whyistayedFit_qual_prev, type = "summary", xlim = c(0, .3), ylim = c(77, 97))
plot.STM(whyistayedFit_k65, type = "summary", xlim = c(0, .3), ylim = c(45, 65))

## ---- Estimate the relationship between metadata and topics ----
# Effect of the #whyILeft / #whyIstayed hashtags on topic proportions
# (default number of simulations = 25)

# qualcomp (K = 97, with prevalence)
qualcomp_prep_all <- estimateEffect(1:97 ~ whyileft_only + whyistayed_only,
                                     whyistayedFit_qual_prev, meta = meta, uncertainty = "Global")
summary(qualcomp_prep_all)

# noK_prev
noK_prep_all <- estimateEffect(1:39 ~ whyileft_only + whyistayed_only,
                                whyistayedFit_noK_prev, meta = meta, uncertainty = "Global")
summary(noK_prep_all)

# k65
k65_prep_all <- estimateEffect(1:65 ~ whyileft_only + whyistayed_only,
                                whyistayedFit_k65, meta = meta, uncertainty = "Global")
summary(k65_prep_all)

# plot estimated effect of #whyILeft vs #whyIstayed
plot(qualcomp_prep_all, "whyileft_only", model = whyistayedFit_qual_prev, method = "pointestimate",
     width = 5, main = "Estimated effect of #WhyIStayed vs #WhyIleft on topic proportions")

plot(k65_prep_all, "whyileft_only", model = whyistayedFit_k65, method = "pointestimate",
     width = 5, main = "Estimated effect of #WhyIStayed vs #WhyIleft on topic proportions: K=65")

# plot top 5 topics, qualcomp model (topics 89, 83, 28, 47, 1)
top_qualcomp <- estimateEffect(c(89, 83, 28, 47, 1) ~ whyileft_only + whyistayed_only,
                                whyistayedFit_qual_prev, meta = meta, uncertainty = "Global")
plot(top_qualcomp, "whyileft_only", model = whyistayedFit_qual_prev, method = "pointestimate",
     width = 5, main = "Estimated effect of #WhyIStayed vs #WhyIleft on Top Topics: Qualitative Comparison")

# plot top 5 topics, K = 65 model (topics 47, 18, 45, 43, 5)
top_k65 <- estimateEffect(c(47, 18, 45, 43, 5) ~ whyileft_only + whyistayed_only,
                           whyistayedFit_k65, meta = meta, uncertainty = "Global")
plot(top_k65, "whyileft_only", model = whyistayedFit_k65, method = "pointestimate",
     width = 5, main = "Estimated effect of #WhyIStayed vs #WhyIleft on Top Topics: K=65")

# plot top 5 topics, noK model (topics 8, 15, 20, 6, 37)
top_noK <- estimateEffect(c(8, 15, 20, 6, 37) ~ whyileft_only,
                           whyistayedFit_noK, meta = meta, uncertainty = "Global")
plot(top_noK, "whyileft_only", model = whyistayedFit_noK, method = "pointestimate",
     width = 5, main = "Estimated effect of #WhyIStayed vs #WhyIleft on Top Topics: NoK")

## ---- Calculate topic correlations ----
corStayed_k65 <- topicCorr(whyistayedFit_k65, method = "simple", cutoff = 0.01, verbose = TRUE)
corStayed_qualcomp <- topicCorr(whyistayedFit_qual_prev, method = "simple", cutoff = 0.01, verbose = TRUE)

# whyistayedFit, whyistayedFit2/3/4 were earlier exploratory fits (K = 20, 62,
# 75, 75) cleared out at this point in the original session. Their fitting
# code isn't part of this trimmed script, so this rm() is left out to avoid
# an error on a fresh run:
# rm(whyistayedFit, whyistayedFit2, whyistayedFit3, whyistayedFit4)

# Topic correlations are best visualized in stminsights (see bottom of script).
# Output reference:
#   posadj - K x K adjacency matrix; an edge = positive correlation selected by the model
#   poscor - K x K correlation matrix; zero where correlation is negative or unselected
#   cor    - K x K correlation matrix, element-wise multiplied by the adjacency matrix
#            (includes significant negative correlations as well as positive ones)

plot(corStayed_k65, cex = 2.0)
plot(corStayed_qualcomp, cex = 2.0)
par(mar = c(0, 0, 1, 0))

## ---- Visualize tweets by topic ----

# qualcomp: topics 89, 83, 28, 47, 1
names(meta)
thoughts89 <- findThoughts(whyistayedFit_qual_prev, texts = meta$Text, topics = 89, n = 2)
thoughts83 <- findThoughts(whyistayedFit_qual_prev, texts = meta$Text, topics = 83, n = 2)
thoughts28 <- findThoughts(whyistayedFit_qual_prev, texts = meta$Text, topics = 28, n = 2)
thoughts47 <- findThoughts(whyistayedFit_qual_prev, texts = meta$Text, topics = 47, n = 2)
thoughts1 <- findThoughts(whyistayedFit_qual_prev, texts = meta$Text, topics = 1, n = 2)

# k65: topics 47, 18, 45, 43, 5
k65_thoughts47 <- findThoughts(whyistayedFit_k65, texts = meta$Text, topics = 47, n = 2)
k65_thoughts18 <- findThoughts(whyistayedFit_k65, texts = meta$Text, topics = 18, n = 2)
k65_thoughts45 <- findThoughts(whyistayedFit_k65, texts = meta$Text, topics = 45, n = 2)
k65_thoughts43 <- findThoughts(whyistayedFit_k65, texts = meta$Text, topics = 43, n = 2)
k65_thoughts5 <- findThoughts(whyistayedFit_k65, texts = meta$Text, topics = 5, n = 2)

## ---- Pair word clouds with quote plots (for presentation) ----
par(mfrow = c(1, 2), mar = c(.5, .5, 1, .5))

# qualcomp: topics 89, 83, 28, 47, 1
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

# k65: topics 47, 18, 45, 43, 5
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

par(mfrow = c(1, 1))  # reset plotting layout

## ---- Run stminsights for remaining visualizations ----

# rename fits for display in the stminsights app
k65 <- whyistayedFit_k65
noK <- whyistayedFit_noK
noK_Prev <- whyistayedFit_noK_prev
quacomp_Prev <- whyistayedFit_qual_prev
qualcomp <- whyistayedFit_qualcomp

run_stminsights()
