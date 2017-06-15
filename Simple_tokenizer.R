## Task 1 - Obtaining the data

## Setting the path where data is stored
setwd("C:/Users/andgo/Coursera - Data Science/Course 10 - Capstone Project/Models/Model_13")

## Loading texts form blog data
fileblog <- file("C:/Users/andgo/Coursera - Data Science/Course 10 - Capstone Project/final/en_US/en_US.blogs.txt", "rb") 
blogdata <-readLines(fileblog, encoding="latin-1")
close(fileblog)

## Loading texts form twitter data
filetwitter <- file("C:/Users/andgo/Coursera - Data Science/Course 10 - Capstone Project/final/en_US/en_US.twitter.txt", "rb") 
twitterdata <-readLines(filetwitter, encoding="latin-1")
close(filetwitter)

## Loading texts form twitter data
filenews <- file("C:/Users/andgo/Coursera - Data Science/Course 10 - Capstone Project/final/en_US/en_US.news.txt", "rb") 
newsdata <-readLines(filenews, encoding="latin-1")
close(filenews)

## Getting random samples of the three sources
sampleblog <- sample(blogdata, length(blogdata)*0.01)
rm(blogdata)
sampletwitter <- sample(twitterdata, length(twitterdata)*0.01)
rm(twitterdata)
samplenews <- sample(newsdata, length(newsdata)*0.01)
rm(newsdata)

## Loading required packages
library(openNLP)
library(tm)
library(qdap)

## Create the corpus object that contains all text entries
sampleblog <- as.String(sampleblog) ##format that annotation works
sampletwitter <- as.String(sampletwitter)
samplenews <- as.String(samplenews)

## Split all data in sentences
sent_token_annotator <- Maxent_Sent_Token_Annotator()
annotation_blog <- annotate(sampleblog, sent_token_annotator) ## split in sentences
annotation_twitter <- annotate(sampletwitter, sent_token_annotator) ## split in sentences
annotation_news <- annotate(samplenews, sent_token_annotator) ## split in sentences

## Create the corpus objects
corpusblog <- VCorpus(VectorSource(sampleblog[annotation_blog]))
corpustwitter <- VCorpus(VectorSource(sampletwitter[annotation_twitter]))
corpusnews <- VCorpus(VectorSource(samplenews[annotation_news]))


## Task 2 - Cleaning the data

## Set a function to remove special character
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

## This function is first used to remove colons and hyphens
corpusblog <- tm_map(corpusblog, toSpace, "-")
corpusblog <- tm_map(corpusblog, toSpace, ":")
corpusblog <- tm_map(corpusblog, toSpace, "`")
corpusblog <- tm_map(corpusblog, toSpace, "´")
corpusblog <- tm_map(corpusblog, toSpace, "=")
##corpusblog <- tm_map(corpusblog, toSpace, "â<U+0080><U+0094>")
corpustwitter <- tm_map(corpustwitter, toSpace, "-")
corpustwitter <- tm_map(corpustwitter, toSpace, ":")
corpustwitter <- tm_map(corpustwitter, toSpace, "`")
corpustwitter <- tm_map(corpustwitter, toSpace, "´")
corpustwitter <- tm_map(corpustwitter, toSpace, "=")
##corpustwitter <- tm_map(corpustwitter, toSpace, "â<U+0080><U+0094>")
corpusnews <- tm_map(corpusnews, toSpace, "-")
corpusnews <- tm_map(corpusnews, toSpace, ":")
corpusnews <- tm_map(corpusnews, toSpace, "`")
corpusnews <- tm_map(corpusnews, toSpace, "´")
corpusnews <- tm_map(corpusnews, toSpace, "=")
##corpustwitter <- tm_map(corpustwitter, toSpace, "â<U+0080><U+0094>")

# Remove ponctuation
corpusblog <- tm_map(corpusblog, removePunctuation)
corpustwitter <- tm_map(corpustwitter, removePunctuation)
corpusnews <- tm_map(corpusnews, removePunctuation)

# Transform to lowercase
corpusblog <- tm_map(corpusblog,content_transformer(tolower))
corpustwitter <- tm_map(corpustwitter,content_transformer(tolower))
corpusnews <- tm_map(corpusnews,content_transformer(tolower))

# Remove numbers
corpusblog <- tm_map(corpusblog, removeNumbers)
corpustwitter <- tm_map(corpustwitter, removeNumbers)
corpusnews <- tm_map(corpusnews, removeNumbers)

# Remove extraneous whitespaces
corpusblog <- tm_map(corpusblog, stripWhitespace)
corpustwitter <- tm_map(corpustwitter, stripWhitespace)
corpusnews <- tm_map(corpusnews, stripWhitespace)

## Removal of profanity words

## List of profanity terms
profanity <- scan("C:/Users/andgo/Coursera - Data Science/Course 10 - Capstone Project/profanity.txt", character(0), sep = "\n", encoding="UTF-8")

## Adding some words not considered to be profanity
profanity <- profanity[-(which(profanity%in%c("refugee","reject","remains","screw","welfare", "sweetness","shoot","sick","shooting","servant","sex","radical","racial","racist","republican","public","molestation","mexican","looser","lesbian","liberal","kill","killing","killer","heroin","fraud","fire","fight","fairy","^die","death","desire","deposit","crash","^crim","crack","^color","cigarette","church","^christ","canadian", "cancer","^catholic","cemetery","buried","burn","breast","^bomb","^beast","attack","australian","balls","baptist","^addict","abuse","abortion","amateur","asian","aroused","angry","arab","bible")==TRUE))]

## Aplying the profanity filter to the corpus data
profanity_vector <- VectorSource(profanity)
corpusblog <- tm_map(corpusblog, removeWords, profanity_vector)
corpustwitter <- tm_map(corpustwitter, removeWords, profanity_vector)
corpusnews <- tm_map(corpusnews, removeWords, profanity_vector)

## Merging all sources in only one corpus
allcorpus <- c(corpusblog,corpustwitter,corpusnews)
rm(corpusblog)
rm(corpustwitter)
rm(corpusnews)

## Task 3 - Exploratory analysis

## Loading required libraries
library(ggplot2)
library(data.table)
library(dplyr)
library(RWeka)

## Setting the size of n-grams 
onegram <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1))
bigram <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
trigram <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
quadgram <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))

## Tokenization

## One gram 
Onegram_allcorpus <- TermDocumentMatrix(allcorpus,control = list(tokenize = onegram))

temp <- slam::row_sums(Onegram_allcorpus)
temp <- data.frame(Terms = names(temp), Freq = temp)

Onegram_allcorpus_df <- arrange(temp, desc(Freq))
saveRDS(Onegram_allcorpus_df, file = "onegram_terms_df.Rda")

## Bi gram
Bigram_allcorpus <- TermDocumentMatrix(allcorpus,control = list(tokenize = bigram))

temp <- slam::row_sums(Bigram_allcorpus)
temp <- data.frame(Terms = names(temp), Freq = temp)

Bigram_allcorpus_df <- arrange(temp, desc(Freq))
terms <- as.character(Bigram_allcorpus_df$Terms)
Bigram_allcorpus_df <- data.frame(term = do.call(rbind, strsplit(terms, ' ')), Freq = Bigram_allcorpus_df$Freq)
saveRDS(Bigram_allcorpus_df, file = "bigram_terms_df.Rda")

## Tri gram
Trigram_allcorpus <- TermDocumentMatrix(allcorpus,control = list(tokenize = trigram))

temp <- slam::row_sums(Trigram_allcorpus)
temp <- data.frame(Terms = names(temp), Freq = temp)

Trigram_allcorpus_df <- arrange(temp, desc(Freq))
terms <- as.character(Trigram_allcorpus_df$Terms)
Trigram_allcorpus_df <- data.frame(term = do.call(rbind, strsplit(terms, ' ')), Freq = Trigram_allcorpus_df$Freq)
saveRDS(Trigram_allcorpus_df, file = "trigram_terms_df.Rda")

## Quad gram
Quadgram_allcorpus <- TermDocumentMatrix(allcorpus,control = list(tokenize = quadgram))

temp <- slam::row_sums(Quadgram_allcorpus)
temp <- data.frame(Terms = names(temp), Freq = temp)

Quadgram_allcorpus_df <- arrange(temp, desc(Freq))
terms <- as.character(Quadgram_allcorpus_df$Terms)
Quadgram_allcorpus_df <- data.frame(term = do.call(rbind, strsplit(terms, ' ')), Freq = Quadgram_allcorpus_df$Freq)
saveRDS(Quadgram_allcorpus_df, file = "quadgram_terms_df.Rda")