#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(data.table)
library(dplyr)
library(RWeka)
library(openNLP)
library(tm)
library(qdap)
library(stringr)
library(shiny)

dt <- readRDS("data/dt_kn.Rda")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  prediction_words <- reactive({
    
    ## Getting the phrase and extracting words to evaluate the prediction
    phrase_temp <- as.String(input$phrase_user)
    
    sent_token_annotator <- Maxent_Sent_Token_Annotator()
    annotation <- annotate(phrase_temp, sent_token_annotator)
    corpus <- VCorpus(VectorSource(phrase_temp[annotation]))

    ## Cleaning the data provided by the user
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus,content_transformer(tolower))
    corpus <- tm_map(corpus, stripWhitespace)

    word_vector <- do.call(rbind, str_match_all(corpus[[length(corpus)]]$content, '\\w+\\b'))

    ## Filtering the n-grams that corresponds to sequence of words
    predict1 <- dt[is.na(term.1) == TRUE & is.na(term.2) == TRUE & is.na(term.3) == TRUE]
    predict2 <- dt[is.na(term.1) == TRUE & is.na(term.2) == TRUE & term.3 == word_vector[length(word_vector),1]]
    predict3 <- dt[is.na(term.1) == TRUE & term.2 == word_vector[length(word_vector)-1,1] & term.3 == word_vector[length(word_vector),1]]
    predict4 <- dt[term.1 == word_vector[length(word_vector)-2,1] & term.2 == word_vector[length(word_vector)-1,1] & term.3 == word_vector[length(word_vector),1]]

    #prediction <- arrange(rbind(predict1,predict2,predict3,predict4), desc(Pkn))
    prediction <- arrange(rbind(predict2,predict3,predict4), desc(Pkn))
    prediction <- subset(prediction, !duplicated(prediction[,4]))

   if(nrow(prediction) == 0){
      prediction <- arrange(predict1, desc(Pkn,3))
    }

    prediction <- subset(prediction, select = -c(term.1, term.2, term.3))
    colnames(prediction)[which(names(prediction) == "term.4")] <- "Suggestion"
    colnames(prediction)[which(names(prediction) == "Pkn")] <- "Knesser-Ney Prob."
    
    prediction
    
  })
  
  output$promissing <- renderDataTable(prediction_words(), options = list(
    pageLength = 10))
  
  
})
