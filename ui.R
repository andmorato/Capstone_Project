#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Prediction of next word"),
  
  # Sidebar with a text box for user entry 
  sidebarLayout(
    sidebarPanel(
      h4("Write the sentence here and look for predictions in the table."),
      textInput('phrase_user',label="Sentence:",value="I would like to ")
      ),
    
    # Show a table with most promissing words
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Main", br(), 
                           h4("The list with most promossing words is shown below:"),
                           dataTableOutput("promissing")
                  ),
                  tabPanel("Documentation", br(),
                           h3("Data Science Capstone Project"),
                           h5("This is a shiny application that evaluates the next word to be written by the user."),
                           helpText(a("Click Here to access GitHub repository", href="https://github.com/andmorato/Capstone_Project")),
                           h4("Overview"),
                           h5("In this project it was developed an aplication to predict the next word in a sentence provided by the user. The theory of NLP (Natural Language Processing) was applied in the form of a n-gram model."),
                           helpText(a("Click here for more information about n-gram model (Wikipedia).", href="https://en.wikipedia.org/wiki/N-gram")),
                           h4("Dataset"),
                           h5("The dataset used for exploration and initial modeling is obtained in Corpora, in following link:"),
                           helpText(a("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", href="https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")),
                           h5("This dataset is collected from publicly available sources by a web crawler. The data is classified by three main sources:"),
                           h6("Blogs"),
                           h6("Twitter"),
                           h6("News"),
                           
                           h4("Using the application:"),
                           h5("Just write a sentence and wait for next word prediction. It will be shown in form of a table with most promissing words. These words are sorted by Kneser-Ney smoothing probability."),
                           helpText(a("Click here for more information about Kneser-Ney smoothing method (Wikipedia).", href="https://en.wikipedia.org/wiki/Kneser%E2%80%93Ney_smoothing")),
                           h5("There are various methods for calculation of n-gram probabilities. The Kneser-Ney method was chosen by the fact that most lectures obtained says that this method is the most accurate."),
                           helpText(a("Click here for access an article that presents an evaluation of smoothing techniques.", href="http://u.cs.biu.ac.il/~yogo/courses/mt2013/papers/chen-goodman-99.pdf")),
                           h4("Model selection:"),
                           h5("The model features was chosen by balancing two main boundaries:"),
                           h6("Speed of processing"),
                           h6("Model accuracy"),
                           h5("Processing speed is easy to evaluate because it is just the time elapsed among user entry and app response. Waiting 2 seconds was considered the limit for good user experience."),
                           h5("Accuracy evaluation is a lot more complex. There is a wide range of techniques available. One of the most used is the perplexity analysis."),
                           helpText(a("Click here for more information about perplexity evaluation (Wikipedia).", href="https://en.wikipedia.org/wiki/Perplexity")),
                           h5("In this project it was not applied the perplexity evaluation. This decision was made by the fact that this method is quite complex and hard to interpret. A qualitative approach was applied since it is easier for other students evaluate this method."),
                           h5("The score in quiz 2 and quiz 3 was adopted as a criteria for model accuracy."),
                           h5("The following variables was selected for model design:"),
                           h6("Size of n-gram: 2-gram - 3-gram - 4-gram"),
                           h6("Stopword removal: yes - no"),
                           h6("Amount of database used: 1% - 5% - 10%"),
                           h5("The model with best accuracy was the one with following parameters: 4-gram - no stopword removal - 10% of database. This model achieved 50% in both quizzes. This model was quite slow, taking more than five seconds to give answers."),
                           h5("The best balance among accuracy and speed was achieved by a model with following parameters:"),
                           h6("4-gram"),
                           h6("No stopword removal"),
                           h6("1% of database"),
                           h5(strong("This model obtained 40% in first quiz and 30% in second quiz and take less than two seconds to give an answer.")),
                           h5("Below is presented a chart with results of all models evaluated."),
                           img(src="modelselection.png", height = 325, width = 650)
                           
                  )
                  
      )
      
      
      
    )
  )
))
