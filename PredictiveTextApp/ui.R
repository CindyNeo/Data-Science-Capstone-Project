library(shiny)
library(quanteda)
library(dplyr)
library(curl)

## Load data 

unigrams <- readRDS("./data/unigramsv2sw.rds")
bigrams <- readRDS("./data/bigramsv2sw.rds")
trigrams <- readRDS("./data/trigramsv2sw.rds")
fourgrams <- readRDS("./data/fourgramsv2sw.rds")


## ui interface

ui <- navbarPage("Text Predictor",
                 tabPanel("Predictor",
                          h2("This app will predict the next top 3 words using the Stupid Backoff Text Predictor"),
                          tags$br(),
                          
                          splitLayout(
                              sidebarPanel(
                                  width = 12,
                                  position = "left",
                                  textInput("phrase", "Input Phrase:", value = ""),
                                  submitButton("Predict")
                              ),
                              mainPanel(
                                  width = 12,
                                  position = "right",
                                  h4("Predicted words (Top 3 possibilities): "),
                                  textOutput("nextword", inline = TRUE)
                              )
                          )
                          )
                 )