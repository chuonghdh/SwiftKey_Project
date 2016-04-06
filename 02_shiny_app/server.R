
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tm)      # Text Minning Library    
library(stringr) # Support to basic file summary
library(RWeka)   # Support to n-gram processes
library(stringi)
library(slam)
library(dplyr)
library(data.table)

load("prgData.RData",verbose = TRUE)

inputText <<- NULL
resultText <<- NULL
triGramResults <<- NULL
binGramResults <<- NULL
uniGramResults <<- NULL
processTime <<- NULL

predictSentence <-function(sentence, dataset){
  inputText <<- sentence
  triGramResults <<- NULL
  binGramResults <<- NULL
  uniGramResults <<- NULL
  
  start.time <- Sys.time()
  
  corST <- Corpus(VectorSource(str_trim(inputText)), readerControl=list(reader=readPlain, language="en_US", load=TRUE))
  corST <- cleanData(corST,cleanType = "test", dataset)
  if(str_trim(corST[[1]]$content)=="") result <- "please_feed_more_word" 
  else {
    x<- str_trim(corST[[1]]$content)
    t <- tail(strsplit(x, " ")[[1]],3)
    n <- length(t)
    triPred = NULL; binPred = NULL; uniPred = NULL
  
    if(n >= 3) triPred <- predictWord(input=paste(t[n-2], t[n-1], t[n]), dataset)[order(freq,decreasing = TRUE)]
    if(n >= 2) binPred <- predictWord(input=paste(t[n-1], t[n]), dataset)[order(freq,decreasing = TRUE)]
    if(n >= 1) uniPred <- predictWord(input=paste(t[n]), dataset)[order(freq,decreasing = TRUE)]
  
    result <- c(triPred$predict[1],binPred$predict[1],uniPred$predict[1])
    triGramResults <<- triPred
    binGramResults <<- binPred
    uniGramResults <<- uniPred
    processTime <<- (Sys.time() - start.time)
    result <- result[!is.na(result)][1]
  }
  result
}

##--------------------
#predictWord <- function(input,readFromFolder, readPrefix){
#  fstChar <- substr(input, 1, 1)
#  df <- data.table(read.table(file=paste0(readFromFolder,"/",readPrefix,fstChar, ".txt"),header = TRUE))
#  df$inputterm <- as.character(df$inputterm)
#  df$predict <- as.character(df$predict) 
#  df$freq <- as.numeric(df$freq)
#  df[inputterm==input]
#}
predictWord <- function(input, dataset) {
  ds <- dataset
  fstChar <- substr(input, 1, 1)
  n <- length(strsplit(input,split = " ")[[1]])
  if (dataset=="withSW") {
    if(n==3) df <- triFinalALL[[fstChar]][inputterm==input] #ALL means include Stop Words
    if(n==2) df <- binFinalALL[[fstChar]][inputterm==input]
    if(n==1) df <- uniFinalALL[[fstChar]][inputterm==input]
  } else if (dataset=="withoutSW") {
    if(n==3) df <- triFinalRSW[[fstChar]][inputterm==input] #RSW means Remove Stop Words
    if(n==2) df <- binFinalRSW[[fstChar]][inputterm==input]
    if(n==1) df <- uniFinalRSW[[fstChar]][inputterm==input]
  } else df <- NULL
  df
}

##--------------------
cleanData <- function(x, cleanType = "train", dataset){
  #remove non Unicode characters
  x <- tm_map(x, content_transformer(function(x) iconv(enc2utf8(x), sub = "")))
  #remove non ASCII characters
  x <- tm_map(x, content_transformer(function(x) iconv(x,from = "latin1", to="ASCII", sub="")))
  #low case and remove Stopwords
  x <- tm_map(x, content_transformer(tolower))
  if(dataset == "withoutSW") x <- tm_map(x, removeWords, stopwords("english"))
  #remove special character but not .?;"!() those will be a delimit signal when devided into ngram
  x <- tm_map(x,content_transformer(function(x) stri_replace_all_regex(x, "[^\\p{L}\\s[.?;!()\"]]+","")))
  #convert .?;!() character to " . " delimiter
  #x <- tm_map(x,content_transformer(function(x) stri_replace_all_regex(x, "[[.,?;!()]]+"," . ")))
  
  x <- tm_map(x, removeNumbers)
  #close when train open when run
  if (cleanType =="test") x <- tm_map(x, content_transformer(removePunctuation))
  #x <- tm_map(x, removeWords, profWords) # Remove profane words
  
  x <- tm_map(x, stripWhitespace)
  x
}

getValue <- function(value, inputTextTrigger, inputSelectDataset){
  x<-inputTextTrigger
  y<-inputSelectDataset
  returnResult <- NULL
  if (value == "inputText") returnResult<-inputText else
  if (value == "processTime") returnResult<-round(processTime,4) else
  if (value == "triGramResults") returnResult<-head(triGramResults,5) else
  if (value == "binGramResults") returnResult<-head(binGramResults,5) else
  if (value == "uniGramResults") returnResult<-head(uniGramResults,5) 
  returnResult
}

shinyServer(function(input, output) {
  
  output$oResultText <- reactive({predictSentence(input$iTextInput, input$iSelectDataset)})
  output$oInputText <- reactive({getValue("inputText",input$iTextInput, input$iSelectDataset)})
  output$oProcessTime <- reactive({getValue("processTime",input$iTextInput, input$iSelectDataset)})
  output$oTriGramResults <- renderPrint({getValue("triGramResults",input$iTextInput, input$iSelectDataset)})
  output$oBinGramResults <- renderPrint({getValue("binGramResults",input$iTextInput, input$iSelectDataset)})
  output$oUniGramResults <- renderPrint({getValue("uniGramResults",input$iTextInput, input$iSelectDataset)})
})
