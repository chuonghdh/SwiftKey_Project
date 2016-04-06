
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("SwiftKey Word Prediction Application"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h4("Text Input for Prediction:"),
      tags$textarea(id="iTextInput", rows=4, cols=40, "thanks for your review"),
      radioButtons("iSelectDataset", 
                  label = "Select Dataset:",
                  choices = list("Removed stopwords dataset" = "withoutSW", 
                                  "Having stopwords dataset" = "withSW"), 
                  selected = "Having stopwords dataset"
                  ),
      hr(),
      helpText(strong("Instruction:"),br(), 
               em(" - This is a realtime running program",style = "font-size:10pt"),br(), 
               em(" - Text area for provide input words",style = "font-size:10pt"),br(),
               em(" - Radio button for select dataset (with/without stop words)",style = "font-size:10pt")),
      helpText(strong("Program Features:"),br(), 
               em(" - Used 100% SwiftKey US datasets",style = "font-size:10pt"),br(), 
               em(" - Reduce size of trained dataset by:",style = "font-size:10pt"),br(),
               em(" -- Ignore ngram terms with freq = 1",style = "font-size:10pt"),br(),
               em(" -- Keep top 5 predict-terms by frequency ranking",style = "font-size:10pt")),
      helpText(strong("Slide Deck:"), 
               a("presentation",href = "http://rpubs.com/chuonghdh/DSCapstoneProject_pitch_deck"))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      br(),
      h4("Prediction Result:",style = "background:lightgrey"),
      em(strong("Input text and predict term")),
      div(textOutput("oInputText",inline = TRUE),em(strong(textOutput("oResultText",inline = TRUE)),style = "color:white"),style = "background:orange"),
      
      fluidRow(
        column(5, offset=-1,
               em(strong("Total Process Time:"))
        ),
        column(1, offset = -1,
               em(strong(textOutput("oProcessTime")),style = "color:orange")
        ),
        column(1, offset= -1,
               em(strong("Seconds"))
        )
      ),
      
      h4("Reference Results:",style = "background:lightgrey"),
      helpText("Note: The Reference Reults will not catch server response when network slow. You can blank the textbox to correct local catche and continue input",style = "font-size:8pt"),
      em(strong("Top 5 trigram model results")),
      verbatimTextOutput("oTriGramResults"),
      em(strong("Top 5 bingram model results")),
      verbatimTextOutput("oBinGramResults"),
      em(strong("Top 5 unigram model results")),
      verbatimTextOutput("oUniGramResults")
    )
  )
))
