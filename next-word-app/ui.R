library(shiny)

shinyUI(
  fluidPage(
    # tags$head(tags$style("input-text {display:inline;}")),
    titlePanel("Next Word Prediction"),
    mainPanel(
      p("Type a word, then press <Enter> to predict the next word"),
      # Apply style so that display:inline
      textInput("sentence", NULL, value = "and", width = "800px"),
      fluidRow(
        column(2,
               actionLink("answer1", "answer 1"),
               p("30%")),
        column(2,
               actionLink("answer2", "answer 2"),
               p("20%")),
        column(2,
               actionLink("answer3", "answer 3"),
               p("10%")),
        column(2,
               actionLink("answer4", "answer 4"),
               p("5%")),
        column(2,
               actionLink("answer5", "answer 5"),
               p("5%"))
        ),
      submitButton("Predict"),
      actionButton("reset", "Start again"),
      br(),
      a("Exploratory analysis of the dataset", href = "http://rpubs.com/chrislill/nextwordmilestone" ),
      br(),
      a("Source code on github", href = "https://github.com/chrislill/next-word" )
      )
    )
  )