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
        div(align = "center",
            column(2,
                   uiOutput("button1"),
                   textOutput("prob1")),
            column(2,
                   uiOutput("button2"),
                   textOutput("prob2")),
            column(2,
                   uiOutput("button3"),
                   textOutput("prob3")),
            column(2,
                   uiOutput("button4"),
                   textOutput("prob4")),
            column(2,
                   uiOutput("button5"),
                   textOutput("prob5"))
            )
        ),
      submitButton("Predict"),
      actionButton("reset", "Start again"),
      br(),
      textOutput("tokens"),
      br(),
      textOutput("answers"),
      br(),
      textOutput("clicked"),
      br(),
      a("Exploratory analysis of the dataset", href = "http://rpubs.com/chrislill/nextwordmilestone" ),
      br(),
      a("Source code on github", href = "https://github.com/chrislill/next-word" )
      )
    )
  )