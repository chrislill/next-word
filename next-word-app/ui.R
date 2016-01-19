library(shiny)

shinyUI(
  fluidPage(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                        href = "little-changes.css"),
              tags$script(src="helper.js")),
    titlePanel("Next Word Prediction"),
    mainPanel(
      p("Type a word to get started..."),
      textInput("sentence", NULL, width = "100%"),

      fluidRow(
        div(align = "center",
            column(2,
                   uiOutput("button1"),
                   textOutput("prob1"),
                   offset = 1),
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
      
      div(style = "height: 30px;"),
      
      checkboxInput("toggle", "Show probability table"),
      
      conditionalPanel("input.toggle == 1", 
                       tableOutput("probtable")
                       ),
      
      actionButton("reset", "Start again", 
                   style = "color: #ffffff; background-color: #777777; border-color: #777777;"),
      div(style = "height: 30px;"),
      
      "Initial ",
      a("exploratory analysis", 
        href = "http://rpubs.com/chrislill/nextwordmilestone" ),
      br(),
      "Source code on ",
      a("github", 
        href = "https://github.com/chrislill/next-word"),
      tags$script("focusOnTextBox()")
      )
    )
  )