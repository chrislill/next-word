library(shiny)

shinyUI(
  fluidPage(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                        href = "little-changes.css"),
              tags$script(src="helper.js")),
    titlePanel("Next Word Prediction"),
    mainPanel(
      p("Type a phrase, then click a suggestion..."),
      textInput("sentence", NULL, width = "100%"),

      fluidRow(
        div(align = "center", 
            column(2,
                   uiOutput("button1"),
                   textOutput("prob1"),
                   class = "col-sm-2 col-xs-4 col-sm-offset-1"),
            column(2,
                   uiOutput("button2"),
                   textOutput("prob2"),
                   class = "col-sm-2 col-xs-4"),
            column(2,
                   uiOutput("button3"),
                   textOutput("prob3"),
                   class = "col-sm-2 col-xs-4"),
            column(2,
                   uiOutput("button4"),
                   textOutput("prob4"),
                   class = "col-sm-2 col-xs-4"),
            column(2,
                   uiOutput("button5"),
                   textOutput("prob5"),
                   class = "col-sm-2 col-xs-4")
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
      
      "Capstone project ",
      a("final report", href = "http://rpubs.com/chrislill/next-word"),
      br(),

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