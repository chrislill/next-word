library(shiny)

source("..\\tokenise-functions.R")

load("..\\data\\prediction.RData")
# load("..\\data\\training-dictionary.RData")
# load("..\\models\\training-trigram-model.RData")

shinyServer(function(input, output, session) {
  
  choice <- reactiveValues(word = NULL)
  answers <- unlist(prediction[1, 3:7, with = FALSE])
  probabilities <- unlist(round(prediction[1, 8:12, with = FALSE] * 100))
  
  observeEvent(input$answer1, { choice$word <- answers[1] })
  observeEvent(input$answer2, { choice$word <- answers[2] })
  observeEvent(input$answer3, { choice$word <- answers[3] })
  observeEvent(input$answer4, { choice$word <- answers[4] })
  observeEvent(input$answer5, { choice$word <- answers[5] })
  
    #     updateTextInput(session, "sentence", value = paste(input$sentence, "sea"))  
    
  output$tokens <- renderText({
    paste(unlist(TokeniseText(input$sentence)))
  })
  
  output$answers <- renderText({
    paste(prediction[,3:7, with = FALSE])
  })
  
  output$clicked <- renderText({
    choice$word
  })
  
  output$button1 <- renderUI({ actionButton("answer1", label = answers[1]) })
  output$button2 <- renderUI({ actionButton("answer2", label = answers[2]) })
  output$button3 <- renderUI({ actionButton("answer3", label = answers[3]) })
  output$button4 <- renderUI({ actionButton("answer4", label = answers[4]) })
  output$button5 <- renderUI({ actionButton("answer5", label = answers[5]) })
  
  output$prob1 <- renderText({ paste0(probabilities[1], "%") })
  output$prob2 <- renderText({ paste0(probabilities[2], "%") })
  output$prob3 <- renderText({ paste0(probabilities[3], "%") })
  output$prob4 <- renderText({ paste0(probabilities[4], "%") })
  output$prob5 <- renderText({ paste0(probabilities[5], "%") })

})
