library(shiny)

source("tokenise-functions.R", local = TRUE)
source("eval-functions.R", local = TRUE)

load("training-dictionary.RData")
load("training-bigram-model.RData")
load("training-trigram-model.RData")
load("training-quadgram-model.RData")

shinyServer(function(input, output, session) {
  
  control <- reactiveValues()

  observeEvent(input$sentence, {
    tokens <- unlist(TokeniseText(input$sentence))
    tokens.length <- length(tokens)
    
    if(tokens.length > 2) {
      # Use the interpolated model
      hash.tokens <- ReplaceUnknownHashes(hash(
        tokens[(tokens.length - 2):tokens.length]))
      
      answers <- InterpolateModels(hash.tokens)
      
      control$answers[1:5] <- unlist(answers[1:5, "word"])
      control$prob[1:5] <- unlist(answers[1:5, "interpolated"])
    } else if(tokens.length == 0){
      # Clear the values if there is no text
      control$answers[1:5] <- ""
      control$prob[1:5] <- ""          
    } else {
      hash.tokens <- ReplaceUnknownHashes(hash(tokens))
      if(tokens.length == 2) {
        # Use the trigram model
        prediction <- unlist(trigram.model[
          word.1 == hash.tokens[2] & word.2 == hash.tokens[1], 
          3:12, with = FALSE])        
      } else {
        # Use the bigram model
        prediction <- unlist(bigram.model[
          word.1 == hash.tokens[1], 
          2:11, with = FALSE])         
      }
      control$answers <- DLookup(prediction[1:5])
      control$prob <- round(prediction[6:10] * 100)
      control$answers[is.na(control$answers)] <- ""
      control$prob[is.na(control$prob)] <- "" 
    }
  })
  
  observeEvent(input$answer1, {updateTextInput(session, "sentence", value = paste(input$sentence, control$answers[1])) })
  observeEvent(input$answer2, {updateTextInput(session, "sentence", value = paste(input$sentence, control$answers[2])) })
  observeEvent(input$answer3, {updateTextInput(session, "sentence", value = paste(input$sentence, control$answers[3])) })
  observeEvent(input$answer4, {updateTextInput(session, "sentence", value = paste(input$sentence, control$answers[4])) })
  observeEvent(input$answer5, {updateTextInput(session, "sentence", value = paste(input$sentence, control$answers[5])) })
  
  observeEvent(input$reset, {
    updateTextInput(session, "sentence", value = "")
    control$answers <- rep("", 5)
    control$prob <- rep("", 5)
  })
  

  # The javascript function only seems to need to be called once - probably
  # because the buttons are refreshed when the user clicks an answer
  output$button1 <- renderUI({list(actionButton("answer1", 
                                                label = control$answers[1]),
                                   tags$script("focusOnTextBox()")) })
  output$button2 <- renderUI({actionButton("answer2", 
                                           label = control$answers[2]) })
  output$button3 <- renderUI({actionButton("answer3", 
                                           label = control$answers[3]) })
  output$button4 <- renderUI({actionButton("answer4", 
                                           label = control$answers[4]) })
  output$button5 <- renderUI({actionButton("answer5", 
                                           label = control$answers[5]) })
  
  output$prob1 <- renderText({paste0(control$prob[1], "%") })
  output$prob2 <- renderText({paste0(control$prob[2], "%") })
  output$prob3 <- renderText({paste0(control$prob[3], "%") })
  output$prob4 <- renderText({paste0(control$prob[4], "%") })
  output$prob5 <- renderText({paste0(control$prob[5], "%") })

})
