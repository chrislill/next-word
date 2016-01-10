library(shiny)

source("..\\tokenise-functions.R")

load("..\\data\\prediction.RData")
# load("..\\data\\training-dictionary.RData")
# load("..\\models\\training-trigram-model.RData")

shinyServer(function(input, output, session) {
  
  observe({
    updateTextInput(session, "sentence", value = paste(input$sentence, "sea"))  
    
  })
  
  # output$renderText({})
  
})

# http://stackoverflow.com/questions/27326929/how-to-update-button-labels-in-r-shiny
# http://stackoverflow.com/questions/33777991/update-label-of-actionbutton-in-shiny
# http://stackoverflow.com/questions/29376031/capture-the-label-of-an-actionbutton-once-it-is-clicked