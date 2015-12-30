library(dplyr)

source("next-word-functions.R")

# Load model
if(!exists("trigram.model")) {
  load(file = "models\\training-model.RData")
  trigram.model <- training.trigram.model
}

# Validation data
if(!exists("validation.trigrams")) {
  load("data\\validation-tokens.RData")
  validation.trigrams <- CountTrigrams(validation.tokens)
  save(validation.trigrams, file = "data\\validation-trigrams.RData")
  } else {
  load("data\\validation-trigrams.RData")
  }
v2.trigrams <- validation.trigrams[101:200, ] %>%
  group_by() %>%
  mutate(word1 = as.character(word1)) %>%
  mutate(word2 = as.character(word2)) %>%
  mutate(prediction = PredictWord(word1, word2))



    
