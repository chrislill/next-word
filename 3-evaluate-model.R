source("next-word-functions.R")

if(!exists("validation.trigrams")) {
  load("data\\validation-tokens.RData")
  validation.trigrams <- CountTrigrams(validation.tokens)
  save(validation.trigrams, file = "data\\validation-trigrams.RData")
  } else {
  load("data\\validation-trigrams.RData")
  }

if(!exists("trigram.model")) {
  load(file = "models\\training-model.RData")
  trigram.model <- training.trigram.model
}

v2.trigrams <- validation.trigrams[101:200, ]
v2.trigram.char <- sapply(v2.trigrams[, 1:2], as.character)


v2.trigrams$prediction <- sapply(v2.trigram.char, PredictWord)
    
