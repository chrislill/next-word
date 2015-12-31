library(dplyr)

source("next-word-functions.R")

# Load model ------------------------------------------------------------------
if(!exists("trigram.model")) {
  load(file = "models\\training-model.RData")
}

# Load validation data --------------------------------------------------------
if(!exists("validation.trigrams")) {
  if(!file.exists("data\\validation-trigrams.RData")) {
    load("data\\validation-tokens.RData")
    validation.trigrams <- CountTrigrams(validation.tokens)
    save(validation.trigrams, file = "data\\validation-trigrams.RData")
  }
  load("data\\validation-trigrams.RData")
} 

# Evaluate --------------------------------------------------------------------
val.results <- trigram.model[validation.trigrams[101:200, ]]
val.results[, accuracy:=(word3 == word3_1)]

# TODO: There is definitely a more elegant way to do this...
val.results[, first.5.accuracy:=(word3 == word3_1 | 
                                   word3 == word3_2 | 
                                   word3 == word3_3 | 
                                   word3 == word3_4 | 
                                   word3 == word3_5 )]
val.results[, perplexity:=(sum((word3 == word3_1) / pr_1,
                               (word3 == word3_2) / pr_2,
                               (word3 == word3_3) / pr_3,
                               (word3 == word3_4) / pr_4,
                               (word3 == word3_5) / pr_5,
                               na.rm = TRUE)), by=.(word1, word2, word3)]
val.eval <- val.results[, list(sum(count),
                               sum(accuracy * count, na.rm = TRUE),
                               sum(first.5.accuracy * count, na.rm = TRUE),
                               sum(probability * count))]
accuracy <- round(val.eval[[1, 2]] / val.eval[[1, 1]], 3)
first.5.accuracy <- round(val.eval[[1, 3]] / val.eval[[1, 1]], 3)
perplexity <- round(val.eval[[1, 4]] ^ (1 / val.eval[[1, 1]]), 3)

    
